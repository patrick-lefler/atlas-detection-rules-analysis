# ATLAS calibration functions: error rates, the equal-variance Gaussian model,
# the two recommendation rules, bootstrap, projections and charts.
#
# Conventions: a rule alerts when its score exceeds threshold t. Higher t = fewer alerts.
# FRR = miss rate = FN / (TP + FN). FAR = false-positive rate = FP / (FP + TN).

# ── Error rates (Mattei 2025, §8.5.1–8.5.2) ─────────────────────────────────

compute_frr <- function(tp, fn) {
  total <- tp + fn
  if (any(total == 0)) {
    cli_abort(c("Cannot compute FRR.", "x" = "TP + FN = 0: no actual threats in dataset."))
  }
  fn / total
}

compute_far <- function(fp, tn) {
  total <- fp + tn
  if (any(total == 0)) {
    cli_abort(c("Cannot compute FAR.", "x" = "FP + TN = 0: no actual benign events in dataset."))
  }
  fp / total
}

wilson_ci <- function(x, n, z = 1.96) {
  p <- x / n
  d <- 1 + z^2 / n
  c <- (p + z^2 / (2 * n)) / d
  h <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / d
  c(lower = c - h, upper = c + h)
}


# ── Equal-variance two-Gaussian model ────────────────────────────────────────
# Benign scores ~ N(0, 1); threat scores ~ N(d', 1). The single observed (FRR, FAR)
# point fixes both the current threshold and d', so the curve passes through it by
# construction: it is an assumption, not a fit.

clamp01 <- function(x, eps = 1e-6) pmin(pmax(x, eps), 1 - eps)

fit_gaussian_model <- function(frr, far) {
  t_current <- qnorm(1 - clamp01(far))
  d_prime   <- t_current - qnorm(clamp01(frr))
  list(
    d_prime   = d_prime,
    t_current = t_current,
    t_eer     = d_prime / 2,
    eer       = pnorm(-d_prime / 2)
  )
}

model_frr <- function(t, d_prime) pnorm(t - d_prime)
model_far <- function(t) 1 - pnorm(t)

# Threshold minimising cost_ratio * p * FRR(t) + (1 - p) * FAR(t),
# i.e. where the likelihood ratio equals (1 - p) / (cost_ratio * p).
cost_optimal_threshold <- function(d_prime, p, cost_ratio) {
  d_prime <- pmax(d_prime, 1e-3)
  d_prime / 2 + log((1 - p) / (cost_ratio * p)) / d_prime
}

# Expected cost over the review window, in units of one false alarm.
expected_cost <- function(frr, far, n_threat, n_benign, cost_ratio) {
  cost_ratio * frr * n_threat + far * n_benign
}


# ── Recommendation rules ─────────────────────────────────────────────────────

# EER-based (original ATLAS reading): move towards FRR = FAR.
classify_eer <- function(frr, far, tolerance = 0.02) {
  delta <- frr - far
  case_when(
    abs(delta) <= tolerance ~ "HOLD",
    delta > 0               ~ "LOOSEN",
    .default                = "TIGHTEN"
  )
}

recommend_eer <- function(frr, far, tolerance = 0.02) {
  action <- classify_eer(frr, far, tolerance)
  gap    <- number(abs(frr - far) * 100, accuracy = 0.1)
  reason <- switch(
    action,
    HOLD = str_glue(
      "FRR ({pct(frr)}) and FAR ({pct(far)}) are within ",
      "{number(tolerance * 100, accuracy = 1)} percentage points of each other."
    ),
    LOOSEN = str_glue(
      "FRR ({pct(frr)}) exceeds FAR ({pct(far)}) by {gap} percentage points: ",
      "lower the threshold towards the EER."
    ),
    TIGHTEN = str_glue(
      "FAR ({pct(far)}) exceeds FRR ({pct(frr)}) by {gap} percentage points: ",
      "raise the threshold towards the EER."
    )
  )
  list(action = action, reason = as.character(reason))
}

# Cost-weighted: compare the current threshold with the cost-optimal one.
classify_cost <- function(t_current, t_opt, d_prime, cost_reduction,
                          delta_t = 0.1, d_prime_max = 2, min_reduction = 0.10) {
  case_when(
    d_prime < d_prime_max & cost_reduction < min_reduction ~ "IMPROVE SIGNAL",
    t_current < t_opt - delta_t                            ~ "TIGHTEN",
    t_current > t_opt + delta_t                            ~ "LOOSEN",
    .default                                               = "HOLD"
  )
}

recommend_cost <- function(t_current, t_opt, d_prime, cost_reduction, frr_opt, far_opt,
                           delta_t = 0.1, d_prime_max = 2, min_reduction = 0.10) {
  action <- classify_cost(t_current, t_opt, d_prime, cost_reduction,
                          delta_t, d_prime_max, min_reduction)
  target <- str_glue("FRR {pct(frr_opt)}, FAR {pct(far_opt)}")
  reason <- switch(
    action,
    `IMPROVE SIGNAL` = str_glue(
      "Separability is low (d′ = {number(d_prime, accuracy = 0.01)}) and moving to the ",
      "cost-optimal threshold ({target}) cuts expected cost by only {pct(cost_reduction)}. ",
      "Better detection features are needed before a threshold change pays off."
    ),
    TIGHTEN = str_glue(
      "Raise the threshold to the cost-optimal point ({target}); ",
      "expected cost falls by {pct(cost_reduction)}."
    ),
    LOOSEN = str_glue(
      "Lower the threshold to the cost-optimal point ({target}); ",
      "expected cost falls by {pct(cost_reduction)}."
    ),
    HOLD = str_glue(
      "The current threshold is within {delta_t} model units of the cost-optimal point ({target})."
    )
  )
  list(action = action, reason = as.character(reason))
}


# ── Full evaluation of one rule ──────────────────────────────────────────────

evaluate_rule <- function(rule_name, tp, fp, fn, tn, cost_ratio, params) {
  n_threat <- tp + fn
  n_benign <- fp + tn
  p        <- n_threat / (n_threat + n_benign)
  frr      <- compute_frr(tp, fn)
  far      <- compute_far(fp, tn)
  z        <- qnorm(1 - (1 - params$ci_level) / 2)
  frr_ci   <- wilson_ci(fn, n_threat, z)
  far_ci   <- wilson_ci(fp, n_benign, z)

  m       <- fit_gaussian_model(frr, far)
  t_opt   <- cost_optimal_threshold(m$d_prime, p, cost_ratio)
  frr_opt <- model_frr(t_opt, m$d_prime)
  far_opt <- model_far(t_opt)

  cost_current   <- expected_cost(frr, far, n_threat, n_benign, cost_ratio)
  cost_opt       <- expected_cost(frr_opt, far_opt, n_threat, n_benign, cost_ratio)
  cost_reduction <- 1 - cost_opt / cost_current

  rec_eer  <- recommend_eer(frr, far, params$eer_tolerance)
  rec_cost <- recommend_cost(
    m$t_current, t_opt, m$d_prime, cost_reduction, frr_opt, far_opt,
    params$delta_t, params$improve_signal_d_prime, params$improve_signal_min_cost_reduction
  )

  tibble(
    rule = rule_name, tp, fp, fn, tn, n_threat, n_benign,
    prevalence = p, precision = tp / (tp + fp),
    frr, frr_lo = frr_ci[["lower"]], frr_hi = frr_ci[["upper"]],
    far, far_lo = far_ci[["lower"]], far_hi = far_ci[["upper"]],
    mean_frr_far = (frr + far) / 2,
    d_prime = m$d_prime, t_current = m$t_current, t_eer = m$t_eer, eer = m$eer,
    cost_ratio, eer_implied_cost_ratio = (1 - p) / p,
    t_opt, frr_opt, far_opt, cost_current, cost_opt, cost_reduction,
    eer_action = rec_eer$action, eer_reason = rec_eer$reason,
    cost_action = rec_cost$action, cost_reason = rec_cost$reason
  )
}


# ── Bootstrap: does the recommendation survive sampling noise? ──────────────
# Parametric resample of the counts: fn* ~ Binom(TP + FN, FRR), fp* ~ Binom(FP + TN, FAR).
# Seeded per rule so a rule's result does not depend on the order of the rules.

bootstrap_rule <- function(rule_name, tp, fp, fn, tn, cost_ratio, params) {
  set.seed(params$seed)
  n_threat <- tp + fn
  n_benign <- fp + tn
  p        <- n_threat / (n_threat + n_benign)
  draws    <- params$bootstrap_draws

  frr_b <- rbinom(draws, n_threat, fn / n_threat) / n_threat
  far_b <- rbinom(draws, n_benign, fp / n_benign) / n_benign

  m       <- fit_gaussian_model(frr_b, far_b)
  t_opt   <- cost_optimal_threshold(m$d_prime, p, cost_ratio)
  cost_c  <- expected_cost(frr_b, far_b, n_threat, n_benign, cost_ratio)
  cost_o  <- expected_cost(model_frr(t_opt, m$d_prime), model_far(t_opt), n_threat, n_benign, cost_ratio)

  bind_rows(
    tibble(method = "EER-based", action = classify_eer(frr_b, far_b, params$eer_tolerance)),
    tibble(
      method = "Cost-weighted",
      action = classify_cost(
        m$t_current, t_opt, m$d_prime, 1 - cost_o / cost_c, params$delta_t,
        params$improve_signal_d_prime, params$improve_signal_min_cost_reduction
      )
    )
  ) |>
    count(method, action) |>
    mutate(rule = rule_name, share = n / draws, .before = 1) |>
    select(rule, method, action, share)
}


# ── Projected workload at a new threshold ────────────────────────────────────

project_at <- function(t_new, d_prime, n_threat, n_benign, tp, fp, minutes_per_alert) {
  tp_new <- (1 - model_frr(t_new, d_prime)) * n_threat
  fp_new <- model_far(t_new) * n_benign
  tibble(
    tp_new, fp_new,
    delta_tp      = tp_new - tp,
    delta_fp      = fp_new - fp,
    precision_new = tp_new / (tp_new + fp_new),
    delta_hours   = (fp_new - fp) * minutes_per_alert / 60
  )
}

precision_at_prevalence <- function(frr, far, p) {
  (1 - frr) * p / ((1 - frr) * p + far * (1 - p))
}


# ── Curves and charts ────────────────────────────────────────────────────────

build_frr_far_curve <- function(d_prime, n_points = 300) {
  thresholds <- seq(min(-3, d_prime - 3), max(3, d_prime + 3), length.out = n_points)
  tibble(
    threshold = thresholds,
    `FRR (missed threats)` = model_frr(thresholds, d_prime),
    `FAR (false alarms)`   = model_far(thresholds)
  ) |>
    pivot_longer(-threshold, names_to = "series", values_to = "rate")
}

# Empirical curve from measured counts at several real thresholds.
# Input: one row per threshold, columns threshold, tp, fp, fn, tn (threshold in the
# rule's own units, e.g. failed attempts per hour). Use this whenever replayed logs or
# score data exist; fall back to the Gaussian model only for a single observed row.
build_empirical_curve <- function(df) {
  stopifnot(all(c("threshold", "tp", "fp", "fn", "tn") %in% names(df)))
  if (nrow(df) < 2) {
    cli_abort(c("Need at least two thresholds for an empirical curve.",
                "i" = "With one row, use {.fn fit_gaussian_model} instead."))
  }
  df |>
    arrange(threshold) |>
    mutate(frr = compute_frr(tp, fn), far = compute_far(fp, tn))
}

plot_rule <- function(r) {
  curve   <- build_frr_far_curve(r$d_prime)
  y_label <- 1.04
  markers <- tibble(
    x     = c(r$t_current, r$t_opt),
    label = c("Current", "Cost-optimal"),
    hjust = if_else(c(r$t_current, r$t_opt) < c(r$t_opt, r$t_current), 1.05, -0.05)
  )

  ggplot(curve, aes(threshold, rate, colour = series)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = r$t_current, colour = COLOURS[[r$cost_action]],
               linetype = "dotted", linewidth = 1) +
    geom_vline(xintercept = r$t_opt, colour = BRAND_PRIMARY,
               linetype = "longdash", linewidth = 0.6) +
    geom_text(data = markers, aes(x = x, y = y_label, label = label, hjust = hjust),
              inherit.aes = FALSE, size = 3.6, colour = BRAND_PRIMARY) +
    annotate("point", x = r$t_current, y = r$frr, colour = BRAND_ACCENT, size = 3) +
    annotate("point", x = r$t_current, y = r$far, colour = BRAND_HIGHLIGHT, size = 3) +
    annotate("point", x = r$t_eer, y = r$eer, colour = BRAND_GREY, size = 3.5, shape = 21,
             fill = "white", stroke = 1.2) +
    annotate("segment", x = r$t_eer, xend = r$t_eer, y = r$eer + 0.03, yend = r$eer + 0.19,
             colour = BRAND_GREY, linewidth = 0.4) +
    annotate("label", x = r$t_eer, y = r$eer + 0.22, label = str_glue("EER = {pct(r$eer)}"),
             colour = BRAND_GREY, size = 3.6, border.colour = NA) +
    scale_colour_manual(
      name = NULL,
      values = c(`FRR (missed threats)` = BRAND_ACCENT, `FAR (false alarms)` = BRAND_HIGHLIGHT)
    ) +
    scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1, 0.25)) +
    coord_cartesian(ylim = c(0, 1.08), clip = "off") +
    labs(
      title = str_glue("{r$rule}: FRR {pct(r$frr)}, FAR {pct(r$far)}, EER {pct(r$eer)}"),
      x = "Threshold (model units: benign-score z)",
      y = "Error rate"
    ) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
}

plot_empirical_curve <- function(curve, x_label) {
  curve |>
    select(threshold, `FRR (missed threats)` = frr, `FAR (false alarms)` = far) |>
    pivot_longer(-threshold, names_to = "series", values_to = "rate") |>
    ggplot(aes(threshold, rate, colour = series)) +
    geom_step(linewidth = 1, direction = "hv") +
    geom_point(size = 2.5) +
    scale_colour_manual(
      name = NULL,
      values = c(`FRR (missed threats)` = BRAND_ACCENT, `FAR (false alarms)` = BRAND_HIGHLIGHT)
    ) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(x = x_label, y = "Error rate") +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom")
}
