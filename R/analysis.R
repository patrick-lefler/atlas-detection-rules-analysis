# Builds every result object the pages use, from params.yaml.
# Requires R/setup.R and R/atlas.R to be sourced first.

params <- yaml::read_yaml("params.yaml")

rules <- params$rules |>
  map(as_tibble) |>
  list_rbind()

results <- rules |>
  select(rule_name, tp, fp, fn, tn, cost_ratio) |>
  pmap(evaluate_rule, params = params) |>
  list_rbind()

boot <- rules |>
  select(rule_name, tp, fp, fn, tn, cost_ratio) |>
  pmap(bootstrap_rule, params = params) |>
  list_rbind()

confidence_of <- function(rule, method, action) {
  shares <- boot |> filter(rule == !!rule, method == !!method, action == !!action)
  if (nrow(shares) == 0) 0 else shares$share
}

with_confidence <- function(action, conf) {
  if_else(conf < params$confidence_threshold, str_glue("{action} (low confidence)"), action)
}

results <- results |>
  mutate(
    eer_conf   = pmap_dbl(list(rule, "EER-based", eer_action), confidence_of),
    cost_conf  = pmap_dbl(list(rule, "Cost-weighted", cost_action), confidence_of),
    eer_label  = with_confidence(eer_action, eer_conf),
    cost_label = with_confidence(cost_action, cost_conf)
  )

# Results by rule name, for inline numbers: res[["Lateral Movement"]]$frr
res <- split(results, results$rule)

projection <- results |>
  reframe(
    target = c("EER", "Cost-optimal"),
    t_new  = c(t_eer, t_opt),
    .by    = c(rule, d_prime, n_threat, n_benign, tp, fp, precision)
  ) |>
  mutate(
    project_at(t_new, d_prime, n_threat, n_benign, tp, fp, params$analyst_minutes_per_alert),
    # Remove floating-point residue (e.g. -1e-14) so a rule already at its target shows 0
    across(c(delta_tp, delta_fp, delta_hours), \(x) if_else(abs(x) < 1e-9, 0, x))
  )

proj <- split(projection, ~ rule + target)

prevalence_tbl <- results |>
  select(rule, frr, far) |>
  cross_join(tibble(p = params$prevalence_grid)) |>
  mutate(precision = precision_at_prevalence(frr, far, p))
