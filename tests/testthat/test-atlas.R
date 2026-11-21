test_that("error rates follow the detection-engineering definitions", {
  expect_equal(compute_frr(tp = 80, fn = 20), 0.20)
  expect_equal(compute_far(fp = 200, tn = 700), 200 / 900)
  expect_error(compute_frr(0, 0), "no actual threats")
  expect_error(compute_far(0, 0), "no actual benign")
})

test_that("Wilson intervals match published values", {
  ci <- wilson_ci(20, 100)
  expect_equal(unname(round(ci, 3)), c(0.133, 0.289))
  ci <- wilson_ci(45, 100)
  expect_equal(unname(round(ci, 3)), c(0.356, 0.548))
})

test_that("Gaussian model passes through the observed point and finds the crossover", {
  m <- fit_gaussian_model(frr = 0.45, far = 20 / 900)
  expect_equal(model_frr(m$t_current, m$d_prime), 0.45)
  expect_equal(model_far(m$t_current), 20 / 900)
  expect_equal(model_frr(m$t_eer, m$d_prime), model_far(m$t_eer))
  expect_equal(m$eer, model_far(m$t_eer))
  expect_equal(round(m$eer, 3), 0.143)
})

test_that("cost-optimal threshold minimises expected cost", {
  d <- 2.1; p <- 0.1; k <- 25
  cost <- function(t) k * p * model_frr(t, d) + (1 - p) * model_far(t)
  numeric_opt <- optimize(cost, c(-5, 8))$minimum
  expect_equal(cost_optimal_threshold(d, p, k), numeric_opt, tolerance = 1e-4)
  # EER is the cost-optimal point when the cost ratio equals (1 - p) / p
  expect_equal(cost_optimal_threshold(d, p, (1 - p) / p), d / 2)
})

test_that("EER-based rule uses a percentage-point band", {
  expect_equal(classify_eer(0.20, 0.222), "TIGHTEN")
  expect_equal(classify_eer(0.45, 0.022), "LOOSEN")
  expect_equal(classify_eer(0.10, 0.11), "HOLD")
  expect_match(recommend_eer(0.10, 0.10)$reason, "2 percentage points")
})

test_that("cost-weighted rule responds to the cost ratio", {
  low  <- evaluate_rule("x", 90, 90, 10, 810, cost_ratio = 2,  params = test_params)
  high <- evaluate_rule("x", 90, 90, 10, 810, cost_ratio = 50, params = test_params)
  expect_equal(low$cost_action, "TIGHTEN")
  expect_equal(high$cost_action, "LOOSEN")
  eer_equiv <- evaluate_rule("x", 90, 90, 10, 810, cost_ratio = 9, params = test_params)
  expect_equal(eer_equiv$cost_action, "HOLD")
})

test_that("IMPROVE SIGNAL fires only for weak separability and small gains", {
  expect_equal(classify_cost(0.8, 1.2, d_prime = 1.6, cost_reduction = 0.05), "IMPROVE SIGNAL")
  expect_equal(classify_cost(0.8, 1.2, d_prime = 2.5, cost_reduction = 0.05), "TIGHTEN")
  expect_equal(classify_cost(0.8, 1.2, d_prime = 1.6, cost_reduction = 0.30), "TIGHTEN")
})

test_that("bootstrap is deterministic and its shares sum to one", {
  p <- modifyList(test_params, list(bootstrap_draws = 2000))
  a <- bootstrap_rule("x", 80, 200, 20, 700, 5, p)
  b <- bootstrap_rule("x", 80, 200, 20, 700, 5, p)
  expect_identical(a, b)
  expect_equal(sum(a$share[a$method == "EER-based"]), 1)
  expect_equal(sum(a$share[a$method == "Cost-weighted"]), 1)
})

test_that("projection reproduces the current counts at the current threshold", {
  m <- fit_gaussian_model(0.45, 20 / 900)
  out <- project_at(m$t_current, m$d_prime, 100, 900, tp = 55, fp = 20, minutes_per_alert = 15)
  expect_equal(out$delta_tp, 0, tolerance = 1e-8)
  expect_equal(out$delta_fp, 0, tolerance = 1e-8)
})

test_that("precision at prevalence matches the observed precision at the observed prevalence", {
  expect_equal(precision_at_prevalence(0.20, 200 / 900, 0.1), 80 / 280)
})

test_that("empirical curve needs several thresholds and computes rates", {
  df <- tibble(threshold = c(5, 3), tp = c(70, 90), fp = c(100, 300), fn = c(30, 10), tn = c(800, 600))
  out <- build_empirical_curve(df)
  expect_equal(out$threshold, c(3, 5))
  expect_equal(out$frr, c(0.1, 0.3))
  expect_error(build_empirical_curve(df[1, ]), "at least two")
})
