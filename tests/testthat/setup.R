root <- testthat::test_path("..", "..")
suppressPackageStartupMessages({
  source(file.path(root, "R", "setup.R"))
  source(file.path(root, "R", "atlas.R"))
})
test_params <- yaml::read_yaml(file.path(root, "params.yaml"))
