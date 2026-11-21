# ATLAS Threshold Detection Rules Analysis
> Detection-rule threshold calibration with miss rates, false-positive rates, EER and stated costs: *Data-Driven Cybersecurity* (Mattei, Manning 2025), Ch. 8.4–8.5

Author: Patrick Lefler <br>
Published: 2026-07-16 <br>
Rendered: https://patrick-lefler.github.io/rQuarto_ATLAS_detection_rules_analysis_2026-07-16/

## Project Introduction
> ATLAS-based threshold calibration for SOC detection rules in R/Quarto, framing alert tuning as a documented, cost-weighted risk decision for cybersecurity leaders.

ATLAS here is the Alert Threshold Lifecycle Assessment System (Mattei, Ch. 8.4), not MITRE ATLAS.

## Overview
This site applies ATLAS to three detection rules: Brute Force Login, Lateral Movement and Data Exfiltration. For each rule it computes the miss rate (FRR), the false-positive rate (FAR) and precision, each with Wilson confidence intervals. It then uses an equal-variance Gaussian model anchored to the observed point to derive the EER and d′ (separability). Two decision rules recommend TIGHTEN, LOOSEN, HOLD or IMPROVE SIGNAL: the original EER-based rule, and a cost-weighted rule driven by a stated per-rule ratio of the cost of a miss to the cost of a false alarm. Each recommendation is checked against 10,000 resamples of the counts. The data are illustrative, constructed for teaching.

## Tech Stack
* **Language:** R (environment pinned with `renv`)
* **Framework:** [Quarto](https://quarto.org/) website, rendered to `docs/`
* **Primary Libraries:** tidyverse (dplyr, ggplot2, purrr), kableExtra, scales, cli, yaml, testthat, gt, sessioninfo

## Repository Structure
```
├── index.qmd            # Overview: introduction, headline results
├── methodology.qmd      # Definitions, calibration model, decision rules
├── rules.qmd            # Inputs, calibration charts, per-rule discussion, workload projection
├── robustness.qmd       # Summary tables, resampling check, prevalence sensitivity
├── leadership.qmd       # Why threshold calibration matters to leaders
├── reproducibility.qmd  # Rebuild steps, parameters, tests, known limitations
├── params.yaml          # Rule counts and every assumption (labelled)
├── R/                   # setup.R, atlas.R (functions), analysis.R (results), repro_env.R
├── tests/testthat/      # Unit tests for the core functions
├── renv.lock            # Pinned package versions
└── docs/                # Rendered site
```

To rebuild: `Rscript -e 'renv::restore()'`, then `quarto render`.

## Key Findings (illustrative data)
- **Lateral Movement: LOOSEN.** Misses 45.0% of threats (95% CI 35.6–54.8%) against a 2.2% FAR. It is the only rule whose recommendation is robust under both decision rules. Moving to the EER adds about 31 detections and 109 false alarms per window.
- **Brute Force Login: IMPROVE SIGNAL (low confidence).** The rule already sits close to its EER; the EER-based TIGHTEN cuts false positives by only about 5% and survives just 53% of resamples. Low separability (d′ = 1.61) means better features, not a new threshold.
- **Data Exfiltration: LOOSEN (cost-weighted).** The rule sits at its EER, but the EER-based HOLD survives only 47% of resamples and implicitly prices a miss at 9 false alarms. At the stated 50:1 cost ratio, the cost-optimal threshold is lower.

## License
This project is licensed under the MIT License. See the LICENSE file for details.

## Contact
Patrick Lefler [https://www.linkedin.com/in/patricklefler/] | [patrick-lefler.github.io] | [https://substack.com/@pflefler]
