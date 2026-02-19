# Day 2 API

This service mirrors the Day 1 API scaffolding and serves Day 2 treatment
predictions from `api/models/day2_bundle.rds`.

Notes:
- Endpoint: `POST /predict/day2`
- Required inputs include all baseline clinical fields plus:
  `LEVEL1_TREATMENTS_D1_SAFE_0` through `LEVEL5_TREATMENTS_D1_SAFE_0`
- Demo scripts are included in this folder and match the Day 1 script patterns.
- Supports optional prevalence post-stratification strata:
  - `country`
  - `inpatient_status` (`Inpatient`/`Outpatient`, also accepts common aliases like `1/0`, `yes/no`)

## Prevalence Post-Stratification

The API always returns raw model output under the balanced (50/50) training prior:
- `mean_predicted_probability`
- `p_50_50` (same value as the raw mean probability)
- `t_50_50` (the threshold in the 50/50 world; defaults to model vote threshold)

If strata are supplied, the API additionally returns:
- `p_adj` (prevalence-adjusted probability)
- `t_adj` (prevalence-adjusted threshold)
- `prevalence`, `prevalence_scope`, `prevalence_stratum` (lookup metadata)

Lookup source:
- `strata prevalence adjustment/prevalence_all_nested.rds`
- overridable via env var `PREVALENCE_TABLE_PATH`

Accepted strata payload shapes:
- top-level fields: `country`, `inpatient_status`
- nested fields: `strata.country`, `strata.inpatient_status`
- per-row fields in `data` (for batch requests)
