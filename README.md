# Sepsis Flow Platform

Research prototype for two-day pediatric treatment-intensity prediction. This repository contains the Day 1 and Day 2 R/Plumber APIs, an orchestration API, a hosted web app, and evaluation tooling for validation workflows.

> Warning
> As of March 27, 2026, Sepsis Flow is unpublished and not yet peer reviewed. It is a research prototype only and must not be used for clinical deployment or patient-care decision-making.

## Quick Links

- [Live demo](https://sepsis-flow-web-app.onrender.com)
- [Model build repo](https://github.com/ffr0517/sepsis-flow-build)
- [Day 1 API service](./services/day1-api)
- [Day 2 API service](./services/day2-api)
- [Web app](./services/web-app)

---

## Implementation


### Active Models

| Model | Stage | Description |
|-------|-------|-------------|
| `day1_bundle.rds` | Day 1 | Ensemble bundle for L1-L5 Day 1 treatment predictions from baseline inputs. |
| `day2_bundle.rds` | Day 2 | Ensemble bundle for L1-L5 Day 2 treatment predictions from baseline inputs plus Day 1 carry-forward fields. |
| `prevalence_all_nested.rds` | Prior adjustment | Lookup table for whole, country, inpatient_status, and country_x_inpatient prevalence post-stratification. |

Each bundle is loaded directly by the matching service under `services/day1-api/api` and `services/day2-api/api`.

### Active Implementation Details

#### Data preprocessing and feature engineering

- Missing numeric predictors are median-imputed and missing nominal predictors are mode-imputed.
- Additionally, explicit missingness indicators are included so missingness patterns remain informative.
- `oxy.ra` (SpO2 %) is log-transformed to reduce skew.
- Age interactions with respiratory rate and heart rate are included to reflect age-dependent physiology.
- Zero-variance predictors are removed.
- Numeric predictors are normalized as the final preprocessing step.

#### Day 1 treatment prediction design

- Five separate Day 1 treatment outcomes are modeled.
- Inputs are baseline clinical predictors only.
- Each outcome model returns probability of receiving that Day 1 treatment level.

#### Day 2 cascade and training design

- Five separate Day 2 treatment outcomes are modeled.
- Inputs are baseline clinical predictors plus Day 1 treatment indicators.
- To avoid assuming perfect Day 1 knowledge at training time, Day 2 uses a double-record design per participant (inputed + real Day 1 treatment indicators)

#### Ensemble specification and voting aggregation

- Each outcome uses 120 constituent heads:
  - 40 linear lasso heads
  - 40 spline-lasso heads
  - 40 CART heads
- This structure is repeated for five outcomes on Day 1 and five outcomes on Day 2.
- Per-head probabilities are converted to votes at threshold 0.5
- Final classification uses simple majority

#### Prior-adjusted probability outputs

- Raw ensemble probabilities are produced under balanced (50/50) training priors and exposed as:
  - `mean_predicted_probability`
  - `p_50_50`
  - `t_50_50`
- When strata are provided, APIs additionally return:
  - `p_adj`
  - `t_adj`
  - prevalence metadata fields
- Strata-supported adjustments are:
  - `country`
  - `inpatient_status`
  - `country_x_inpatient`
- Adjustment uses prevalence from `prevalence_all_nested.rds` 
- This mapping is monotonic, so ranking is preserved while probabilities are re-anchored to the selected prevalence stratum.

---

## API Overview

The platform exposes direct Day 1/Day 2 prediction endpoints and an orchestrated two-step flow that handles warm-up, Day 1 inference, and Day 2 prefill derivation.

### Endpoints

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/health` (orchestrator) | GET | Returns orchestrator status plus downstream Day 1 and Day 2 health checks. |
| `/warmup` (orchestrator) | POST | Actively warms and checks Day 1 and Day 2 APIs before user interaction. |
| `/flow/day1` (orchestrator) | POST | Runs Day 1 prediction, returns `day1_result`, and derives `day2_prefill`. |
| `/flow/day2` (orchestrator) | POST | Runs Day 2 prediction from `baseline_inputs` + `day2_prefill` (or full `data`). |
| `/predict/day1` (day1-api) | POST | Direct Day 1 predictions with optional prevalence adjustment strata. |
| `/predict/day2` (day2-api) | POST | Direct Day 2 predictions with optional prevalence adjustment strata. |

---

## Example Request (Stage 1)

```bash
curl -X POST http://localhost:8000/flow/day1?format=long \
  -H "Content-Type: application/json" \
  -d '{
        "data": {
          "age.months": 24,
          "sex": 0,
          "adm.recent": 0,
          "wfaz": -1.1,
          "cidysymp": 2,
          "not.alert": 0,
          "hr.all": 120,
          "rr.all": 28,
          "envhtemp": 37.8,
          "crt.long": 0,
          "oxy.ra": 98
        },
        "strata": {
          "country": "Bangladesh",
          "inpatient_status": "Inpatient"
        }
      }'
```

---

## Service Architecture

- **Orchestrator router:** `services/orchestrator-api/api/plumber.R` -> startup check (`/warmup`) and two-step flow (`/flow/day1`, `/flow/day2`).
- **Day 1 API:** `services/day1-api/api/plumber.R` -> direct Day 1 ensemble inference and optional prevalence post-stratification.
- **Day 2 API:** `services/day2-api/api/plumber.R` -> direct Day 2 ensemble inference and optional prevalence post-stratification.
- **Model artefacts:** `services/day1-api/api/models/day1_bundle.rds` and `services/day2-api/api/models/day2_bundle.rds`.
- **Prevalence table:** `strata prevalence adjustment/prevalence_all_nested.rds` used by both APIs (overridable via `PREVALENCE_TABLE_PATH`).
- **Frontend:** `services/web-app/` static UI for startup-gated Day 1 -> Day 2 workflow and CSV export.
- **Evaluation script:** `test_eval.R` for batched API evaluation across prior-adjustment variants.

---

## Privacy and Data Handling

- APIs are designed for transient in-memory inference and do not intentionally persist patient payloads.
- Any persistence, request logging, or telemetry behavior depends on the hosting platform configuration.
- Deployments should be protected with HTTPS, access controls, and institution-approved governance policies.
- Existing privacy materials in service folders should be reviewed before production-like deployments.

---

## Deployment

### Local

Recommended local UI workflow (after cloning/pulling this repo): run the local stack script from the repository root, then open the local page (`index.local.html`).

Prerequisites:
- `R` (with required packages installed, including `plumber`)
- `python3`
- `curl`

```bash
# From the repository root
# Starts Day 1 API, Day 2 API, orchestrator, and local web server
./scripts/run-local-web.sh

# If executable permissions are missing on your machine:
# bash ./scripts/run-local-web.sh

# Open in your browser:
# http://localhost:5173/index.local.html
```

What this does:
- Starts local Day 1 API (`:8001`)
- Starts local Day 2 API (`:8002`)
- Starts local orchestrator (`:8000`) pointed at the local APIs
- Starts the static web app server (`:5173`)
- Waits for health checks and runs orchestrator warmup

Important:
- Use `http://localhost:5173/index.local.html` for the local-API web app
- `http://localhost:5173/index.html` still points at deployed Render endpoints
- Local-only frontend changes should go in `services/web-app/app.local.js`
- The Render-hosted frontend remains on `services/web-app/app.js`

If you see `Failed to fetch` in the local page status:
- Confirm the script is still running
- Check the log directory path printed by the script
- Confirm `http://localhost:8000/health` loads locally

Manual startup (advanced / debugging):

```bash
# Day 1 API
cd services/day1-api/api
R -e "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8001)"

# Day 2 API
cd services/day2-api/api
R -e "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8002)"

# Orchestrator API (points to Day 1 + Day 2)
cd services/orchestrator-api/api
DAY1_API_BASE_URL=http://localhost:8001 DAY2_API_BASE_URL=http://localhost:8002 CORS_ALLOW_ORIGINS=http://localhost:5173 R -e "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8000)"

# Web app
cd services/web-app
python3 -m http.server 5173
```

Open `http://localhost:5173/index.local.html` for the local-API web app.
(`http://localhost:5173/index.html` still points at deployed Render endpoints.)
Local-only frontend changes should be made in `services/web-app/app.local.js` (not `services/web-app/app.js`).

### Remote

`render.yaml` currently defines deployment for the orchestrator (`sepsis-flow-orchestrator`) and static web app (`sepsis-flow-web-app`).
Day 1 and Day 2 APIs can be deployed as separate services and then connected via `DAY1_API_BASE_URL` and `DAY2_API_BASE_URL`.

---

## Repository Structure

```text
sepsis-flow-platform/
├── README.md
├── render.yaml
├── test_eval.R
├── strata prevalence adjustment/
│   └── prevalence_all_nested.rds
└── services/
    ├── day1-api/
    │   ├── README.md
    │   ├── sepsis-flow-api.yaml
    │   └── api/
    │       ├── plumber.R
    │       ├── Dockerfile
    │       └── models/day1_bundle.rds
    ├── day2-api/
    │   ├── README.md
    │   └── api/
    │       ├── plumber.R
    │       ├── Dockerfile
    │       └── models/day2_bundle.rds
    ├── orchestrator-api/
    │   ├── README.md
    │   └── api/
    │       ├── plumber.R
    │       ├── Dockerfile
    │       └── R/flow.R
    └── web-app/
        ├── README.md
        ├── index.html
        ├── app.js
        └── styles.css
```
---

## Disclaimer

This platform is provided for research purposes only.
It is not a certified clinical product, and outputs must not be used as the sole basis for patient care decisions.
All contributors and deployers are responsible for data governance, ethics approval, and regulatory compliance in their setting.
