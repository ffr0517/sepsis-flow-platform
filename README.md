**Sepsis Flow Platform Repository Quick Links**

[Platform Repo](https://github.com/ffr0517/sepsis-flow-platform)
[Day 1 API Service](./services/day1-api)
[Day 2 API Service](./services/day2-api)
[Web App](./services/web-app)

# Sepsis Flow Platform Repository

The Sepsis Flow platform is a research-focused multi-service system for two-day pediatric treatment-intensity prediction.
This repository contains the Day 1 and Day 2 prediction APIs (R + Plumber), an orchestration API, a static web app, and evaluation tooling.
It is packaged for evaluation and validation workflows and is not for clinical deployment.

---

## Current Implementation (Legacy-Compatible)

This repository runs the current two-day Sepsis Flow workflow.

### Active Models

| Model | Stage | Description |
|-------|-------|-------------|
| `day1_bundle.rds` | Day 1 | Ensemble bundle for L1-L5 Day 1 treatment predictions from baseline inputs. |
| `day2_bundle.rds` | Day 2 | Ensemble bundle for L1-L5 Day 2 treatment predictions from baseline inputs plus Day 1 carry-forward fields. |
| `prevalence_all_nested.rds` | Prior adjustment | Lookup table for whole, country, inpatient_status, and country_x_inpatient prevalence post-stratification. |

Each bundle is loaded directly by the matching service under `services/day1-api/api` and `services/day2-api/api`.

**Note:** `services/day1-api/sepsis-flow-api.yaml` documents an older API contract and is not the primary contract for the current `/predict/day1` and `/predict/day2` services.

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

```bash
# Day 1 API
cd services/day1-api/api
R -e "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8001)"

# Day 2 API
cd services/day2-api/api
R -e "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8002)"

# Orchestrator API (points to Day 1 + Day 2)
cd services/orchestrator-api/api
DAY1_API_BASE_URL=http://localhost:8001 DAY2_API_BASE_URL=http://localhost:8002 R -e "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8000)"

# Web app
cd services/web-app
python3 -m http.server 5173
```

Open `http://localhost:5173` for the web app.

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
