# Orchestrator API

This service provides a lightweight orchestration layer for the two-day flow:

1. `POST /flow/day1` calls Day 1 and derives Day 2 prefill.
2. `POST /flow/day2` calls Day 2 using baseline inputs plus editable Day 2 fields.

It returns a consistent response envelope:

- `ok`
- `data`
- `warnings`
- `error`
- `trace`

## Environment Variables

- `DAY1_API_BASE_URL` (default: `https://sepsis-flow-d1-api.onrender.com`)
- `DAY2_API_BASE_URL` (default: `https://sepsis-flow-platform.onrender.com`)
- `REQUEST_TIMEOUT_SECONDS` (default: `20`)
- `WARMUP_TIMEOUT_SECONDS` (default: `90`)
- `WARMUP_POLL_SECONDS` (default: `3`)
- `WARMUP_REQUEST_TIMEOUT_SECONDS` (default: `min(WARMUP_TIMEOUT_SECONDS, max(30, REQUEST_TIMEOUT_SECONDS))`)
- `DOWNSTREAM_RETRY_ATTEMPTS` (default: `3`)
- `DOWNSTREAM_RETRY_DELAY_SECONDS` (default: `2`)
- `CORS_ALLOW_ORIGINS` (default: `*`)
  - Comma-separated allowlist, e.g. `https://myapp.pages.dev,https://myapp.com`

## Endpoints

- `GET /health`
  - Returns orchestrator status and downstream Day 1/Day 2 health checks.

- `POST /flow/day1`
  - Accepts Day 1 baseline fields.
  - Returns `day1_result`, `day2_prefill`, and echoed `baseline_inputs`.

- `POST /flow/day2`
  - Accepts `baseline_inputs` + `day2_prefill` or a complete `data` object.
  - Returns `day2_result` and `final_day2_input_used`.

## Local Run

```bash
cd services/orchestrator-api/api
R -e "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8000)"
```

## Docker Build

```bash
docker build -f services/orchestrator-api/api/Dockerfile -t sepsis-flow-orchestrator .
docker run --rm -p 8000:8000 \
  -e DAY1_API_BASE_URL=https://sepsis-flow-d1-api.onrender.com \
  -e DAY2_API_BASE_URL=https://sepsis-flow-platform.onrender.com \
  sepsis-flow-orchestrator
```
