FROM rocker/r-ver:4.3.2

# Minimal runtime dependencies for this API.
RUN apt-get update && apt-get install -y --no-install-recommends \
    make \
    g++ \
    && rm -rf /var/lib/apt/lists/*

RUN R -q -e "install.packages(c('plumber','jsonlite'), repos='https://cloud.r-project.org')"

WORKDIR /app
COPY api/plumber.R /app/plumber.R
COPY api/R /app/R
COPY api/models /app/models

EXPOSE 8000

CMD ["R", "-e", "port <- as.integer(Sys.getenv('PORT', unset = '8000')); pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=port)"]
