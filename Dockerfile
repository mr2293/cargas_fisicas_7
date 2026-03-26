FROM rocker/shiny:4.5.1

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libv8-dev \
    g++ \
    python3 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /home/cargas_fisicas_7

COPY cargas_fisicas_7.Rproj renv.lock ./
COPY app.R cargas7.R deploy.R ./
COPY data data

RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')" && \
    R -e "renv::restore(prompt = FALSE)"

# Strip non-standard array fields from renv.lock so rsconnect can parse it
RUN python3 -c "
import json
with open('renv.lock') as f:
    lock = json.load(f)
std = {'Package','Version','Source','Repository','Hash','Requirements','RemoteType','RemoteHost','RemoteUsername','RemoteRepo','RemoteRef','RemoteSha','RemoteSubdir','RemotePkgRef'}
for pkg in lock.get('Packages', {}).values():
    for k in list(pkg.keys()):
        if k not in std:
            del pkg[k]
with open('renv.lock', 'w') as f:
    json.dump(lock, f, indent=2)
"

CMD ["Rscript", "deploy.R"]
