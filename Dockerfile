# Use the official R image as base
FROM r-base:latest

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV PYTHON_VERSION=3.11

# Install system dependencies
RUN apt-get update && apt-get install -y \
    wget \
    curl \
    build-essential \
    gfortran \
    python3 \
    python3-dev \
    python3-pip \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgit2-dev \
    libssh2-1-dev \
    libglu1-mesa-dev \
    libgl1-mesa-dev \
    libgmp-dev \
    libmpfr-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    pandoc \
    texlive-latex-base \
    texlive-fonts-recommended \
    git \
    libtool \
    automake \
    autoconf \
    m4 \
    file \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install Python packages
RUN python3 -m pip install --no-cache-dir --break-system-packages \
    numpy \
    scipy \
    matplotlib \
    pandas \
    plotly \
    numba \
    llvmlite \
    unidec==7.0.3

# Install wxPython (keep specific handling)
RUN python3 -m pip install --no-cache-dir wxpython || echo "wxPython installation failed, continuing..."

COPY packages.R packages.R

RUN Rscript "packages.R"

# Create app directory
WORKDIR /app

# Copy application files
COPY . /app/

# Expose port 8180
EXPOSE 8180

# Set environment for Python/R integration
ENV RETICULATE_PYTHON=/usr/bin/python3

# Run the app
CMD Rscript /app/app.R
