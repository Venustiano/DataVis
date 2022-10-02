FROM r-base:4.2.1

ENV CRAN_URL "http://cran.rstudio.com"

RUN apt-get update && \
    apt-get install -y  build-essential \
    libcurl4-gnutls-dev \
    libfontconfig1-dev \
    freetype2-demos \
    libgit2-dev \
    dirmngr \
    libnode-dev \
    libzmq3-dev \
    libgmp3-dev \
    libmpfr-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libxml2-dev \
    libssl-dev \
	&& rm -rf /var/lib/apt/lists/*

# install R and customed R packages
RUN R -e "install.packages('devtools', repos='${CRAN_URL}');"
RUN R -e "library(devtools); \
 install_version( package = 'data.table', repos='${CRAN_URL}'); \
 install_version( package = 'ggplot2', repos='${CRAN_URL}'); \
 install_version( package = 'dplyr',  repos='${CRAN_URL}'); \
 install_version( package = 'jsonlite', repos='${CRAN_URL}'); \
 install_version( package = 'ggfortify', repos='${CRAN_URL}'); \
 install_version( package = 'htmlwidgets', repos='${CRAN_URL}'); \
 install_version( package = 'jsonvalidate', repos='${CRAN_URL}');"

RUN R -e 'devtools::install_github("ropensci/plotly")'
