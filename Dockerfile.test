FROM jupyter/datascience-notebook

USER root

RUN apt-get update && apt-get -y install \
    libxtst6 \
    libprotobuf-dev \
    protobuf-compiler \
    cmake \
    libpoppler-cpp-dev \
    poppler-utils

USER $NB_USER

RUN pip install --quiet --no-cache-dir \
	RISE \
	plotly \
	cufflinks \
	kaleido


RUN R -e "install.packages(c('tikzDevice','GGally','plotly','repr','IRdisplay','pdbZMQ','devtools'), dependencies=TRUE, repos='http://cran.rstudio.com/')"

USER $NB_USER

COPY ./slides /home/$NB_USER/work/
