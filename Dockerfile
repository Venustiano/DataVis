FROM jupyter/datascience-notebook:8b4d6f6ac0d7

USER root

RUN apt-get update && apt-get -y install \
    libxtst6 \
    libgconf2-4 \
    xvfb

USER $NB_USER

RUN conda install -y -c conda-forge cufflinks-py \
    rise \
    poppler
RUN R -e "install.packages(c('tikzDevice','GGally','plotly'),dependencies=TRUE, repos='http://cran.rstudio.com/')"

USER root

# Download orca AppImage, extract it, and make it executable under xvfb
RUN wget https://github.com/plotly/orca/releases/download/v1.1.1/orca-1.1.1-x86_64.AppImage -P /home
RUN chmod 777 /home/orca-1.1.1-x86_64.AppImage 

# To avoid the need for FUSE, extract the AppImage into a directory (name squashfs-root by default)
RUN cd /home && /home/orca-1.1.1-x86_64.AppImage --appimage-extract
RUN printf '#!/bin/bash \nxvfb-run --auto-servernum --server-args "-screen 0 640x480x24" \
    /home/squashfs-root/app/orca "$@"' > /usr/bin/orca
RUN chmod 777 /usr/bin/orca
RUN chmod -R 777 /home/squashfs-root/

USER $NB_USER

RUN export NODE_OPTIONS=--max-old-space-size=4096 &&\
    jupyter labextension install jupyterlab-plotly@4.6.0 --no-build &&\
    jupyter labextension install plotlywidget@4.6.0 --no-build &&\
    jupyter lab build &&\
    unset NODE_OPTIONS

COPY ./slides /home/$NB_USER/work/