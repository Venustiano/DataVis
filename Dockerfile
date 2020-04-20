FROM jupyter/datascience-notebook:584e9ab39d22

RUN pip install BM25
RUN pip install 'plotly==4.4.1'

RUN export NODE_OPTIONS=--max-old-space-size=4096 &&\
    jupyter labextension install jupyterlab-plotly@4.6.0 --no-build &&\
    jupyter labextension install plotlywidget@4.6.0 --no-build &&\
    jupyter lab build &&\
    unset NODE_OPTIONS
