FROM jupyter/datascience-notebook:8b4d6f6ac0d7
#FROM jupyter/scipy-notebook:4609df0afbf3

RUN pip install 'BM25==1.0.*' \
    'plotly==4.4.1' \
    'chart_studio==1.1.*' \
    'cufflinks=0.17.*' \
    'pyarrow==0.16.*'

RUN export NODE_OPTIONS=--max-old-space-size=4096 &&\
    jupyter labextension install jupyterlab-plotly@4.6.0 --no-build &&\
    jupyter labextension install plotlywidget@4.6.0 --no-build &&\
    jupyter lab build &&\
    unset NODE_OPTIONS
