# DataVis


<a href="https://mybinder.org/v2/gh/Venustiano/DataVis/master" target="_blank">Open a container in Jupyter notebook</a>


<a href="https://mybinder.org/v2/gh/Venustiano/DataVis/master?urlpath=lab" target="_blank">Open a container in Jupyter lab</a>

### Build the container locally

```
docker build -t jupyter/datascience-notebook:DataVis .
```

### Run the container

```
docker run --rm -p 8888:8888 -v "$PWD":/home/jovyan/work jupyter/datascience-notebook:DataVis
```