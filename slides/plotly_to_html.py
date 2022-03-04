import pandas as pd
# Data
tableRes = pd.read_csv('data/results.txt',sep=' ')
# Long format
longTable = pd.melt(tableRes, id_vars=['iSubj','trial','Age','Decade'], var_name='myVars')
longTable.head()
longTable.groupby('myVars').transform(lambda value:(value - value.mean()) / value.std())['value']

# Normalizing the data
longTable["normVal"]=longTable.groupby('myVars'). \
                        transform(lambda value:(value - value.mean()) / value.std())["value"]
longTable.head() 

# Identifying older and younger, creating a new variable `old`
longTable["Old"] = longTable["Age"] > 60
longTable.head()

import plotly.express as px
fig = px.box(longTable, y="normVal", x="myVars", color="Old", width=800, 
             height=400, hover_data=longTable.columns)

fig.write_html("plotly_to_html_result.html")