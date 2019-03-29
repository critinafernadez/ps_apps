## BASIC SCATTERPLOT

import dash
import dash_table
import dash_core_components as dcc
import dash_html_components as html
import pandas as pd
import plotly.graph_objs as go
import kaggle
from dash.dependencies import Input, Output

external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']
#df = pd.read_csv('C:/Users/crisf/OneDrive/Escritorio/ESTADISTICA DATA SCIENCE/''Semicuatri 3/Generación de informes/final_clean_data.csv', encoding="ISO-8859-1")

kaggle.api.authenticate()
kaggle.api.dataset_download_files('lava18/google-play-store-apps', unzip=True)
google_apps = pd.read_csv("googleplaystore.csv", sep=',')
print(google_apps)
df = google_apps.dropna()
#google_apps1 = google_apps[['App','Category','Rating','Reviews','Installs','Type','Price','Last Updated']]

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)
app.title='Google Apps'


app.layout = dash_table.DataTable(
    id='table',
    columns=[{"name": i, "id": i} for i in google_apps.columns[:14]],
    data=google_apps.to_dict("rows"),
)

app.layout = html.Div([
    dcc.Graph(
        id='Rating vs Reviews',
        figure={
            'data': [
                go.Scatter(
                    x=df[df['Category'] == i]['Reviews'],
                    y=df[df['Category'] == i]['Rating'],
                    text=df[df['Category'] == i]['App'],
                    mode='markers',
                    opacity=0.7,
                    marker={
                        'size': 34,
                        'line': {'width': 0.5, 'color': 'white'}
                    },
                    name=i
                ) for i in df.Category.unique()
            ],
            'layout': go.Layout(
                xaxis={'title': 'Reviews'},
                yaxis={'title': 'Rating'},
                margin={'l': 40, 'b': 40, 't': 10, 'r': 10},
                legend={'x': 0, 'y': 1},
                hovermode='closest'
            )
        }
    )
])

if __name__ == '__main__':
    app.run_server(debug=True)