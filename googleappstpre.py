
# -*- coding: utf-8 -*-
import dash
import dash_core_components as dcc
import dash_html_components as html
import pandas as pd
import plotly.graph_objs as go
import kaggle
from dash.dependencies import Input, Output
import dash_table

kaggle.api.authenticate()
kaggle.api.dataset_download_files('lava18/google-play-store-apps', unzip=True)
google_apps = pd.read_csv("googleplaystore.csv", sep=',')
#print(google_apps)
df = google_apps.dropna()

external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)
app.title = 'Google Apps'
app.layout = html.Div(children=[
    html.H1(children='Google App Store'),

    html.Div(children='''
        Dash: A web application framework for Python. Viewing price and rating of Google Apps.
    '''),

    dcc.Graph(
        id='example-graph',
        figure={
            'data': [
                go.Scatter(
                    x=df[df['Category'] == i]['Price'],
                    y=df[df['Category'] == i]['Rating'],
                    text=df[df['Category'] == i]['App'],
                    mode='markers',
                    opacity=0.7,

                    name=i
                ) for i in df.Category.unique()


            ],
            'layout': {
                'title': 'Price vs Rating'
            }
        }
    ),
    dcc.Graph(
        id='graph 1',
        figure={
            'data': [
                go.Scatter(
                    x=df[df['Category'] == i]['Reviews'],
                    y=df[df['Category'] == i]['Rating'],
                    text=df[df['Category'] == i]['App'],
                    mode='markers',
                    opacity=0.5,

                    name=i
                ) for i in df.Category.unique()
            ],
            'layout': {
                'title': 'Rating vs Reviews'
            }
        }
    ),
    dcc.Graph(
        id='graph 2',
        figure={
            'data': [
                go.Box(
                    y=df['Reviews']
                )
            ],
            'layout': {
                'title': 'Reviews Boxplot'
            }
        }
    ),
    dcc.Graph(
        id='graph 3',
        figure={
            'data': [
                go.Box(
                    y=df['Rating']
                )
            ],
            'layout': {
                'title': 'Rating Boxplot'
            }
        }
    ),
    dcc.Graph(
        id='graph 4',
        figure={
            'data': [
                go.Histogram(
                    x=df['Type'],
                    y=df['Rating']
                )
            ],
            'layout': {
                'title': 'Rating Histogram'
            }
        }
    )
])

if __name__ == '__main__':
    app.run_server(debug=True)