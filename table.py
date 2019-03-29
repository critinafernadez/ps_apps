## TABLE

import dash
import dash_table
import pandas as pd
import kaggle

kaggle.api.authenticate()
kaggle.api.dataset_download_files('lava18/google-play-store-apps', unzip=True)
google_apps = pd.read_csv("googleplaystore.csv", sep=',')
print(google_apps)
google_apps = google_apps.dropna()
#google_apps1 = google_apps[['App','Category','Rating','Reviews','Installs','Type','Price','Last Updated']]

app = dash.Dash(__name__)

app.layout = dash_table.DataTable(
    id='table',
    columns=[{"name": i, "id": i} for i in google_apps.columns[:14]],
    data=google_apps.to_dict("rows"),
)

if __name__ == '__main__':
    app.run_server(debug=True)