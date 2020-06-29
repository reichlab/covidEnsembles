import pandas as pd
import requests
import json

# retrieve information about all commits that modified the file we want
all_commits = []

page = 0
while True:
    page += 1
    r = requests.get(
        'https://api.github.com/repos/CSSEGISandData/COVID-19/commits',
        params = {
            'path': 'csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv',
            'page': str(page)
        }
    )
    
    if (not r.ok) or (r.text == '[]'):
        break
    
    all_commits += json.loads(r.text or r.content)


# date of each commit
commit_dates = [
    commit['commit']['author']['date'][0:10] for commit in all_commits
]

# sha for the last commit made each day
commit_shas_to_get = {}
for index, value in enumerate(commit_dates):
    result_path =  '../../data-raw/JHU/' + value + \
        '_time_series_covid19_deaths_US.csv'
    if (value not in commit_shas_to_get) and (not os.path.isfile(result_path)):
        commit_shas_to_get[value] = all_commits[index]['sha']

# download and save the csvs
for commit_date, commit_sha in commit_shas_to_get.items():
    df = pd.read_csv(
        'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/' +
        commit_sha +
        '/csse_covid_19_data/csse_covid_19_time_series/' \
        'time_series_covid19_deaths_US.csv')
    
    df.to_csv('../../data-raw/JHU/' + commit_date + \
        '_time_series_covid19_deaths_US.csv', index=False)
