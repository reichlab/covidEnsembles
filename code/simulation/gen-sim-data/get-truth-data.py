# adapted from
# https://github.com/reichlab/covid19-forecast-hub/blob/master/data-truth/get-truth-data.py

import pandas as pd
import pymmwr as pm
import datetime
import warnings
import io
import requests
warnings.simplefilter(action='ignore')


def get_epi_data(date):
    format_str = '%m/%d/%y'  # The format
    dt = datetime.datetime.strptime(date, format_str).date()
    epi = pm.date_to_epiweek(dt)
    return epi.year, epi.week, epi.day


def save_JHU_data():
    url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
    url_req = requests.get(url).content
    df = pd.read_csv(io.StringIO(url_req.decode('utf-8')))

    fips_codes = pd.read_csv('data-raw/fips_codes.csv')

    # aggregate by state and nationally
    state_agg = df.groupby(['Province_State']).sum()
    us_nat = df.groupby(['Country_Region']).sum()
    df_state_nat = state_agg.append(us_nat)

    # drop unnecessary columns
    cols = list(range(0, 6))
    df_truth = df_state_nat.drop(df_state_nat.columns[cols], axis=1)

    df_truth_incident = df_truth - df_truth.shift(periods=1, axis='columns')

    # convert matrix to repeating row format
    df_truth = df_truth_incident.unstack()
    df_truth = df_truth.reset_index()

    # get epi data from date
    df_truth['year'], df_truth['week'], df_truth['day'] = \
        zip(*df_truth['level_0'].map(get_epi_data))

    # rename columns
    df_truth = df_truth.rename(columns={0: "value",
                                        "level_1": "location_long"})

    # Get state IDs
    df_truth = df_truth.merge(fips_codes, left_on='location_long', right_on='location_name', how='left')

    # Drop NAs
    df_truth = df_truth.dropna(subset=['location', 'value'])

    # add leading zeros to state code
    df_truth['location'] = df_truth['location'].apply(lambda x: '{0:0>2}'.format(x))

    # Only keep certain locations
    locations = ['US', 'Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California',
                'Colorado', 'Connecticut', 'Delaware', 'District of Columbia',
                'Florida', 'Georgia', 'Guam', 'Hawaii', 'Idaho', 'Illinois',
                'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine',
                'Maryland', 'Massachusetts', 'Michigan', 'Minnesota',
                'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada',
                'New Hampshire', 'New Jersey', 'New Mexico', 'New York',
                'North Carolina', 'North Dakota', 'Northern Mariana Islands',
                'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Puerto Rico',
                'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee',
                'Texas', 'Utah', 'Vermont', 'Virgin Islands', 'Virginia',
                'Washington', 'West Virginia', 'Wisconsin', 'Wyoming']
    df_truth = df_truth[df_truth["location_long"].isin(locations)]

    # Group by week for incident deaths
    df_vis = df_truth.groupby(['week', 'location_long'], as_index=False). \
        agg({'level_0': 'last',
            'value': 'sum',
            'year': 'last',
            'day': 'last',
            'location': 'last',
            'location_abbreviation': 'last'})
    df_vis = df_vis[df_vis['day'] == 7]
    df_vis['value'] = df_vis['value'].astype(int)

    # add leading zeros to epi week
    df_vis['week'] = df_vis['week'].apply(lambda x: '{0:0>2}'.format(x))

    # define epiweek
    df_vis['epiweek'] = df_vis['year'].astype(str) + df_vis['week']

    # keep only necessary columns
    df_truth_short = df_vis[["location", "location_abbreviation", "year", "week", "epiweek", "value"]]

    df_truth_short.to_csv('data-raw/jhu-incident.csv', index=False)


if __name__ == "__main__":
    save_JHU_data()
