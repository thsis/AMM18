"""
### What does this script?

This script scrapes the web for weather data, interfacing with the Dark Sky API
in order to obtain data about temperature and weather conditions measured at
Boston Logan International Airport. At the end you will write a `csv-file` to
the `data` directory of this repository (on your local disk, of course).
"""

import os
import warnings
import requests
import pandas as pd
from darksky import forecast
from datetime import datetime
from tqdm import tqdm


def ping_darksky(time, key):
    """
    Interface to Dark Sky API. Requests data from Boston Airport for a specific
    time.

    * Arguments:
        + time: a datetime object. Denotes the day of the requested record.
        + key: a character string. Key to interface the Dark Sky API.

    * Returns:
        + Dictionary containing:
            + day: datetime of the observation (in timestamp format).
            + tempMin: minimum temperature in degrees Fahrenheit at that date.
            + tempMax: maximum temperature in degrees Fahrenheit at that date.
            + summary: weather summary of that date.
            + desc: single word description of weather conditions.

    """
    with forecast(key, *BOSTON, time=time.isoformat()) as boston:
        fetch = {'day': time,
                 'tempMin': boston.daily[0].temperatureMin,
                 'tempMax': boston.daily[0].temperatureMax,
                 'summary': boston.daily[0].summary,
                 'desc': boston.daily[0].icon}
    return fetch


def switch_key():
    """
    Key generator that allows to switch between keys that are provided in the
    `secret_key.txt` file.

    * Yields:
        + Key to access Dark Sky API.
    """
    with open("secret_key.txt", 'r') as f:
        keys = f.read().splitlines()

    for key in keys:
        yield key


BOSTON = (42.3601, 71.0589)
feature_columns = ["day", "tempMin", "tempMax", "summary", 'desc']
weather_boston = pd.DataFrame(columns=feature_columns)
dataout = os.path.join("data", "weather_boston_daily.csv")

start = datetime(2001, 4, 2, 12)
keygen = switch_key()
key = next(keygen)

for day in tqdm(pd.date_range(start, periods=4004)):
    try:
        row = ping_darksky(key=key, time=day)
        weather_boston = weather_boston.append(row, ignore_index=True)
    except requests.exceptions.HTTPError:
        key = next(keygen)
        row = ping_darksky(key=key, time=day)
        weather_boston = weather_boston.append(row, ignore_index=True)
        continue
    except StopIteration:
        warnings.warn("End of keys reached. Your dataset might be incomplete.")
        break
    finally:
        weather_boston.to_csv(dataout)

print("Wrote {} rows".format(weather_boston.shape[0]))
