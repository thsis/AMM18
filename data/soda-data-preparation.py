"""
Create full dataset of available information.

* join data of:
    + cola-dataset
    + ginger-ale-lemon-dew-dataset
    + sugar prices
    + weather
"""
import os
import itertools
import pandas as pd
import numpy as np
from glob import glob

# Paths
week_path = os.path.join("data", "IRI week translation_2008_2017.csv")
weather_path = os.path.join("data", "Weather_Boston_*")
sugar_path = os.path.join("data", "week_sugar.csv")
cola_path = os.path.join("data", "cola_amm_boston.csv")
soda_path = os.path.join("data", "GingerAleLemonDew_Chain139.csv")
outpath = os.path.join("data", "soda.csv")

# Task Nr. 1: Get weather data and translate it to IRI-format.
# Create converter.
date_cols = ["Calendar week starting on", "Calendar week ending on"]
iri_weeks = pd.read_csv(week_path, parse_dates=date_cols, usecols=[0, 1, 2])

iri_translator = {}
for i, row in iri_weeks.iterrows():
    dates = pd.date_range(row[1], row[2])
    week = {d: int(row["IRI Week"]) for d in dates}
    iri_translator = {**iri_translator, **week}

# Import and concatenate weather data.
weather = []
for file in glob(weather_path):
    df = pd.read_csv(file, header=0,
                     names=["year", "month", "day", "high", "avg", "low"],
                     keep_default_na=False)
    weather.append(df)
weather = pd.concat(weather)
weather_temp = weather.year*10000+weather.month*100+weather.day
weather["date"] = pd.to_datetime(weather_temp, format='%Y%m%d')
weather["week"] = weather.date.apply(lambda x: iri_translator.get(x))
weather.dropna(inplace=True)
weather["week"] = weather.week.astype(int)
weather.rename(columns={"high": "temperature_high",
                        "avg": "temperature_avg",
                        "low": "temperature_low"},
               inplace=True)
del weather_temp
# Aggregate weekly weather data.
weather = weather.groupby("week")["temperature_high",
                                  "temperature_avg",
                                  "temperature_low"].mean()

# Task Nr.2: sugar, cola and soda data.
sugar = pd.read_csv(sugar_path)
cola = pd.read_csv(cola_path)
soda = pd.read_csv(soda_path)


# Task Nr 3: create compount dataset.
# Harmonise columns in cola.
def flavor_mapper(x):
    if "DIET" in x:
        return "DIET"
    else:
        return "REGULAR"


cola.rename(columns={"feature": "f", "display": "d"}, inplace=True)
cola.drop(["price_deflated", "total_rev_cola", "total_rev_l4",
           "total_vol_carbbev", "total_vol_cola", "total_vol_l4"],
          axis=1, inplace=True)
cola["FLAVOR/SCENT"] = "COLA"
cola["CALORIE LEVEL"] = cola.L5.apply(flavor_mapper)


# Groom rows and columns of soda.
def l5_mapper(x):
    label = ""
    if (x["CALORIE LEVEL"] == "DIET") & ("DIET" not in x["L5"]):
        label = "DIET "
    if x["L5"] == "PRIVATE LABEL":
        label += x["FLAVOR/SCENT"] + " "
    return label + x["L5"]


soda["L5"] = soda.apply(l5_mapper, axis=1)
soda.drop("PRODUCT TYPE", axis=1, inplace=True)

# Aggregate out the upc column
agg_vars = ["MARKET", "CHAIN", "store_type", "iri_key", "week", "year", "L4",
            "L5", "VOL_EQ", "PACKAGE", "FLAVOR/SCENT", "CALORIE LEVEL"]
agg = soda.groupby(agg_vars).aggregate({"units": np.sum,
                                        "dollars": np.sum,
                                        "price": np.mean,
                                        "f": np.unique,
                                        "d": np.unique})
data = soda.drop(["upc", "units", "dollars", "price", "f", "d"], axis=1)
data = data.merge(agg, left_on=agg_vars, right_index=True)
# NOTE: we LOSE 45 rows, since different upc's are on display/feature or not.
data = data.loc[data.f.astype(str).apply(lambda x: x in soda.f.unique())]
data = data.loc[data.d.astype(str).apply(lambda x: x in ['0', '1', '2'])]

data = pd.concat([data, cola], sort=False)

# Task 4: Merge weather and sugar data.
data = data.merge(weather, on=["week"], how='left')
data = data.merge(sugar, on=["week"], how='left')

# Add seasonal data
seasonals = {"thanksgiving": [1160, 1213, 1265, 1317, 1369, 1421, 1473, 1526,
                              1578, 1630, 1682],
             "christmas": [1165, 1217, 1269, 1321, 1373, 1425, 1478, 1530,
                           1582, 1634, 1686],
             "newyearseve": [1166, 1218, 1270, 1322, 1374, 1426, 1479, 1531,
                             1583, 1635, 1687]}

data["thanksgiving"] = data["week"].apply(
    lambda x: x in seasonals["thanksgiving"]).astype(int)
data["christmas"] = data["week"].apply(
    lambda x: x in seasonals["christmas"]).astype(int)
data["newyearseve"] = data["week"].apply(
    lambda x: x in seasonals["newyearseve"]).astype(int)


data.to_csv(outpath, index=None)
