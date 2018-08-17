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
soda_path = os.path.join("data", "GingerAleLemonDew_Chain139.csv")
outpath = os.path.join("data", "soda.csv")

# Task Nr. 1: Get weather data and translate it to IRI-format.
# Create converter.
iri_weeks = pd.read_csv(week_path, parse_dates=["start", "end"],
                        header=0,
                        usecols=[0, 1, 2],
                        names=["week", "start", "end"])

date2IRI = {}
for i, row in iri_weeks.iterrows():
    dates = pd.date_range(row[1], row[2])
    week = {d: int(row["week"]) for d in dates}
    date2IRI = {**date2IRI, **week}

IRI2date = iri_weeks.set_index("week").start.to_dict()

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
weather["week"] = weather.date.apply(lambda x: date2IRI.get(x))
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

# Task Nr.2: sugar and soda data.
sugar = pd.read_csv(sugar_path)
soda = pd.read_csv(soda_path)


# Task Nr 3: create compount dataset.
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
agg_vars = ["MARKET", "CHAIN", "store_type", "week", "year", "L4",
            "L5", "VOL_EQ", "PACKAGE", "FLAVOR/SCENT", "CALORIE LEVEL"]
# Get dummies.
soda = pd.get_dummies(soda, columns=["d", "f"], drop_first=True, dtype=bool)
soda["feature_all"] = (~soda["f_NONE"]).astype(int)
soda["display_all"] = soda["d_1"] | soda["d_2"]

# Aggregate - Drop upc's + stores.
data = soda.groupby(agg_vars).aggregate({"units": np.sum,
                                         "dollars": np.sum,
                                         "price": np.mean,
                                         "total_rev_carbbev": np.mean,
                                         "feature_all": np.mean,
                                         "display_all": np.mean}).reset_index()

# Task 4: Merge weather and sugar data.
data = data.merge(weather, on=["week"], how='left')
data = data.merge(sugar, on=["week"], how='left')

# Add seasonal data
data["start_week"] = data.week.apply(lambda x: IRI2date.get(x))
seasonals = {"thanksgiving": [1160, 1213, 1265, 1317, 1369, 1421, 1473, 1526,
                              1578, 1630, 1682],
             "christmas": [1165, 1217, 1269, 1321, 1373, 1425, 1478, 1530,
                           1582, 1634, 1686],
             "newyearseve": [1166, 1218, 1270, 1322, 1374, 1426, 1479, 1531,
                             1583, 1635, 1687],
             "superbowl": [1117, 1170, 1217, 1274, 1327, 1379, 1431, 1483,
                           1535, 1588, 1641],
             "july4th": [1139, 1191, 1243, 1295, 1347, 1399, 1451, 1503, 1555,
                         1609, 1662]}

for key in seasonals.keys():
    data[key] = data["week"].apply(lambda x: x in seasonals[key]).astype(int)

# Week before St. Patrick's day.
data["stpatricksday"] = (data.start_week.dt.week == 10).astype(int)
data["nbafinals"] = data.start_week.dt.week.isin([22, 23]).astype(int)

data["liters"] = data["VOL_EQ"] * data["units"]
data["price_liter"] = data["price"] / data["VOL_EQ"]

agg = data.groupby(["year", "week", "PACKAGE"])["liters", "dollars"].sum()
agg.columns = ["sum_liters", "sum_dollars"]
res = data.merge(agg, left_on=["year", "week", "PACKAGE"],
                 right_index=True)

data["share"] = res["liters"]/res["sum_liters"] * (
    res["sum_dollars"]/data["total_rev_carbbev"])


# Clean the labels
data.loc[data["FLAVOR/SCENT"] == "CITRUS DEW", "FLAVOR/SCENT"] = "DEW"

# Save to disk
data.to_csv(outpath, index=None)
