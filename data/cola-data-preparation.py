import os
import pandas as pd

# Load dataset.
datapath = os.path.join("data", "cola_amm_boston.csv")
dataout = os.path.join("data", "cola_amm_boston_clean.csv")
data = pd.read_csv(datapath)

# Subset data.
subset = (data.year > 2009) & (data.CHAIN == 65)
data = data.loc[subset]

# Data transformations.
data["liters"] = data["VOL_EQ"] * data["units"]
data["price_liter"] = data["price"] / data["VOL_EQ"]

# Recode categorical data.
data = pd.get_dummies(data, columns=["display", "feature"], drop_first=True)
data["feature_all"] = (~data["feature_NONE"]).astype(int)
data["display_all"] = data["display_1"] + data["display_2"]

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

# Calculate Shares.
agg = data.groupby(["year", "week", "PACKAGE"])["liters", "dollars"].sum()
agg.columns = ["sum_liters", "sum_dollars"]
res = data.merge(agg, left_on=["year", "week", "PACKAGE"],
                 right_index=True)

data["share"] = res["liters"]/res["sum_liters"] * (
    res["sum_dollars"]/data["total_rev_cola"])

# Define sugar-group.
groups = {"DIET COKE": "diet",
          "DIET PEPSI": "diet",
          "COKE CLASSIC": "regular",
          "PEPSI": "regular"}

data["sugar"] = data.L5.apply(lambda x: groups[x])
data.to_csv(dataout, index=None)
