"""
Sanity check
"""
import os
import pandas as pd
from models.amm import AMM

datapath = os.path.join("data", "soda.csv")
data = pd.read_csv(datapath)
data.columns

# Data transformations.
seasonals = "july4th+christmas+newyearseve+thanksgiving+stpatricksday"
sports = "superbowl+nbafinals"
base = "L5+year+price_liter+display_all+feature_all"

model = base + "+" + sports + "+" + seasonals
modifiers = {"MOUNTAIN DEW": 0.9}
# Aggregated Logit Model
agg = AMM()
agg.fit(data=data,
        RHS=model,
        share="share",
        agg=["year", "week"])
agg.summary()
agg.simulate(modifiers)
agg.to_latex(prefix="final-assignment/aggregated-logit")

# Nested Aggregated Logit Model (2 Levels)
n2lagg = AMM()
n2lagg.fit(data=data,
           RHS=model,
           share="share",
           agg=["year", "week"],
           nests=["FLAVOR/SCENT"])
n2lagg.summary()
n2lagg.simulate(modifiers)
n2lagg.to_latex(prefix="final-assignment/nested-2L-aggregated-logit")

# Nested Aggregated Logit Model (3 Levels)
n3lagg = AMM()
n3lagg.fit(data=data,
           RHS=model,
           share="share",
           agg=["year", "week"],
           nests=["FLAVOR/SCENT", "CALORIE LEVEL"])
print(n3lagg.summary())
n3lagg.simulate(modifiers)
n3lagg.to_latex(prefix="final-assignment/nested-3L-aggregated-logit")
