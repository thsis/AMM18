import os
import numpy as np
import pandas as pd
import seaborn as sns
from matplotlib import pyplot as plt

datapath = os.path.join("data", "soda.csv")
data = pd.read_csv(datapath, parse_dates=["start_week"])
data.boxplot("share", vert=False, by="L5")
plt.savefig(os.path.join("figures", "boxplot_share.png"))
plt.close()
data.plot.scatter(x="temperature_avg", y="share")
plt.savefig(os.path.join("figures", "scatterplot_share_temp.png"))
plt.close()

sns.pairplot(data)
plt.savefig(os.path.join("figures", "pairplot_soda.png"))
plt.close()
# Correlation matrix
corr = data.drop(["CHAIN", "week", "VOL_EQ"], axis=1).corr()
fig, ax = plt.subplots(figsize=(9, 6))
sns.heatmap(corr,
            mask=np.zeros_like(corr, dtype=np.bool),
            cmap=sns.diverging_palette(220, 10, as_cmap=True),
            square=True,
            ax=ax)
ax.set_title("Correlation plot")
plt.tight_layout()
plt.savefig(os.path.join("figures", "correlation_plot.png"))
plt.close()

# Find influential weeks.
grouping = [data.start_week.dt.week, "year"]
top_weeks = data.groupby(grouping).units.sum().to_frame("sum_units")
top = data.groupby(grouping[0]).units.mean().sort_values(ascending=False)
top[:10].index
top.mean()
# Week 47: thanksgiving
# Week 51: christmas
# Week 1: New years eve
# Week 10: Week before St. Patrick's Day

# Usual suspects:
# july4th
# superbowl
# NBA finals
# baseball league post-season
calendar_weeks = data.start_week.dt.week
weekly = data.groupby(["FLAVOR/SCENT", calendar_weeks]).units.mean().reset_index()
fig, ax = plt.subplots(figsize=(10, 6))
sns.lineplot(data=weekly,
             x="start_week",
             y="units",
             hue="FLAVOR/SCENT",
             ax=ax)
plt.title("Mean unit sales by flavor over time")
plt.legend(loc="upper right")
plt.xlabel("calendar week")
plt.ylabel("average units sold")
plt.savefig(os.path.join("figures", "unit_sales_time.png"))
plt.close()
data.columns
data.loc[data["FLAVOR/SCENT"] == "DEW"].L5.unique()
