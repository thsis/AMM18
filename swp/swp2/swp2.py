import os
import pandas as pd
import numpy as np
from matplotlib import pyplot as plt

plotpath = os.path.join("swp", "swp2", "plots")
datapath = os.path.join("data", "cola_boston_clean.csv")
cola = pd.read_csv(datapath, index_col=0, parse_dates=True)

cola.shape
cola.columns
cola.head(10)

# Subset the data.
cola = cola[(cola["CHAIN"] == 33) | (cola["CHAIN"] == 65)]
cola.shape
cola.describe()

# How many stores does a chain have?
cola.groupby("CHAIN").iri_key.nunique()

# Average volume of carborated beverages by chain over time.
fig1, ax1 = plt.subplots(1, 1, figsize=(9, 4))
avg_vol = cola.groupby(["CHAIN", "week"]).total_vol_cola.mean()
avg_vol.unstack(level=0).plot(ax=ax1)
plt.ylabel("Average Volume")
plt.title("Average Volume of Cola over Time")
plt.grid(True)
plt.savefig(os.path.join(plotpath, "volume_vs_time.png"))
plt.show()

cola.groupby(["CHAIN", "L5", "PACKAGE"]).units.plot()
pkg_sal = cola.groupby(["CHAIN", "L5", "PACKAGE"]).units.sum().unstack(level=1)
pkg_sal
pkg_sal.index
pkg_sal.columns
pkg_sal.index.levels
pkg_sal.xs('CAN', level=1)

pkg_sal.hist()
plt.show()

cola["month"] = cola.index.month
avg_units = cola.groupby(["CHAIN", "PACKAGE", "L5", "month"]).units.mean()
avg_units = avg_units.unstack(level=[0, 1, 2])

fig, ax = plt.subplots(2, 2, figsize=(10, 5), sharex=True)
for i, chain in enumerate((65, 33)):
    for j, pkg in enumerate(('BOTTLE', 'CAN')):
        avg_units[chain][pkg].plot(grid=True, ax=ax[i, j], legend=False)
        patches, labels = ax[i, j].get_legend_handles_labels()

        ax[i, j].set_title("Chain {}, Package {}".format(chain, pkg))
        ax[i, j].set_ylabel("Units")
        if chain == 65 and pkg == 'BOTTLE':
            ax[i, j].legend(patches, labels, title="", ncol=2)


plt.suptitle("Average Unit Sales per month accross Chains and Package sizes")
plt.savefig(os.path.join(plotpath, "monthly_units.png"))
plt.show()
