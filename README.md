# Advanced Marketing Modeling 2018

## Getting started with Python
Download and install `Python3.6` from https://www.python.org/downloads/. Make sure to include it to your `PATH` variable if you are using `Windows` (just tick the box during the installation).

To run the `Python` code from this repository you may need to install additional modules. You can do this by opening a terminal (`CTRL-ALT-T` on `Linux` or `MacOS`) or a command line (on `Windows` just push `HOME` and start typing `run`, you should see a black icon).

First you want to change into your directory:
```
cd __path_to_your_cloned_repository__
```
Next you want to install the packages from the `requirements.txt`-file:
```
pip install -r requirements.txt
```

Thats all. After that you can run the python scripts by running:
```
python __name_of_script__.py
```
Note that this might be different if you are using `IDE`'s like `PyCharm`, `IDLE`, `Spyder`, etc.


## Weather data

This script scrapes the web for weather data, interfacing with the Dark Sky API
in order to obtain data about temperature and weather conditions measured at
Boston Logan International Airport. At the end you will write a `csv-file` to
the `data` directory of this repository (on your local disk, of course).


### How to run this script

1. Get a key from https://darksky.net/dev (don't worry, it's for free).
2. install the `darksky`-module from github, this seems to be the only version that is not broken:
```
pip install git+https://github.com/zachwill/darksky.git --user
```
3. Save your key in the `key.txt`-file.
4. run the `weather.py` script.

#### Functions
```python
ping_darksky(time, key)
```

Interface to Dark Sky API. Requests data from Boston Airport for a specific
time.

* Arguments:
    + time: a datetime object. Denotes the day of the requested record.
    + key: a character string. Key to interface the Dark Sky API.

* Returns:
    + Dictionary containing:
        + `day`: datetime of the observation (in timestamp format).
        + `tempMin`: minimum temperature in degrees Fahrenheit at that date.
        + `tempMax`: maximum temperature in degrees Fahrenheit at that date.
        + `summary`: weather summary of that date.
        + `desc`: single word description of weather conditions.

```python
switch_key():
```

Key generator that allows to switch between keys that are provided in the
`secret_key.txt` file.
* Yields:
    + Key to access Dark Sky API.

<h1 id="models">models</h1>

Models for AMM 18.

Create handy class interface for estimation, calculation of elasticities,
simulation of price changes and generation of Latex tables.

<h2 id="models.amm.AMM">AMM</h2>

```python
AMM(self)
```

Model class for the 2018 Advanced Marketing Modeling lecture.

Estimate a model, calculate elasticities and simulate the effects of a price change in just a few lines.

Examples:
```python
model = "L5+C(year)+price_liter+C(display_all)+C(feature_all)"
modifiers = {"DIET COKE": 0.25, "COKE CLASSIC": 1.25}
amm = AMM()
amm.fit(data=data.loc[data.PACKAGE == "BOTTLE"],
        RHS=model,
        share="share",
        agg=["year", "week"])
amm.summary()
amm.simulate(modifiers)
amm.to_latex()
```

<h3 id="models.amm.AMM.fit">fit</h3>

```python
AMM.fit(self, data, RHS, share, agg=None, nests=None, grp_unit='liters', price_var='price_liter')
```

Fit AMM-model to data and calculate elasticities.

The type of model is specified through the `nests` parameter. If
`nests` is None (the default) an Aggregated Logit Model is fitted. If
`nests` is a list, a Nested Aggregated Logit Model is fitted. The
length of the list determines the nesting structure. A length of one
corresponds to a 2 level Nested Aggregated Logit Model and a length of
two corresponds to a 3 level Nested Aggregated Logit Model.

Note that `data` has very specific assumptions in place. It assumes an
IRI format which means it expects following columns to exist:

* L5: brand names
* *share*: precomputed shares for each observation.
* *grp_unit*: precomputed measure of volume.
* *price_var*: precomputed measure of price.

Where L5 is necessary and each *column* can be customized to allow for
different ways to infer the shares of the outside good (you don't need
to calculate the outside good explicitly).

For example: it is possible to compute shares of each product based on:

* `total_rev_cola`
* `total_rev_carbbev`
* `total_vol_cola`
* `total_vol_carbbev`

Pick one, calculate the shares (by yourself), store the results as a
column in data and set *share* to that column's name. Similarly, you
need to proceed with *grp_unit* and *price_var*.

Parameters:
* `data`: a `pandas.DataFrame` formatted in the IRI format.
* `RHS`: string of variables used in the OLS model.
* `share`: column of data that stores the shares of the products.
* `agg`: list of most coarse aggregation level. Defaults to `None`
* `nests`: list of nesting structure.
* `grp_unit`: column of data that specifies group aggregation units.
* `price_var`: column of data that specifies prices to be used.

Returns:
* self

Examples:
```python
model = "L5+C(year)+price_liter+C(display_all)+C(feature_all)"
amm = AMM()
amm.fit(data=data.loc[data.PACKAGE == "BOTTLE"],
        RHS=model,
        share="share",
        agg=["year", "week"])
```
<h3 id="models.amm.AMM.simulate">elasticities</h3>

```python
AMM.elasticities(self)
```
Calculate elasticities.

Returns
* table of estimated elasticities

Examples:
```python
amm.elasticities()
```
<h3 id="models.amm.AMM.simulate">simulate</h3>

```python
AMM.simulate(self, modifiers={})
```

Simulate price changes.

Parameters:
* `modifiers`: dictionary
    * key: brand, must appear in data's L5 column
    * value: multiplier of price

Returns:
* self

Examples:
```python
modifiers = {"DIET COKE": 0.25, "COKE CLASSIC": 1.25}
amm.simulate(modifiers)
```
<h3 id="models.amm.AMM.summary">summary</h3>

```python
AMM.summary(self)
```

Print summary of OLS-model and estimated elasticities to stdout.

<h3 id="models.amm.AMM.to_latex">to_latex</h3>

```python
AMM.to_latex(self, prefix='')
```

Write results as Latex-tables to disk.

Creates three files:
* `ols_results.tex`: file containing OLS-summary tables
* `elasticities.tex`: file containing elasticity-tables
* `simulation.tex`: file containing simulation-tables

Parameters:
* `prefix`: string to prepend filename (defaults to empty string)
