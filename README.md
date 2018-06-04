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

1. Get a key from https://darksky.net/dev (don't worry, it's for free).
2. install the `darksky`-module from github, this seems to be the only version that is not broken:
```
pip install git+https://github.com/zachwill/darksky.git --user
```
3. Save your key in the `key.txt`-file.
4. run the `weather.py` script.

### What does this script?

This script scrapes the web for weather data, interfacing with the Dark Sky API
in order to obtain data about temperature and weather conditions measured at
Boston Logan International Airport. At the end you will write a `csv-file` to
the `data` directory of this repository (on your local disk, of course).

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
