# HELPER FUNCTIONS FOR DATA CLEANING

import re
import numpy as np
from helper.currency_exchange import exchange_rates


# find all numbers in a string and compare them, and keep the highest one

def find_highest_value(string):

    # replace nan with empty string
    if type(string) == float:
        string = ""
   
    string=re.sub(" ", "", string)
    string=re.sub(",", ".", string)
    string=re.findall('(\d{4,})', string)

    list=[]

    if string:
        for s in string:
            list.append(int(s))
        return max(list)
    else:
        return np.nan
    
# for all numbers in column cpv, extract the number and save in a list in a column.

def extract_cpv(string):
    if type(string) == float:
        return np.nan
    else:
        string = re.findall('(\d+)', string)
        return string

# convert values to euro using currency exchange dictionary

def convert_to_euro(x):
    if x["currency"] == "EUR":
        return np.format_float_positional(x["value_hi"], trim='-')
    else:
        try:
            return np.format_float_positional(round(x["value_hi"] / exchange_rates[x["year"]][x["currency"]], 1), trim='-')
        except:
            return np.nan
