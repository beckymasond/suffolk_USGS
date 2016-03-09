#Author: Becky Davies
#Date: March 6, 2016

#Purpose: To check input csvs for columns in which all data values are 0

#Input: CSV with variables as column headers and geographic units as index

#Output: List of names for columns in which all data values are 0

import pandas as pd
import numpy as np

#read in data
cat_data = pd.read_csv('C:/Users/Becky/Documents/suffolk_USGS/cat4_data.csv', index_col=0)

#create pandas data frame
indicators = pd.DataFrame(cat_data)

#create series with value True for columns in which all values are 0
zeros = indicators.apply(lambda x: np.all(x==0))
zeros2 = indicators.apply(lambda x: np.all(x=='0%'))

#create series containing variables with only zero values
empty = zeros[zeros == True]
empty2 = zeros2[zeros2 == True]

#create list of variable names containing only zeros 
empty_values = empty.index.tolist()
empty2 = empty2.index.tolist()
empty_values.extend(empty2)

print empty_values
