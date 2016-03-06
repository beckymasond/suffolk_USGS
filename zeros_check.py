import pandas as pd
import numpy as np

#read in data
cat_data = pd.read_csv('C:/Users/Becky/Documents/suffolk_USGS/cat4_data.csv', index_col=0)

#create pandas data frame
indicators = pd.DataFrame(cat_data)

#series with value True if variable values are all zeros
zeros = indicators.apply(lambda x: np.all(x==0))

#series containing variables with only zeros
empty = zeros[zeros == True]

#variable names containing only zeros 
empty_values = empty.index

print empty_values