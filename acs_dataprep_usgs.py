# Authors: Rebecca Davies & David Folch
# Date: 2/17/16
# Description: Script to download select American Community Survey data from the Census API
# Output: Produces four csv files for each run: variable estimates (ests.csv), MOEs (moes.csv), column names (columns.csv), and list of block groups containing zero households (empty.csv)

# Cenpy library: https://github.com/ljwolf/cenpy/tree/master/cenpy
import cenpy as cen
import pandas as pd
import numpy as np
import copy

local_path = '/Users/Becky/'
data_path = local_path + 'documents/suffolk_USGS/data/'

# Data source: 20010-2014 ACS 5-yr. Summary Files
conn = cen.base.Connection('ACSSF5Y2014')

# Download ACS data from API by specified variable category e.g. "housing_yearbuilt" or "all"
def get_data(scenario):
    # Housing Age variables
    if scenario == 'housing_yearbuilt':
        cols = ['B25034_001', 'B25034_010', 'B25034_009', 'B25034_008','B25034_007','B25034_006','B25034_005','B25034_004','B25034_003','B25034_002']
    # Housing Density variables
    elif scenario == 'housing_units':
        cols = ['B25024_001', 'B25024_002', 'B25024_003', 'B25024_004', 'B25024_005', 'B25024_006', 'B25024_007', 'B25024_008', 'B25024_009']
    # Race variables
    elif scenario == 'race':
        cols = ['B03002_001', 'B03002_003', 'B03002_004', 'B03002_012', 'B03002_006']
    # Family Structure variables
    elif scenario == 'household':
        cols = ['B11001_001', 'B11001_003', 'B11001_004', 'B11001_009', 'B11001_008'] 
    # Length in Residence variables
    elif scenario == 'occupancy':
        cols = ['B25026_001', 'B25026_003', 'B25026_010','B25026_004','B25026_011','B25026_005','B25026_012','B25026_006','B25026_007','B25026_008','B25026_013','B25026_014','B25026_015']
    # Seasonal Residency variables
    elif scenario == 'vacant':
        cols = ['B25004_001', 'B25004_006']
    #Home Values variables
    elif scenario == 'home_value':
        cols = ['B25075_001', 'B25075_002','B25075_003','B25075_004','B25075_005','B25075_006','B25075_007','B25075_008','B25075_009','B25075_010','B25075_011','B25075_012','B25075_013',
        'B25075_014','B25075_015','B25075_016','B25075_017','B25075_018','B25075_019','B25075_020','B25075_021','B25075_022','B25075_023','B25075_024','B25075_025']
    #all data at once 
    elif scenario == 'all':
        cols = ['B25034_001', 'B25034_010', 'B25034_009', 'B25034_008','B25034_007','B25034_006','B25034_005','B25034_004','B25034_003','B25034_002', 'B25024_001', 'B25024_002', 'B25024_003',
        'B25024_004', 'B25024_005', 'B25024_006', 'B25024_007', 'B25024_008', 'B25024_009', 'B03002_001', 'B03002_003', 'B03002_004', 'B03002_012', 'B03002_006', 'B11001_001', 'B11001_003', 
        'B11001_004', 'B11001_009', 'B11001_008','B25026_001', 'B25026_003', 'B25026_010','B25026_004','B25026_011','B25026_005','B25026_012','B25026_006','B25026_007','B25026_008','B25026_013',
        'B25026_014','B25026_015', 'B25004_001', 'B25004_006', 'B25075_001', 'B25075_002', 'B25075_003','B25075_004','B25075_005','B25075_006','B25075_007','B25075_008','B25075_009','B25075_010',
        'B25075_011','B25075_012','B25075_013', 'B25075_014','B25075_015','B25075_016','B25075_017','B25075_018','B25075_019','B25075_020','B25075_021','B25075_022','B25075_023','B25075_024','B25075_025']  
    # Name of output file
    base_file_name = data_path+'blockgroup'+'_'+scenario
    # Retrieve estimates and margins of error (MOEs)
    cols_ests = [i+'E' for i in cols]
    cols_moes = [i+'M' for i in cols]
    cols_all_data = cols_ests + cols_moes
    cols_all_data.append('B11001_001E')  # always pull total households to identify empty units
    cols_all_data = list(set(cols_all_data))  # get the unique column names
    cols_all = copy.copy(cols_all_data)
    cols_all.extend(['NAME', 'GEOID'])
    api_key = '19809d94f2ffead8a50814ec118dce7c5a987bfd'  # rebecca's census API key
    api_database = 'ACSSF5Y2014'  # ACS 2010-2014
    api_conn = cen.base.Connection(api_database)
    # pull column info from API
    cols_detail = api_conn.variables.ix[cols_all].label.to_dict()        
    cols_detail = pd.DataFrame.from_dict(cols_detail, orient='index')
    # pull the data from the API
    data = pd.DataFrame()
    # selects block group data from all of Suffolk County, NY
    data = conn.query(cols_all, geo_unit='block group:*', geo_filter = {'state':'36', 'county':'103'})
    # GEOID for each block group/row in dataframe serves as index
    index = 'g' + data.GEOID
    index = index.str.replace('15000US','')
    data.index = index
    data[cols_all_data] = data[cols_all_data].convert_objects(convert_numeric=True)        
    # organize output dataframes and add multiindex column headers
    output_ests = data[cols_ests]
    output_moes = data[cols_moes]
    #identify block groups without households
    empty_blocks = data[data.B11001_001E == 0]
    empty_blocks = empty_blocks.index.values
    #write results to hard drive
    output_ests.to_csv(base_file_name+'_ests.csv') 
    output_moes.to_csv(base_file_name+'_moes.csv') 
    cols_detail.to_csv(base_file_name+'_columns.csv', header=False)
    np.savetxt(base_file_name+'_empty.csv', empty_blocks, delimiter=',', fmt='%s')
    return data
