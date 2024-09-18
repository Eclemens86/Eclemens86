# Import modules
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import geopandas as gpd
from census import Census
from us import states
import os


# Set API key
c = Census("4a6312d77db109879039890c413e71c845d9c3ee")

ia_census = c.acs5.state_county_tract(fields = ('NAME', 'C17002_001E', 'C17002_002E', 'C17002_003E', 'B01003_001E'),
                                      state_fips = states.IA.fips,
                                      county_fips = "*",
                                      tract = "*",
                                      year = 2022)

# Create a dataframe from the census data
ia_df = pd.DataFrame(ia_census)

# Show the dataframe
print(ia_df.head(2))
print('Shape: ', ia_df.shape)

# Access shapefile of Virginia census tracts
ia_tract = gpd.read_file("https://www2.census.gov/geo/tiger/TIGER2019/TRACT/tl_2019_51_tract.zip")

# Reproject shapefile to UTM Zone 17N
# https://spatialreference.org/ref/epsg/wgs-84-utm-zone-17n/
ia_tract = ia_tract.to_crs(epsg = 32617)

# Print GeoDataFrame of shapefile
print(ia_tract.head(2))
print('Shape: ', ia_tract.shape)

# Check shapefile projection
print("\nThe shapefile projection is: {}".format(ia_tract.crs))

# Combine state, county, and tract columns together to create a new string and assign to new column
ia_df["GEOID"] = ia_df["state"] + ia_df["county"] + ia_df["tract"]

# Print head of dataframe
ia_df.head(2)

# Remove columns
ia_df = ia_df.drop(columns = ["state", "county", "tract"])

# Show updated dataframe
ia_df.head(2)

# Check column data types for census data
print("Column data types for census data:\n{}".format(ia_df.dtypes))

# Check column data types for census shapefile
print("\nColumn data types for census shapefile:\n{}".format(ia_tract.dtypes))

# Join the attributes of the dataframes together
ia_merge = ia_tract.merge(ia_df, on = "GEOID")

# Show result
print(ia_merge.head(2))
print('Shape: ', ia_merge.shape)

# Create new dataframe from select columns
ia_poverty_tract = ia_merge[["STATEFP", "COUNTYFP", "TRACTCE", "GEOID", "geometry", "C17002_001E", "C17002_002E", "C17002_003E", "B01003_001E"]]

# Show dataframe
print(ia_poverty_tract.head(2))
print('Shape: ',ia_poverty_tract.shape)

# Dissolve and group the census tracts within each county and aggregate all the values together
ia_poverty_county = ia_poverty_tract.dissolve(by = 'COUNTYFP', aggfunc = 'sum')

# Show dataframe
print(ia_poverty_county.head(2))
print('Shape: ', ia_poverty_county.shape)

# Get poverty rate and store values in new column
ia_poverty_county["Poverty_Rate"] = (ia_poverty_county["C17002_002E"] + ia_poverty_county["C17002_003E"]) / ia_poverty_county["B01003_001E"] * 100

# Show dataframe
ia_poverty_county.head(2)

# Create subplots
fig, ax = plt.subplots(1, 1, figsize = (20, 10))

# Plot data
ia_poverty_county.plot(column = "Poverty_Rate",
                       ax = ax,
                       cmap = "RdPu",
                       legend = True)

# Stylize plots
plt.style.use('bmh')

# Set title
ax.set_title('Poverty Rates (%) in Iowa', fontdict = {'fontsize': '25', 'fontweight' : '3'})