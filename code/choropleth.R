# choropleth - US Census data mapping
library(choroplethr)
# choropleth requires two columns in the dataframe to be named "region" and "value"

# State population
data(df_pop_state)
head(df_pop_state)
state_choropleth(df_pop_state)

# Per-capita income by State
data("df_state_demographics")
names(df_state_demographics)
head(df_state_demographics)
df_state_demographics$value <- df_state_demographics$per_capita_income
state_choropleth(df_state_demographics)

# homework: hispanic population by state
df_state_hispanic<-df_state_demographics[c("region", "percent_hispanic")]
names(df_state_hispanic)<-c("region", "value")
state_choropleth(df_state_hispanic,
                 title = "Hispanic Population by State",
                 num_colors = 1)

# map the population of each county
data(df_pop_county)
head(df_pop_county)
county_choropleth(df_pop_county,
                  title = "US Population by County",
                  num_colors = 9)
# Pinellas FIPS is 12103
# Mapo the per-capita income of each county
data("df_county_demographics")
names(df_county_demographics)
df_county_demographics$value<-df_county_demographics$per_capita_income
county_choropleth(df_county_demographics[, c("region", "value")],
                  title = "US Counties Per-Capita Income")

# homework: hispanic population by county
data("df_county_demographics")
names(df_county_demographics)
df_county_demographics$value<-df_county_demographics$percent_hispanic
county_choropleth(df_county_demographics[,c("region","value")],
                  title = "Percent Hispanic Population by US County",
                  num_colors = 9)

# Activate the ACS license
library(acs)
api.key.install("f15bd283e04b92759deae9a259110aac5dda8913")

# ACS data: Map the Population of US States in 2010
df_2010_demographics<-get_state_demographics(2010)
head(df_2010_demographics)

df_2010_demographics$value<-df_2010_demographics$total_population
state_choropleth(df_2010_demographics,
                 title = "2010 StatePopulation Estimates",
                 legend = "Population",
                 num_colors = 9)

# Percent of Hispanics in Texas Counties in 2011
df_2011_Counties<-get_county_demographics(2011)
head(df_2011_Counties)
summary(df_2011_Counties)

df_2011_Counties$value<-df_2011_Counties$percent_hispanic
county_choropleth(df_2011_Counties)
# TX
df_2011_TX <- df_2011_Counties[df_2011_Counties$region >= 48000 & df_2011_Counties$region < 49000, ]
summary(df_2011_TX)
# FL
df_2011_FL <- df_2011_Counties[df_2011_Counties$region >= 12000 & df_2011_Counties$region < 13000, ]

# Texas Map
df_2011_TX$value<-df_2011_TX$percent_hispanic
county_choropleth(df_2011_TX,
                  title = "Percent Hispanics in 2011 Texas Counties",
                  legend = "% Hispanic",
                  state_zoom = "texas",
                  num_colors = 9)

# Florida map
df_2011_FL$value<-df_2011_FL$percent_hispanic
county_choropleth(df_2011_FL,
                  title = "Percentage of Hispanics in Florida Counties",
                  legend = "% Hispanics:",
                  state_zoom = "florida",
                  num_colors = 1)