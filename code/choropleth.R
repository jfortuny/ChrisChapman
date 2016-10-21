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
state_choropleth(df_state_hispanic)
