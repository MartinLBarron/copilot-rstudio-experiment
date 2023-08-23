library(tidyverse)

# Read crime data from city of chicago website
#crime <- read_csv("https://data.cityofchicago.org/api/views/ijzp-q8t2/rows.csv?accessType=DOWNLOAD")

# Explore crime data
str(crime)

library(simplefreqs)
freq(crime$`Primary Type`)
length(levels(factor(crime$`Primary Type`)))

# recode `Primary Type`
df <- crime |> 
  rename (primary_type = `Primary Type`) |>
  mutate(primary_type = fct_lump_prop(primary_type, .005)) |> 
  mutate(primary_type = fct_other(primary_type, drop=c('Other', "OTHER OFFENSE"), other_level="OTHER"))

freq(df$primary_type)

# Recode Date to data variable
df <- df |> 
  mutate(date = as.Date(Date, format = "%m/%d/%Y"))

# Plot crime by date
df |>
  ggplot(aes(x=date)) +
  geom_freqpoly(aes(color=primary_type), binwidth=30) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Prepare data for plotting
df <- df |> 
  mutate(month=month(date), year=year(date)) |>
  group_by(year, month, primary_type) |> 
  summarise(crime_count=n()) |> 
  mutate(date=as.Date(paste(year, month, "01", sep="-"))) |> 
  ungroup()

# Plot crime by date
library(gghighlight)
df |> 
  ggplot(aes(x=date, y=crime_count)) +
  geom_line(aes(color=primary_type)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  gghighlight(max(crime_count) > 1000)


df |> 
  ggplot(aes(x=date, y=crime_count)) +
  geom_line(aes(color=primary_type)) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  gghighlight(primary_type == "NARCOTICS")

