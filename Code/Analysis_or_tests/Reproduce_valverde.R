library(ggplot2)
library(scales)
library(ggthemes)
library(dplyr)

# Read data
#data <- feather::read_feather("DataFiles/processed/patent_cat.feather")
data <- readr::read_csv("DataFiles/processed/patent_cat.csv")

# Summarise results
year_counts <- data %>% group_by(year) %>% summarise(count = n())
year_counts$year_posixct <- as.Date(as.character(year_counts$year), "%Y")

# (graph1 valverde)
ggplot(data = year_counts, aes(x = as.numeric(as.character(year)), y = count)) + 
  geom_point() 

# log-log cumsum of patents over time (inset of graph1 valverde)
year_counts$cumsum <- cumsum(year_counts$count)
ggplot(data = year_counts, aes(x = (year - 1975), y = cumsum)) + 
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  #stat_smooth(formula = y ~ x, method = "lm")

model <- lm(log10(cumsum) ~ log10(year - 1975), data = filter(year_counts, year <= 2000))

# Figure 3
fig3_data <- data %>% group_by(count,year) %>% summarise(N = n())
fig3_data2 <- filter(fig3_data, year %in% c(1984,1992,2015))

ggplot(data = fig3_data, aes(x = count, y = N, colour = as.character(year))) +
    geom_line() +
    scale_x_log10() +
    scale_y_log10()