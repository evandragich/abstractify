library(tidyverse)

dog_travel <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv")

write_csv(dog_travel, "data/dog_travel_data.csv")
