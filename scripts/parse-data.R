library(tidyverse)

# this is the only class-provided dataset we downloaded online ourselves.
# here we write to csv for ease of use along with our other csv datasets in app.R
dog_travel <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv")

write_csv(dog_travel, "data/dog_travel_data.csv")
