library(tidyverse)
library(colordistance)

img <- here::here("headshot-for-site.jpeg")

colordistance::plotPixels(img, lower = NULL, upper = NULL)

my_colors <- colordistance::getKMeanColors(img, lower = NULL, upper = NULL)


clustervector <- my_colors$cluster

cluster_centers <- my_colors$centers

my_r_squared <- my_colors$betweenss / (my_colors$betweenss + my_colors$withinss)