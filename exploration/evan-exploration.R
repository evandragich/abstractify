library(tidyverse)
library(magick) # generalize beyond jpeg files
# magick will also help with flood filling later
# https://cran.r-project.org/web/packages/magick/vignettes/intro.html#Cut_and_edit
library(OpenImageR) # rotate function
library(colordistance) # eveyrthing else

# load filepaths; in future this could be input to function
#img <- here::here("data", "headshot-for-site.jpeg")
img <- here::here("data", "sample-image.jpeg")
#img <- here::here("data", "block-art.jpeg")

# not used for rest of code but fun to see the 3d colorspace map
colordistance::plotPixels(img, lower = NULL, upper = NULL)

# parametrize how many clusters are created in next step.
# my images looked best with 5
n_color <- 6

# turns out there was an implicit "sample size = 20000" argument! i was like how is the pixel
# count of a square image not a perfect square LOL. way slower now but still good!
my_colors <- colordistance::getKMeanColors(img, lower = NULL, upper = NULL,
                                           sample.size = FALSE, n = n_color)

# this is the "big boy" vector with all pixels and their mapping
raw_image_vector <- my_colors$cluster

# # grab rgb values of cluster centers to feed into image later
# cluster_centers <- my_colors$centers %>%
#   as_tibble() %>%
#   mutate(id = row_number(),
#          rgb_scaled = rgb(r, g, b)) %>%
#   select(rgb_scaled) %>%
#   pull()

# NEW Version OF THE ABOVE

cluster_lookup <- my_colors %>%
  extractClusters() %>%
  mutate(rgb_scaled = rgb(R, G, B),
         cluster = row_number()) %>%
  select(rgb_scaled) %>%
  pull()

# gets dimensions of image to help size matrix
# irrelevant right now with our square images, but will work in future i think
my_dim <- img %>%
  magick::image_read() %>%
  magick::image_info()

# create 3d image array (width * height * 3 (RGB) channels)
cluster_map <- tibble(cluster = raw_image_vector) %>%
  left_join(cluster_lookup)

image_array <- c(cluster_map$R, cluster_map$G, cluster_map$B) %>%
  array(c(my_dim$height, my_dim$width, 3))

img_yuh <- image_read(image_array)

# get quantified goodness of fit; unrelated to rest of analysis but could be fun to display
my_r_squared <- ((1 - (my_colors$tot.withinss / my_colors$totss)) * 100) %>%
  round(digits = 1)



# need to rotate here because image() has annoying behavior of transposing/reversing rows
my_matrix <- matrix(cluster_vector, nrow = my_dim$height, ncol = my_dim$width) %>%
  rotateFixed(90)

# combine it all together into our color-simplified raster image!
image(z = my_matrix, col = cluster_centers, axes=FALSE)

