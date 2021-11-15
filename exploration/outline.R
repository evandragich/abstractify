library(colordistance)
# library(OpenImage)
library(tidyverse)

boundary_func <- function(X, my_dim){
  X = matrix(X, nrow = my_dim[1], ncol = my_dim[2])
  n = dim(X)[1]
  m = dim(X)[2]
  ret = matrix(0, nrow = n, ncol = m)

  for(i in seq(n)){
    for(j in seq(m)){
      current_color = X[i, j]
      if(i != n & j != m & i != 1 & j != 1){
        if((current_color != X[i, j - 1]) | (current_color != X[i, j + 1]) |
           (current_color != X[i + 1, j]) | (current_color != X[i - 1, j-1])){
          ret[i, j] = 1
        }
        else{ret[i,j] = 2}
      }

    }
  }

  return(ret)
}

bw_colors <- tibble(r = c(0,1), g = c(0,1), b = c(0,1)) %>%
  mutate(id = row_number(),
         rgb_scaled = rgb(r, g, b)) %>%
  select(rgb_scaled) %>%
  pull()

outline_func <- function(cluster_vector, im_dim) {
  # does not return anything, simply plots the image -- maybe change this
  block_matrix <- matrix(cluster_vector, nrow = im_dim[1], ncol = im_dim[2])
  # %>% rotateFixed(90)
  alt_image <- image(z = boundary_func(block_matrix, im_dim), col = bw_colors)
}