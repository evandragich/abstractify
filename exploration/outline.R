library(colordistance)
library(tidyverse)

blockify <- function(X, my_dim, n){
  X_new <- rep(X, each = n)
  X_new = matrix(X_new, nrow = my_dim[1], ncol = my_dim[2]*n, byrow = T)
  ret_new = c()
  for(i in seq(my_dim[1])){
    ret_new <- append(ret_new, rep(X_new[i,], n))
  }

  mat <- matrix(ret_new, nrow = my_dim*n, ncol = my_dim[2]*n)
  return(mat)
}



boundary_func <- function(X, my_dim){
  if(my_dim[1] < 500){X = blockify(X, my_dim, 2)}
  n = dim(X)[1]
  m = dim(X)[2]
  ret = matrix(2, nrow = n, ncol = m)

  for(i in seq(2, n-1)){
    for(j in seq(2, m-1)){
      current_color = X[i, j]
      if((current_color != X[i, j - 1]) | (current_color != X[i, j + 1]) |
         (current_color != X[i + 1, j]) | (current_color != X[i - 1, j-1])){
        ret[i, j] = 1
      }
      else{ret[i,j] = 2}

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
  block_matrix <- matrix(cluster_vector, nrow = im_dim[1], ncol = im_dim[2])
  block_matrix <- t(apply(block_matrix, 2, rev))
  return(block_matrix)
}

plot_outline <- function(mat, my_dim){
  if(my_dim[1] < 500){
    alt_image <- image(z = mat, col = bw_colors)
  }
    else{
      alt_image <- image(z = boundary_func(mat, my_dim), col = bw_colors)
      }
}

rbg_outline <- function(mat){
  rgb_mat <- c(mat, mat, mat)
  return(rgb_mat)
}
