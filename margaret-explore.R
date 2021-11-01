library(colordistance)
library(OpenImage)

boundary_func <- function(X, dim){

  X = matrix(X, nrow = dim[1], ncol = dim[2])
  n = dim(X)[1]
  m = dim(X)[2]
  ret = matrix(0, nrow = n, ncol = m)

  for(i in seq(n)){
    for(j in seq(m)){
      current_color = X[i, j]
      # print(current_color)
      # print(X[i, j - 1])
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

image <- "~/project-02/data/block-art.jpeg"

block_colors <- colordistance::getKMeanColors(image, lower = NULL, upper = NULL, sample.size = FALSE, n = 6)

cluster_vector_b <- block_colors$cluster

# grab rgb values of cluster centers to feed into image later
cluster_centers_b <- block_colors$centers %>%
  as_tibble() %>%
  mutate(id = row_number(),
         rgb_scaled = rgb(r, g, b)) %>%
  select(rgb_scaled) %>%
  pull()

# gets dimensions of image to help size matrix
# irrelevant right now with our square images, but will work in future i think
block_dim <- image %>%
  jpeg::readJPEG() %>%
  dim()

# need to rotate here because image() has annoying behavior of transposing/reversing rows
block_matrix <- matrix(cluster_vector_b, nrow = block_dim[1], ncol = block_dim[2]) %>%
  rotateFixed(90)
block_image <- image(z = block_matrix, col = cluster_centers_b)

alt_image <- image(z = boundary_func(block_matrix, block_dim), col = bw_colors)
