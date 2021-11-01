library(colordistance)
library(OpenImage)

boundary_func <- function(X){

  n = dim(X)[1]
  m = dim(X)[2]
  ret = matrix(0, nrow = n, ncol = m)

  for(i in range(n)){
    for(j in range(m)){
      current_color = X[i, j]
      if(current_color != X[i, j - 1] | current_color != X[i, j + 1] |
         current_color != X[i + 1, j] | current_color != X[i - 1, j-1]){
        ret[i, j] = c(0, 0, 0)
      }
      else{ret[i,j] = c(255, 255, 255)}
    }
  }

  return(ret)
}



image <- "~/project-02/block-art.jpeg"
colordistance::plotPixels("~/project-02/block-art.jpeg", lower = rep(0.8, 3), upper = rep(1, 3))
colordistance::getImageHist(image, bins = 2, lower = NULL, upper = NULL, plotting = TRUE)
lower <- NULL
upper <- NULL
kmeans01 <- colordistance::getKMeanColors(image, lower = lower, upper = upper, n = 6)
kmeans01$centers
