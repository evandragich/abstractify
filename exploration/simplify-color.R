library(tidyverse)
library(colordistance)

get_clusters <- function(colors){
  ret <- colors %>%
    extractClusters() %>%
    mutate(
      rgb_scaled = rgb(R, G, B),
      cluster = row_number()
    )

  return(ret)
}


get_rsquared <- function(colors){
  value <- ((1 - (colors()$tot.withinss / colors()$totss)) * 100) %>%
    round(digits = 1)
  return(value)
}


pxl_array <- function(colors, clusters, dim){
  pxl_img_array <- tibble(cluster = colors$cluster) %>%
      left_join(clusters) %>%
      {
        array(
          data = c(.$R, .$G, .$B),
          dim = c(dim()$height, dim()$width, 3)
        )
      }
  return(pxl_img_array)
}

color_table <- function(clusters) {
  table <- clusters %>%
  arrange(desc(Pct)) %>%
  mutate(
    Pct = paste0(round(Pct * 100, digits = 1), "%"),
    placeholder = NA
    ) %>%
  select(rgb_scaled, Pct, placeholder) %>%
  reactable(
    columns = list(
      Pct = colDef(name = "Percentage of pixels"),
      placeholder = colDef(
        style = function(value, index) {
          color <- cluster_lookup()$rgb_scaled[index]
          list(background = color)
            }
          )
        ),
        rownames = FALSE,
      )
  return(table)
}
