
library(shiny)
library(tidyverse)
library(scales) # label_percent on degree base plot
library(reactable) # color table
library(reactablefmtr) # conditional colors in cells
library(magick) # generalize beyond jpeg files
# magick will also help with flood filling later
library(palmerpenguins) # penguins dataset for sample plot
library(colorblindr) # okabe-ito palette for sample plots

# data source for degrees data:
# https://wilkelab.org/SDS375/datasets/BA_degrees.csv
# sample plots inspired by HW4 Q1


# https://cran.r-project.org/web/packages/magick/vignettes/intro.html#Cut_and_edit
library(colordistance) # everything else

source("exploration/outline.R")

# load data for sample plots
degrees <- read_csv(here::here("data", "BA_degrees.csv"))


# Define UI
ui <- fluidPage(

  # Application title
  titlePanel(
    h1("PBN-ify",
    h4("Click \"Browse...\" to replace the default image with one of your own"))
  ),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("clusters",
        "Number of colors:\n(Most images look best with 3-5!)",
        min = 1,
        max = 10,
        value = 5
      ),
      fileInput("upload",
        "Upload an image:",
        accept = "image/*",
        placeholder = "No image selected"
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Home",
          imageOutput("pixelated_img", height = "200px"),
          plotOutput("colorspace_plot"),
          reactableOutput("color_table"),
          textOutput("r_squared")
          ),
        tabPanel(
          title = "Simplified Output"
        ),
        tabPanel(
          title = "Outline",
          plotOutput("outline")
        ),
        tabPanel(
          title = "Example plots",
          textOutput("example_plot_description"),
          radioButtons("example_type",
                      "Choose Plot Type:",
                      choices = c("Discrete", "Sequential", "Diverging"),
                      selected = "Discrete"
          ),
          sliderInput("gray_val",
                      "Adjust the darkness of the \"Other\" Category",
                      min = 10,
                      max = 90,
                      value = 70,
                      step = 5
          ),
          plotOutput("basic_plot"),
          plotOutput("colorized_plot"),
          #plotOutput("viridis_plot"),
          #plotOutput("okabeito_plot"),
        )

    )
  )
)
)

# Define server logic
server <- function(input, output) {


  # prevents errors when app is first started
  my_path <- reactive(
    if (is.null(input$upload)) {
      here::here("data", "sample-image.jpeg")
    } else {
      input$upload$datapath
    }
  )



  # turns out there was an implicit "sample size = 20000" argument! i was like how is the pixel
  # count of a square image not a perfect square LOL. way slower now but still good!
  my_colors <- reactive(
    colordistance::getKMeanColors(my_path(),
      lower = NULL, upper = NULL,
      sample.size = FALSE, n = input$clusters
    )
  )

  # grab rgb values of cluster centers to feed into image later
  # this is a df with obs for every cluster, cols RGB vals and % of image
  cluster_lookup <- reactive(
    my_colors() %>%
      extractClusters() %>%
      mutate(
        rgb_scaled = rgb(R, G, B),
        cluster = row_number()
      )
  )

  # get quantified goodness of fit; unrelated to rest of analysis but could be fun to display
  output$r_squared <- renderText({
    value <- ((1 - (my_colors()$tot.withinss / my_colors()$totss)) * 100) %>%
      round(digits = 1)
    paste0("The goodness of fit, or R-Squared, associated with this pixelation is ", value, "%")
  })


  # obtains dimensions of original image to size first 2d of 3d array
  my_dim <- reactive(
    my_path() %>%
      image_read() %>%
      image_info()
  )

  # parses the cluster vector outputted from getKmeanColors() with
  # cluster lookup table to create 3d array of pixelated image!

  pxl_img_array <- reactive(
    tibble(cluster = my_colors()$cluster) %>%
      left_join(cluster_lookup()) %>%
      {
        array(
          data = c(.$R, .$G, .$B),
          dim = c(my_dim()$height, my_dim()$width, 3)
        )
      }
  )


  output$pixelated_img <- renderImage(
    {
      ret <- pxl_img_array() %>%
        image_read() %>%
        image_write(tempfile(fileext = my_dim()$format), format = my_dim()$format)

      list(src = ret, contentType = paste0("image/", my_dim()$format), height = "200px")
    },
    deleteFile = FALSE
  )

  output$colorspace_plot <- renderPlot({
    colordistance::plotPixels(my_path(),
      lower = NULL, upper = NULL,
      main = "Colorspace Plot"
    )
  })

  output$color_table <- renderReactable({
    cluster_lookup() %>%
      arrange(desc(Pct)) %>%
      mutate(
        Pct = paste0(round(Pct * 100, digits = 1), "%"),
        placeholder = NA
      ) %>%
      select(rgb_scaled, Pct, placeholder) %>%
      reactable(
        columns = list(
          rgb_scaled = colDef(name = "Hex Code"),
          Pct = colDef(name = "Percentage of pixels"),
          placeholder = colDef(
            name = "Color",
            style = function(value, index) {
              color <- cluster_lookup()$rgb_scaled[index]
              list(background = color)
            }
          )
        ),
        rownames = FALSE,
      )
  })

  # description to explain example plot tab
  output$example_plot_description <- renderText({
    "On this page, you can test out the color palette generated from your image in-use in `ggplot2()`."
  })

  # create base plot to be layered upon
  # only reactive change is number of non-lumped factors
  base_plot <- reactive(
    degrees %>%
    mutate(field = fct_lump_n(field,
                              n = input$clusters,
                              ties.method = "first")) %>%
    group_by(field, year) %>%
    summarise(perc = sum(perc), .groups = "drop_last") %>%
    ggplot(mapping = aes(x = year, y = perc, group = field, color = field)) +
    geom_line() +
    labs(
      title = "Percentage of annual degrees awarded",
      subtitle = "from 1971 to 2015 in the US",
      x = "Year",
      y = "Percent",
      color = "Field"
    ) +
    scale_y_continuous(labels = label_percent())
  )

## EVAN TODO:
  # parametrize all but color palette for basic discrete plot
  # add okabe ito and viridis example plots
  # create sample plot for diverging
  # create UI to choose color for diverging


# output the base ggplot for discrete color palettes
    output$basic_plot <- renderPlot({
      base_plot() +
        scale_color_manual(
          values = c(hue_pal()(input$clusters), paste0("gray", (100 - input$gray_val))),
        ) +
        theme_minimal() +
        theme(legend.position = "top")
  })

# output the ggplot with our colors for discrete color palettes
output$colorized_plot <- renderPlot({
  base_plot() +
    scale_color_manual(
      values = c(cluster_lookup()$rgb_scaled, paste0("gray", (100 - input$gray_val))),
    ) +
    theme_minimal() +
    theme(legend.position = "top")

})

  output$outline <- renderPlot({
    outline_func(my_colors()$cluster, c(my_dim()$height, my_dim()$width))

})

}

# Run the application
shinyApp(ui = ui, server = server)
