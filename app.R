
library(shiny)
library(tidyverse)
library(reactable) # color table
library(reactablefmtr) # conditional colors in cells
library(magick) # generalize beyond jpeg files
# magick will also help with flood filling later
# https://cran.r-project.org/web/packages/magick/vignettes/intro.html#Cut_and_edit
library(colordistance) # eveyrthing else

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
      imageOutput("pixelated_img"),
      plotOutput("colorspace_plot"),
      reactableOutput("color_table"),
      textOutput("r_squared")
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
  dim <- reactive(
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
          dim = c(dim()$height, dim()$width, 3)
        )
      }
  )


  output$pixelated_img <- renderImage(
    {
      ret <- pxl_img_array() %>%
        image_read() %>%
        image_write(tempfile(fileext = dim()$format), format = dim()$format)

      list(src = ret, contentType = paste0("image/", dim()$format))
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
}

# Run the application
shinyApp(ui = ui, server = server)
