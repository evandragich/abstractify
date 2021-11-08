#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(magick) # generalize beyond jpeg files
# magick will also help with flood filling later
# https://cran.r-project.org/web/packages/magick/vignettes/intro.html#Cut_and_edit
library(OpenImageR) # rotate function
library(colordistance) # eveyrthing else

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("PBN-ify"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("clusters",
                        "Number of colors:\n(Most images look best with 3-5!)",
                        min = 1,
                        max = 10,
                        value = 5),
            fileInput("upload",
                      "Upload an image:",
                      accept = "image/*",
                      placeholder = "No image selected")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           # plotOutput("distPlot")
            imageOutput("pixelated_img"),
            plotOutput("colorspace_plot"),
            textOutput("r_squared")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })

    # from https://gist.github.com/jeroen/bd1d0a30e7184a5320695ee2bda12c70
    # observeEvent(input$upload, {
    #
    #     image_path <- input$upload$datapath
    #      image <- image_read(image_path)
    #     image_info <- image_info(image)
    # })



    # turns out there was an implicit "sample size = 20000" argument! i was like how is the pixel
    # count of a square image not a perfect square LOL. way slower now but still good!
    my_colors <- reactive(
        colordistance::getKMeanColors(input$upload$datapath, lower = NULL, upper = NULL,
                                      sample.size = FALSE, n = input$clusters)
    )

    # # this is the "big boy" vector with all pixels and their mapping
    # cluster_vector <- reactive(
    #     my_colors$cluster
    # )

    # grab rgb values of cluster centers to feed into image later
    clustered_colors <- reactive(
        my_colors()$centers %>%
            as_tibble() %>%
            mutate(id = row_number(),
                   rgb_scaled = rgb(r, g, b)) %>%
            select(rgb_scaled) %>%
            pull()
    )

    # get quantified goodness of fit; unrelated to rest of analysis but could be fun to display
    output$r_squared <- renderText({
        value <- ((1 - (my_colors()$tot.withinss / my_colors()$totss)) * 100) %>%
            round(digits = 1)
        paste0("The goodness of fit, or R-Squared, associated with this pixelation is ", value, "%")
    })

    # gets dimensions of image to help size matrix
    # irrelevant right now with our square images, but will work in future i think
    # metadata <- img %>%
    #     magick::image_read() %>%
    #     magick::image_info()

    # metadata <- reactive(
    #     img_path() %>%
    #         magick::image_read() %>%
    #         magick::image_info()
    # )

    dim <- reactive(
        input$upload$datapath %>%
            image_read() %>%
            image_info()
    )

    # need to rotate here because image() has annoying behavior of transposing/reversing rows
    img_matrix <- reactive(
        matrix(my_colors()$cluster, nrow = dim()$height, ncol = dim()$width) %>%
            rotateFixed(90)
    )


    output$pixelated_img <- renderImage({
        # combine it all together into our color-simplified raster image!
        ret <- image(z = img_matrix(), col = clustered_colors(), axes=FALSE) %>%
            as.raster() %>%
            image_read() %>%
            image_write(tempfile(fileext = 'jpeg'), format = 'jpeg')


        list(src = ret, contentType = "image/jpeg")
    })

    output$colorspace_plot <- renderPlot({
        colordistance::plotPixels(input$upload$datapath, lower = NULL, upper = NULL)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
