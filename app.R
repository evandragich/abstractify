
library(shiny)
library(tidyverse)
library(scales) # label_percent on degree base plot
library(reactable) # color table
library(magick) # generalize beyond jpeg files
# magick will also help with flood filling later
library(palmerpenguins) # penguins dataset for sample plot
library(colorblindr) # okabe-ito palette for sample plots

# data source for degrees data:
# https://wilkelab.org/SDS375/datasets/BA_degrees.csv
# source for dogs data: HW3 Q5
# "state-list.csv: Created using state.abb and state.name"
# dog_travel: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-12-17
# sample plots inspired by HW4 Q1
# source for color names:
# https://en.wikipedia.org/wiki/Lists_of_colors


# https://cran.r-project.org/web/packages/magick/vignettes/intro.html#Cut_and_edit
library(colordistance) # everything else

source("exploration/outline.R")

# load data for sample plots
degrees <- read_csv(here::here("data", "BA_degrees.csv")) %>%
  filter(year >= 1990)

dog_travel <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv")
state_list <- tibble(abbreviation = state.abb, name = state.name)

# load color name data
color_names <- read_csv(here::here("data", "color_names.csv"))


# vector of fields in desc frequency of avg over years; helpful for fct_other() later
ordered_fields <- degrees %>%
  group_by(field) %>%
  summarise(avg = mean(perc)) %>%
  arrange(desc(avg)) %>%
  select(field) %>%
  pull()


# Define UI
ui <- fluidPage(

  # Application title
  titlePanel(
    h1(
      "PBN-ify",
      h4("Click \"Browse...\" to replace the default image with one of your own")
    )
  ),
  tabsetPanel(
    tabPanel(
      title = "PBN",
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
          ),
          p("Or"),
          selectInput(
            inputId = "defaultImage",
            label = "Choose an Image",
            choices = c(
              "Marge Simpson" = "marge-simpson.jpeg",
              "Margaret Picture" = "MargaretReed.jpeg",
              "Evan Picture" = "sample-image.jpeg",
              "Block Art" = "block-art.jpeg"
            ),
            selected = "sample-image.jpeg",
            multiple = FALSE
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel(
              title = "Home",
              h2("Welcome to PBNify!"),
              p("How to use: select one our preset images or upload one of your
                own, and choose a k value. You can explore the various tabs to
                see the simplified version of your image and the outline
                version. You can also see some sample color palettes to use in
                ggplot!"),
              p("Your image:"),
              imageOutput("original_img")
            ),
            tabPanel(
              title = "Simplified Output",
              imageOutput("pixelated_img", height = "200px"),
              downloadButton("download_pxl", "Download modified image")
            ),
            tabPanel(
              title = "Outline",
              plotOutput("outline")
            )
          )
        )
      )
    ),
    tabPanel(
      title = "Color Palette",
      tabsetPanel(
        tabPanel(
          title = "Color Info",
          plotOutput("colorspace_plot"),
          reactableOutput("color_table"),
          textOutput("r_squared")
        ),
        tabPanel(
          title = "Plotting",
          textOutput("example_plot_description"),
               radioButtons("example_type",
                            "Choose Plot Type:",
                            choices = c("Discrete", "Sequential", "Diverging"),
                            selected = "Discrete"
               ),
               sliderInput("gray_val",
                           "Adjust the darkness of the \"Other\" or NA Category",
                           min = 10,
                           max = 90,
                           value = 30,
                           step = 5
               ),
               h2("PBN-ified Plot"),
               plotOutput("colorized_plot"),
               h2("Base color Plot"),
               plotOutput("basic_plot"),
               h2("Viridis Plot"),
               plotOutput("viridis_plot"),
               h2("Okabe-Ito Plot"),
               plotOutput("okabeito_plot")
      ))
      )

)
)

# Define server logic
server <- function(input, output) {


  # prevents errors when app is first started
  my_path <- reactive(
    if (is.null(input$upload)) {
      here::here("data", input$defaultImage)
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

  # function to lookup closest hex code from the named dictionary for any given rgb
  find_closest <- function(my_r, my_g, my_b) {
    temp <- color_names %>%
      mutate(diff = abs(R - my_r) + abs(G - my_g) + abs(B - my_b)) %>%
      arrange(diff) %>%
      select(name) %>%
      pull()

    temp[1]
  }

  # grab rgb values of cluster centers to feed into image later
  # this is a df with obs for every cluster, cols RGB vals and % of image
  # also has the closest named hex code given from above function
  cluster_lookup <- reactive(
    my_colors() %>%
      extractClusters() %>%
      mutate(
        rgb_scaled = rgb(R, G, B),
        cluster = row_number()
      ) %>%
      rowwise() %>%
      mutate(closest = find_closest(R, G, B))
  )

  # vector of ordered hex codes to find lightest/darkest for continuous plots
  ordered_hexes <- reactive(
    cluster_lookup() %>%
      mutate(avg = sum(R, G, B)) %>%
      arrange(avg) %>%
      select(rgb_scaled) %>%
      pull()
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


  output$original_img <- renderImage(
    {
      list(src = my_path(), height = "200px")
    },
    # saves image after sending to UI
    deleteFile = FALSE
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

  # write image object. needs outside of pixelated_img for use when downloading
  ret <- reactive(
    pxl_img_array() %>%
      image_read() %>%
      image_write(tempfile(fileext = my_dim()$format), format = my_dim()$format)
  )


  output$pixelated_img <- renderImage(
    {
      list(src = ret(), contentType = paste0("image/", my_dim()$format), height = "200px")
    },
    # saves image after sending to UI
    deleteFile = FALSE
  )

  output$colorspace_plot <- renderPlot({
    colordistance::plotPixels(my_path(),
      lower = NULL, upper = NULL,
      main = "Colorspace Plot"
    )
  })

  find_closest <- function(my_r, my_g, my_b) {
    temp <- color_names %>%
      mutate(diff = (R - my_r)^2 + (G - my_g)^2 + (B - my_b)^2) %>%
      arrange(diff) %>%
      select(name) %>%
      pull()

    temp[1]
  }

  hex_to_bw <- function(hex) {
    temp <- col2rgb(hex)[1, 1]^2 * .241 +
      col2rgb(hex)[2, 1]^2 * .691 +
      col2rgb(hex)[3, 1]^2 * .068

    ret <- if_else(temp > 16900, "#000000", "#FFFFFF")
    ret
  }

  output$color_table <- renderReactable({
    temp <- cluster_lookup() %>%
      # works now, but minimal matching. will work on matching closest neighbor
      # left_join(color_names, by = "rgb_scaled") %>%
      arrange(desc(Pct)) %>%
      mutate(
        Pct = paste0(round(Pct * 100, digits = 1), "%")
      ) %>%
      select(rgb_scaled, Pct, closest)

    reactable(temp,
      columns = list(
        Pct = colDef(name = "Percentage of pixels"),
        rgb_scaled = colDef(
          name = "Hex Code",
          style = function(value) {
            list(
              color = hex_to_bw(value),
              background = value
            )
          }
        ),
        closest = colDef(name = "Nearest named color")
      ),
      rownames = FALSE
    )
  })

  # description to explain example plot tab
  output$example_plot_description <- renderText({
    "On this page, you can test out the color palette generated from your image in-use in `ggplot2()`."
  })

  # create base discrete plot to be layered upon
  # only reactive change is number of non-lumped factors
  discrete_plot <- reactive(
    degrees %>%
      arrange(desc(perc)) %>%
      mutate(field = fct_other(field,
        keep = ordered_fields[1:input$clusters]
      )) %>%
      group_by(field, year) %>%
      summarise(perc = sum(perc), .groups = "drop_last") %>%
      ggplot(mapping = aes(x = year, y = perc, group = field, color = field)) +
      geom_line() +
      labs(
        title = "Percentage of annual degrees awarded",
        subtitle = "from 1990 to 2015 in the US",
        x = "Year",
        y = "Percent",
        color = "Field"
      ) +
      scale_y_continuous(labels = label_percent()) +
      theme_minimal() +
      theme(legend.position = "top")
  )

  # creates sequential base plot which is NOT reactive
  sequential_plot <- dog_travel %>%
    group_by(contact_state) %>%
    summarise(n = n()) %>%
    right_join(state_list, by = c("contact_state" = "abbreviation")) %>%
    mutate(name = str_to_lower(name)) %>%
    left_join(map_data("state"), dogs, by = c("name" = "region")) %>%
    ggplot(mapping = aes(x = long, y = lat, group = group, fill = n)) +
    geom_polygon(color = "black") +
    labs(
      title = "Number of Dogs Available to Adopt in U.S. States",
      caption = "Gray represents states with missing or unknown data.",
      fill = "Dogs"
    ) +
    theme_void() +
    theme(plot.title = element_text(size = 18, hjust = 0.5))

  ## EVAN TODO:
  # create sample plot for diverging
  # create UI to choose color for diverging


  # output the base ggplot for discrete color palettes
  output$basic_plot <- renderPlot({
    if (input$example_type == "Discrete") {
      discrete_plot() +
        scale_color_manual(
          values = c(hue_pal()(input$clusters), paste0("gray", (100 - input$gray_val))),
        )
    } else if (input$example_type == "Sequential") {
      sequential_plot +
        scale_fill_gradient(
          trans = "log10",
          na.value = paste0("gray", (100 - input$gray_val))
        )
    }
  })

  # output the ggplot with our colors for discrete color palettes
  output$colorized_plot <- renderPlot({
    if (input$example_type == "Discrete") {
      discrete_plot() +
        scale_color_manual(
          values = c(cluster_lookup()$rgb_scaled, paste0("gray", (100 - input$gray_val))),
        )
    } else if (input$example_type == "Sequential") {
      sequential_plot +
        scale_fill_gradient(
          trans = "log10",
          low = ordered_hexes()[1],
          high = ordered_hexes()[input$clusters],
          na.value = paste0("gray", (100 - input$gray_val))
        )
    }
  })

  # output the ggplot with okabe ito colors for discrete color palettes
  # fyi -- OkabeIto can only take 7 non-gray values, so will need guard
  output$okabeito_plot <- renderPlot({
    if (input$example_type == "Discrete") {
      discrete_plot() +
        scale_color_manual(
          values = c(palette_OkabeIto[1:input$clusters], paste0("gray", (100 - input$gray_val))),
        )
    } else if (input$example_type == "Sequential") {
      NULL
    }
  })

  # output the ggplot with viridis colors for discrete color palettes
  output$viridis_plot <- renderPlot({
    if (input$example_type == "Discrete") {
      discrete_plot() +
        scale_color_manual(
          values = c(viridis_pal()(input$clusters), paste0("gray", (100 - input$gray_val))),
        )
    } else if (input$example_type == "Sequential") {
      sequential_plot +
        scale_fill_viridis_c(
          trans = "log10",
          option = "A",
          na.value = paste0("gray", (100 - input$gray_val))
        )
    }
  })

  output$outline <- renderPlot({
    outline_func(my_colors()$cluster, c(my_dim()$height, my_dim()$width))
  })

  output$download_pxl <- downloadHandler(
    filename = paste0("pixelated_image_", input$clusters, "_colors_", Sys.Date(), ".jpeg", sep = ""),
    contentType = "image/jpeg",
    content = function(file) {
      file.copy(ret(), file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
