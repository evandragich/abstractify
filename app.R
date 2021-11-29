library(shiny)
library(tidyverse)
library(scales) # label_percent on degree base plot
library(reactable) # color table
library(magick) # image manipulation/metadata extraction
library(colorspace) # sample plots
library(colordistance) # color clustering and pixel plot
library(rclipboard) # copying ordered hexes to user's clipboard
library(bslib) # Shiny themes

# https://cran.r-project.org/web/packages/magick/vignettes/intro.html#Cut_and_edit

source("exploration/outline.R")

# load + create data for sample plots
degrees <- read_csv(here::here("data", "BA_degrees.csv")) %>%
  filter(year >= 1990)

state_list <- tibble(abbreviation = state.abb, name = state.name)

dog_travel <- read_csv(here::here("data", "dog_travel_data.csv")) %>%
  group_by(contact_state) %>%
  summarise(n = n()) %>%
  right_join(state_list, by = c("contact_state" = "abbreviation")) %>%
  mutate(name = str_to_lower(name)) %>%
  left_join(map_data("state"), dogs, by = c("name" = "region"))

brexit <- read_csv(here::here("data", "brexit.csv"), show_col_types = FALSE) %>%
  filter(opinion != "Don't know") %>%
  group_by(region, opinion) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(
    perc = n / sum(n),
    region = case_when(
      region == "scot" ~ "Scotland",
      region == "london" ~ "London",
      region == "midlands_wales" ~ "Midlands / Wales",
      region == "north" ~ "North",
      TRUE ~ "Rest of South"
    ),
    region = factor(region, levels = c("London", "Rest of South", "Midlands / Wales", "North", "Scotland")),
    opinion = factor(opinion, levels = c("Very well", "Fairly well", "Fairly badly", "Very badly"))
  )

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
  theme = bs_theme(version = 4, bootswatch = "flatly"),
  # Application title
  titlePanel(
    h1(
      "Paint by Numbers",
      h4("Click \"Browse...\" to replace the default image with one of your own")
    ),
    windowTitle = "PBN-ify"
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
              "Block Art" = "block-art.jpeg",
              "Emely Picture" = "emely_gutierrez.jpg"
            ),
            selected = "sample-image.jpeg",
            multiple = FALSE
          ),
        ),


        mainPanel(
          tabsetPanel(
            tabPanel(
              title = "Home",
              h2("Welcome to Paint by Numbers"),
              p("How to use: select one our preset images or upload one of your
                own, and use the slider bar to select how many colors you would
                like to see in your image. You can explore the various tabs to
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
              plotOutput("outline"),
              downloadButton("download_outline", "Download outline image")
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
          plotOutput("pixels_plot"),
          reactableOutput("color_table"),
          textOutput("r_squared")
        ),
        tabPanel(
          title = "Plotting",
          fluid = TRUE,
          textOutput("example_plot_description"),
          textOutput("color_vector"),
          rclipboardSetup(), # activates clipboard.js
          uiOutput("clip"),
          radioButtons("example_type",
            "Choose Plot Type:",
            choices = c("Discrete", "Sequential", "Diverging"),
            selected = "Discrete"
          ),
          conditionalPanel(
            condition = "input.example_type != 'Discrete'",
            sliderInput("gray_val",
              "Adjust the darkness of the NA Category",
              min = 10,
              max = 90,
              value = 30,
              step = 5
            ),
            selectInput("low_color",
              "Low Color:",
              choices = NULL
            ),
            selectInput("high_color",
              "High Color:",
              choices = NULL
            )
          ),

          fluidRow(
            column(
              6,
              h2("PBN-ified Plot"),
              plotOutput("colorized_plot"),
              h2("Base color Plot"),
              plotOutput("basic_plot")
              ),
            column(
              6,
              conditionalPanel(
                condition = "input.example_type != 'Diverging'",
              h2("Viridis Plot"),
              plotOutput("viridis_plot")),
              h2("RColorBrewer Plot"),
              plotOutput("colorbrewer_plot"),
                h2("Colorspace Plot"),
                plotOutput("colorspace_plot")
                   )
              )
          )
        )
      )
    ),
    tabPanel(title = "About",
             tabsetPanel(
               tabPanel(title = "Writeup")
             ))

)

# Define server logic
server <- function(input, output, session) {


  # prevents errors when app is first started
  my_path <- reactive(
    if (is.null(input$upload)) {
      here::here("data", input$defaultImage)
    } else {
      input$upload$datapath
    }
  )

  # large object used to extract cluster hexes, pixelated image vector, and goodness of fit data
  my_colors <- reactive(
    colordistance::getKMeanColors(my_path(),
      lower = NULL, upper = NULL,
      sample.size = FALSE, n = input$clusters
    )
  )

  # function used in next value to find closest name for clustered hex colors
  find_closest <- function(my_r, my_g, my_b) {
    temp <- color_names %>%
      mutate(diff = (R - my_r)^2 + (G - my_g)^2 + (B - my_b)^2) %>%
      arrange(diff) %>%
      select(name) %>%
      pull()

    temp[1]
  }

  # dataframe with an observation for every cluster
  # columns are R, G, B vals, % of image comprised, and closest named color
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

  # vector of clustered hex codes arranged to find lightest/darkest for default values for continuous plots
  ordered_hexes <- reactive(
    cluster_lookup() %>%
      mutate(avg = sum(R, G, B)) %>%
      arrange(avg) %>%
      select(rgb_scaled) %>%
      pull()
  )

  # vector of clustered hex codes displayed neatly to user for copy-paste access
  output$color_vector <- renderText({
    paste0("'", ordered_hexes(), "',")
    })

  # "copy to clipboard" button for color_vector() abvoe
  output$clip <- renderUI({
    rclipButton("clipbtn",
                "Copy to Clipboard",
                paste0("'", ordered_hexes(), "',"),
                icon("clipboard"))
  })

  # display quantified goodness of fit
  output$r_squared <- renderText({
    value <- ((1 - (my_colors()$tot.withinss / my_colors()$totss)) * 100) %>%
      round(digits = 1)
    paste0("The goodness of fit, or R-Squared, associated with this pixelation is ", value, "%")
  })


  # obtains dimensions of original image to size the first 2d of manipulated 3d array
  my_dim <- reactive(
    my_path() %>%
      image_read() %>%
      image_info()
  )

  # display original image
  output$original_img <- renderImage(
    {
      list(src = my_path(), height = "200px")
    },
    # saves image after sending to UI
    deleteFile = FALSE
  )


  # converts cluster numbers to respective RGB values creating pixelated image
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

  # write the image object created above; image is written separately here to allow
  # for display and download
  ret <- reactive(
    pxl_img_array() %>%
      image_read() %>%
      image_write(tempfile(fileext = my_dim()$format), format = my_dim()$format)
  )

  # display pixelated image
  output$pixelated_img <- renderImage(
    {
      list(src = ret(), contentType = paste0("image/", my_dim()$format), height = "200px")
    },
    # saves image after sending to UI
    deleteFile = FALSE
  )

  # display plot of pixels in RGB space as outputted by colorspace
  output$pixels_plot <- renderPlot({
    colordistance::plotPixels(my_path(),
      lower = NULL, upper = NULL,
      main = "Colorspace Plot"
    )
  })

  # function to find suitable white/black contrast for input hex code for reactable display
  hex_to_bw <- function(hex) {
    temp <- col2rgb(hex)[1, 1]^2 * .241 +
      col2rgb(hex)[2, 1]^2 * .691 +
      col2rgb(hex)[3, 1]^2 * .068

    if_else(temp > 16900, "#000000", "#FFFFFF")
  }

  # display reactable of image hexes and background colors, percentage of image comprised,
  # and nearest named color
  output$color_table <- renderReactable({
    temp <- cluster_lookup() %>%
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
    "On this page, you can test out the color palette generated from your image in-use in `ggplot2()`.
    \n
    \n
    Paste the hexcodes to recreate the palette.
    \n
    \n
    (If you have chosen alternative
    \"low\" or \"high\" colors for the Sequential or Diverging plots, you may need
    to reorder the hex codes for use.)
    \n
    \n "
  })

  # update color choices for low value on sequential/diverging plots
  observe({
    updateSelectInput(session,
      "low_color",
      label = "Low Color:",
      choices = ordered_hexes(),
      selected = ordered_hexes()[1]
    )
  })

  # update color choices for high value on sequential/diverging plots
  observe({
    updateSelectInput(session,
      "high_color",
      label = "High Color:",
      choices = ordered_hexes(),
      selected = ordered_hexes()[length(ordered_hexes())]
    )
  })

  # create base discrete plot to be layered upon
  # only reactive change is number of non-lumped factors
  discrete_plot <- reactive(
    degrees %>%
      arrange(desc(perc)) %>%
      mutate(field = fct_other(field,
        keep = ordered_fields[1:(input$clusters - 1)]
      )) %>%
      group_by(field, year) %>%
      summarise(perc = sum(perc), .groups = "drop_last") %>%
      ggplot(mapping = aes(x = year, y = perc, group = field, color = field)) +
      geom_line(size = 1) +
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
  sequential_plot <- ggplot(dog_travel, mapping = aes(x = long, y = lat, group = group, fill = n)) +
    geom_polygon(color = "black") +
    labs(
      title = "Number of Dogs Available to Adopt in U.S. States",
      caption = "Gray represents states with missing or unknown data.",
      fill = "Dogs"
    ) +
    theme_void() +
    theme(plot.title = element_text(size = 18, hjust = 0.5))

  # creates diverging base plot which is NOT reactive
  diverging_plot <- ggplot(brexit, mapping = aes(x = perc, y = region, fill = opinion)) +
    geom_col() +
    labs(
      title = "How well or badly do you think the government are doing\n at handling Britain's exit from the European Union?",
      subtitle = "YouGov Survey Results, 2-3 September 2019",
      caption = "Source: bit.ly/2lCJZVg",
      x = NULL,
      y = NULL,
      fill = "Opinion"
    ) +
    scale_x_continuous(labels = label_percent()) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.direction = "horizontal"
    )


  # output the base ggplot
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
          na.value = paste0("gray", (100 - input$gray_val)),
          guide = guide_colorbar()
        )
    } else {
      diverging_plot +
        scale_fill_manual(
          values = c(
            colorRampPalette(c("#00BFC4", "white"))(3)[1:2],
            colorRampPalette(c("white", "#F8766D"))(3)[2:3]
          ),
          na.value = paste0("gray", (100 - input$gray_val)),
          guide = guide_legend(reverse = TRUE)
        )
    }
  })

  # output the ggplot with our colors
  output$colorized_plot <- renderPlot({
    if (input$example_type == "Discrete") {
      discrete_plot() +
        scale_color_manual(
          values = cluster_lookup()$rgb_scaled
        )
    } else if (input$example_type == "Sequential") {
      sequential_plot +
        scale_fill_gradient(
          trans = "log10",
          low = input$low_color,
          high = input$high_color,
          na.value = paste0("gray", (100 - input$gray_val)),
          guide = guide_colorbar()
        )
    } else {
      diverging_plot +
        scale_fill_manual(
          values = c(
            colorRampPalette(c(input$low_color, "white"))(3)[1:2],
            colorRampPalette(c("white", input$high_color))(3)[2:3]
          ),
          na.value = paste0("gray", (100 - input$gray_val)),
          guide = guide_legend(reverse = TRUE)
        )
    }
  })

  # output the plot with colorspace package colors
  output$colorspace_plot <- renderPlot({
    if (input$example_type == "Discrete") {
      discrete_plot() +
        scale_color_discrete_qualitative()
    } else if (input$example_type == "Sequential") {
      sequential_plot +
        scale_fill_continuous_sequential(
          trans = "log10",
          na.value = paste0("gray", (100 - input$gray_val)),
          guide = guide_colorbar()
        )
    } else {
        diverging_plot +
          scale_fill_discrete_diverging(
            na.value = paste0("gray", (100 - input$gray_val)),
            guide = guide_legend(reverse = TRUE)
          )
      }
  })

  # output the ggplot with viridis colors
  output$viridis_plot <- renderPlot({
    if (input$example_type == "Discrete") {
      discrete_plot() +
        scale_color_viridis_d()
    } else if (input$example_type == "Sequential") {
      sequential_plot +
        scale_fill_viridis_c(
          trans = "log10",
          option = "A",
          na.value = paste0("gray", (100 - input$gray_val)),
          guide = guide_colorbar()
        )
    } else {
      NULL
    }
  })

  # output the plot with RColorbrewer colors
  output$colorbrewer_plot <- renderPlot({
    if (input$example_type == "Discrete") {
      discrete_plot() +
        scale_color_brewer(
          type = "qual"
        )
    } else if (input$example_type == "Sequential") {
      sequential_plot +
        scale_fill_distiller(
          trans = "log10",
          na.value = paste0("gray", (100 - input$gray_val)),
          guide = guide_colorbar()
        )
    } else {
      diverging_plot +
        scale_fill_brewer(
          type = "div",
          na.value = paste0("gray", (100 - input$gray_val)),
          guide = guide_legend(reverse = TRUE)
        )
    }
  })

  output_mat <- reactive({
    outline_func(my_colors()$cluster, c(my_dim()$height, my_dim()$width))
  })

  output$outline <- renderPlot({
    plot_outline(output_mat(), c(my_dim()$height, my_dim()$width))
  })

  output_image_mat <- reactive({
    rbg_outline(output_mat())
  })

  ret1 <- reactive({
    as.raw(c(output_mat(),output_mat(),output_mat())) %>%
      image_read() %>%
      image_write(tempfile(fileext = ".jpeg"), format = my_dim()$format)
  })

  #download doesn't work yet
  output$outline_img <- renderImage(
    {
      list(src = ret1(), contentType = paste0("image/", my_dim()$format), height = "200px")
    },
    # saves image after sending to UI
    deleteFile = FALSE
  )

  # adds download capability for pixelated image
  output$download_pxl <- downloadHandler(
    filename = paste0("pixelated_image_", input$clusters, "_colors_", Sys.Date(), ".jpeg"),
    contentType = "image/jpeg",
    content = function(file) {
      file.copy(ret(), file)
    }
  )

  output$download_outline <- downloadHandler(
    filename = paste0("outline_image_", Sys.Date(), ".jpeg"),
    contentType = "image/jpeg",
    content = function(file) {
      file.copy(ret1(), file)
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)
