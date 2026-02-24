library(shiny)
library(tidyverse)
library(leaflet)
library(DT)
library(scales)
library(dplyr)
library(ggplot2)
library(plotly)

theme_set(theme_bw())
utils::globalVariables(c(
    "capacity_mw", "commissioning_year", "latitude", "longitude",
    "primary_fuel", "country_long", "owner", "has_generation_data",
    "gppd_idnr", "name", "source", "url", "total_capacity_mw"
))

#### Load & clean data #####
power <- read_csv(
    "https://raw.githubusercontent.com/luisylizaliturri/stat436-hw2-data/136c0c8d573f893dbd947d316d600d4975d22487/global_power_plant_database.csv",
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
)

power <- power |>
    mutate(
        capacity_mw = as.numeric(capacity_mw),
        commissioning_year = as.integer(commissioning_year),
        latitude = as.numeric(latitude),
        longitude = as.numeric(longitude),
        primary_fuel = if_else(is.na(primary_fuel) | primary_fuel == "", "Unknown", primary_fuel),
        country_long = if_else(is.na(country_long) | country_long == "", "Unknown", country_long),
        owner = if_else(is.na(owner) | owner == "", "Unknown", owner),
        has_generation_data = if_any(starts_with("generation_gwh_"), ~ !is.na(.x))
    )

#global color palette for bar chart and map
fuel_colors <- c(
  "Coal"           = "black",
  "Gas"            = "darkorange",
  "Hydro"          = "dodgerblue",
  "Nuclear"        = "red",
  "Oil"            = "saddlebrown",
  "Solar"          = "gold",
  "Wind"           = "mediumorchid",
  "Biomass"        = "darkgreen",
  "Waste"          = "limegreen",
  "Geothermal"     = "deeppink",
  "Cogeneration"   = "navy",
  "Petcoke"        = "darkred",
  "Wave and Tidal" = "lightseagreen",
  "Storage"        = "forestgreen",
  "Other"          = "gray70",
  "Unknown"        = "white"
)

##### Helper function for bar chart of capacity by fuel ######
fuel_plot <- function(df) {
    fuel_df <- df |>
        group_by(primary_fuel) |>
        summarize(total_capacity_mw = sum(capacity_mw, na.rm = TRUE), .groups = "drop") |>
        arrange(desc(total_capacity_mw)) |>
        slice_head(n = 12)

    p <- ggplot(fuel_df, aes(primary_fuel, total_capacity_mw, fill = primary_fuel, key = primary_fuel)) +
        geom_col(show.legend = FALSE) +
        scale_fill_manual(values = fuel_colors, na.value = fuel_colors[["Unknown"]]) +
        scale_y_continuous(labels = scales::comma) +
        labs(
            title = "Installed Capacity by Fuel",
            x = "Primary fuel",
            y = "Total capacity (MW)"
        ) +
        theme(axis.text.x = element_text(angle = 35, hjust = 1))

    ggplotly(p, source = "fuel_bar") |>
        config(displayModeBar = FALSE) |>
        layout(dragmode = FALSE)
}


#### UI config #####
ui <- fluidPage(
    titlePanel("Global Power Plant Explorer"),
    p("Filter on the left, then click bars or map points to dive deeper."),
    sidebarLayout(
        sidebarPanel(
            width = 5,
            selectizeInput("country", "Country", choices = NULL, multiple = TRUE),
            selectizeInput("fuel", "Primary Fuel", choices = NULL, multiple = TRUE),
            sliderInput("capacity", "Capacity (MW)", min = 0, max = 1000, value = c(0, 1000)),
            sliderInput("year", "Commissioning Year", min = 1900, max = 2025, value = c(1950, 2025), sep = ""),
            checkboxInput("generation_only", "Only plants with generation data", FALSE),
            actionButton("clear", "Clear chart/map selection"),
            hr(),
            plotlyOutput("fuel_plot", height = 380),
            hr(),
            strong("Dynamic queries:"),
            tags$ul(
                tags$li("Sidebar filters"),
                tags$li("Click a fuel bar to filter by fuel"),
                tags$li("Click map marker to inspect one plant")
            )
        ),
        mainPanel(
            width= 7,
            fluidRow(
                column(4, h4(textOutput("n_plants"))),
                column(4, h4(textOutput("total_capacity"))),
                column(4, h4(textOutput("n_countries")))
            ),
            leafletOutput("map", height = 600),
            DTOutput("table"),
            p(
                "Data source: ",
                tags$a("Global Power Plant Database (Kaggle)",
                       href = "https://www.kaggle.com/datasets/dianaddx/global-power-plant-database?resource=download",
                       target = "_blank")
            )
        )
    ),
    theme = bs_theme(
      primary = "dodgerblue",   
      secondary = "skyblue",
    )
)


#### Server #####
server <- function(input, output, session) {
    #reactive state for click based queries
    selected_fuel <- reactiveVal(NULL)
    selected_plant <- reactiveVal(NULL)

    # set filter widgets from data
    observe({
        updateSelectizeInput(session, "country", choices = sort(unique(power$country_long)), server = TRUE)
        updateSelectizeInput(session, "fuel", choices = sort(unique(power$primary_fuel)), server = TRUE)

        max_cap <- ceiling(max(power$capacity_mw, na.rm = TRUE) / 100) * 100
        updateSliderInput(session, "capacity", min = 0, max = max_cap, value = c(0, max_cap))

        years <- power$commissioning_year[!is.na(power$commissioning_year)]
        updateSliderInput(session, "year", min = min(years), max = max(years), value = c(min(years), max(years)))
    })

    #Dynamic query 1: sidebar filters
    filtered <- reactive({
        df <- power

        if (length(input$country) > 0) {
            df <- filter(df, country_long %in% input$country)
        }

        if (length(input$fuel) > 0) {
            df <- filter(df, primary_fuel %in% input$fuel)
        }

        df <- df |>
            filter(is.na(capacity_mw) | (capacity_mw >= input$capacity[1] & capacity_mw <= input$capacity[2])) |>
            filter(is.na(commissioning_year) | (commissioning_year >= input$year[1] & commissioning_year <= input$year[2]))

        if (isTRUE(input$generation_only)) {
            df <- filter(df, has_generation_data)
        }

        df
    })

    observeEvent(input$clear, {
        selected_fuel(NULL)
        selected_plant(NULL)
    })

    #Dynamic query 2: click bar chart to filter by fuel type
    observeEvent(event_data("plotly_click", source = "fuel_bar"), {
        click_info <- event_data("plotly_click", source = "fuel_bar")
        if (!is.null(click_info)) {
            selected_fuel(click_info$key[[1]])
            selected_plant(NULL)
        }
    })

    #click map marker to inspect a single plant
    observeEvent(input$map_marker_click, {
        if (!is.null(input$map_marker_click$id)) {
            selected_plant(input$map_marker_click$id)
        }
    })

    #combine sidebar filters with click selection
    final_df <- reactive({
        df <- filtered()
        if (!is.null(selected_fuel())) {
            df <- filter(df, primary_fuel == selected_fuel())
        }
        df
    })

    #### Outputs ####
    output$fuel_plot <- renderPlotly({
        fuel_plot(filtered())
    })

    output$map <- renderLeaflet({
        df <- final_df() |>
            filter(!is.na(latitude), !is.na(longitude), !is.na(capacity_mw))

        if (nrow(df) == 0) {
            return(
                leaflet() |>
                    addProviderTiles(providers$CartoDB.Positron) |>
                    setView(lng = 0, lat = 20, zoom = 2)
            )
        }

        leaflet(df) |>
            addProviderTiles(providers$CartoDB.Positron) |>
            addCircleMarkers(
                lng = ~longitude,
                lat = ~latitude,
                radius = ~ pmax(4, pmin(12, sqrt(capacity_mw) / 3)),
                color = ~ifelse(
                    primary_fuel %in% names(fuel_colors),
                    fuel_colors[primary_fuel],
                    fuel_colors[["Other"]]
                ),
                fillColor = ~ifelse(
                    primary_fuel %in% names(fuel_colors),
                    fuel_colors[primary_fuel],
                    fuel_colors[["Other"]]
                ),
                fillOpacity = 0.5,
                stroke = FALSE,
                layerId = ~gppd_idnr,
                popup = ~ paste0(
                    "<strong>", name, "</strong><br/>",
                    country_long, "<br/>",
                    "Fuel: ", primary_fuel, "<br/>",
                    "Capacity: ", comma(capacity_mw), " MW"
                )
            )
    })

    output$table <- renderDT({
        df <- final_df()

        if (!is.null(selected_plant())) {
            one <- filter(df, gppd_idnr == selected_plant())
            if (nrow(one) > 0) df <- one
        }

        df |>
            select("name", "country_long", "primary_fuel", "capacity_mw", "commissioning_year", "owner", "source", "url") |>
            arrange(desc(capacity_mw)) |>
            mutate(
                commissioning_year = if_else(is.na(commissioning_year), "Unknown", as.character(commissioning_year))
            ) |>
            datatable(
                rownames = FALSE,
                options = list(pageLength = 8, scrollX = TRUE),
                colnames = c("Plant", "Country", "Fuel", "Capacity (MW)", "Commissioning Year", "Owner", "Source", "URL")
            ) |>
            formatCurrency("capacity_mw", currency = "", digits = 0)
    })

    #Summary stats
    output$n_plants <- renderText(paste("Plants shown:", scales::comma(nrow(final_df()))))
    output$total_capacity <- renderText(paste("Total capacity:", scales::comma(sum(final_df()$capacity_mw, na.rm = TRUE)), "MW"))
    output$n_countries <- renderText(paste("Countries:", n_distinct(final_df()$country_long)))
}

shinyApp(ui, server)
