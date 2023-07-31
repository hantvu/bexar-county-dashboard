#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(shinydashboard)
library(tidycensus)
library(tidyverse)
library(shinythemes)
library(RColorBrewer)

# Define the groups variable
groups <- c("Hispanic" = "hispanic",
            "White" = "white",
            "Black" = "black",
            "Native American" = "native",
            "Asian" = "asian")

# UI
ui <- dashboardPage(
  dashboardHeader(
    # Increase the titleWidth to expand the header
    titleWidth = 600,  # Adjust this value as needed
    title = "Bexar County Community Dashboard"
  ),
  dashboardSidebar(
    # Add narrative text to the sidebar with working links
    tags$div(
      style = "padding: 10px; font-family: sans-serif; font-size: 14px;",
      HTML("<h4>Welcome to Bexar County Community Dashboard</h4>
           <p>This dashboard provides an overview of various demographic and socioeconomic factors for different census tract in Bexar County, Texas. Explore the data and maps to gain insights into the community.</p>
           <p>The data used in this analysis comes from the Census Bureau's American Community Survey 5-year 2017-2021, which is the latest available data.</p>
           <p>The data for the Population Pyramid is from the Census's Decennial 2020.</p>
           <p>This community map is created by Han Vu of API's Product team to support partners to have more context about the community they serve.</p>
           <p>The dashboard includes:</p>
           <ul>
             <li><a href='#map_tab-1'>Household Median Income</a></li>
             <li><a href='#map_tab-1'>Population Pyramid</a></li>
             <li><a href='#map_tab-1'>Race Map</a></li>
             <li><a href='#map_tab-2'>Median Age Map</a></li>
             <li><a href='#map_tab-3'>Median Income Map</a></li>
             <li><a href='#map_tab-4'>Educational Level Map</a></li>
           </ul>")
    )
  ),
  
  dashboardBody(tags$head(
    tags$style(
      HTML("
          /* Customize the tab bar */
          .nav-tabs > li > a:hover,
          .nav-tabs > li.active > a,
          .nav-tabs > li.active > a:hover,
          .nav-tabs > li.active > a:focus {
            background-color: lightblue !important;
            color: white;
          }
        ")
    )
  ),
  fluidRow(
    column(width = 6,
           box(title = "Income Distribution", width = 12, plotOutput("income_chart"))
    ),
    column(width = 6,
           box(title = "Gender and Age Distribution", width = 12, plotOutput("gender_age_chart"))
    )
  ),
  fluidRow(
    tabBox(
      title = "Map Tab",
      id = "map_tab",
      width = 12,
      tabPanel(
        "Race Map",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "group",
              label = "Select a race group to map distribution",
              choices = groups
            )
          ),
          mainPanel(
            leafletOutput("race_map", height = "700")
          )
        )
      ),
      tabPanel(
        "Median Age Map",
        splitLayout(
          cellWidths = c("40%", "60%"),
          plotOutput("line_chart"),
          leafletOutput("age_map", height = "700")
        )
      ),
      tabPanel("Median Income Map", leafletOutput("income_map", height = "700")),
      tabPanel("Educational Level Map", leafletOutput("education_map", height = "700"))
    )
  )
  )
)

# Server
server <- function(input, output, session) {
  # Median Income
  race_income <- get_acs (
    geography = "county",
    state = "TX",
    county = "Bexar",
    variables = c(White = "B19013A_001",
                  Black = "B19013B_001",
                  Asian = "B19013D_001",
                  Hispanic = "B19013I_001"),
    summary_var = "B19013_001",
    year = 2021
  )
  
  # Render income chart (there are too many counties in Texas, the chart doesn't look good, need another one to replace)
  output$income_chart <- renderPlot({
    ggplot(race_income, aes(x = estimate, y = reorder(variable, estimate))) +
      geom_col(fill = "steelblue", color = "black") +
      geom_text(aes(label = scales::dollar(estimate, scale = 1e-3)), 
                position = position_stack(vjust = 0.5), 
                size = 6, color = "white", hjust = -1.0) +  # Adjust position and appearance of text
      theme_minimal() +
      labs(x = "Median Household Income by Race, ACS5 2017 - 2021",
           y = NULL,  # Remove y-axis label
           title = "Median Household Income by Race in Bexar County") +
      scale_x_continuous(labels = scales::dollar_format(scale = 1e-3))
    
  })
  
  # Population Pyramid
  
  cohort_names <- c("0-4", "5-9", "10-14", "15-19",
                    "20-24", "25-29", "30-34", "35-39",
                    "40-44", "45-49", "50-54", "55-59",
                    "60-64", "65-69", "70-74", "75-79",
                    "80-84", "85+")
  
  male_vars <- 26:43 %>%
    paste0("DP1_00", ., "P") %>%
    set_names(cohort_names)
  
  female_vars <- 50:67 %>%
    paste0("DP1_00", ., "P") %>%
    set_names(cohort_names)
  
  male_data <- get_decennial(
    geography = "county",
    variables = male_vars,
    state = "TX",
    county = "Bexar",
    year = 2020,
    sumfile = "dp"
  ) %>%
    mutate(sex = "Male",
           pyramid_value = value * -1)
  
  female_data <- get_decennial(
    geography = "county",
    county = "Bexar",
    variables = female_vars,
    state = "TX",
    year = 2020,
    sumfile = "dp"
  ) %>%
    mutate(sex = "Female",
           pyramid_value = value)
  
  pyramid_data <- bind_rows(male_data, female_data) %>%
    mutate(variable = factor(variable, levels = cohort_names))
  
  
  output$gender_age_chart <- renderPlot({
    ggplot(pyramid_data, aes(x = pyramid_value, y = variable,
                             fill = sex)) +
      geom_col(width = 0.95, alpha = 0.75) +
      theme_minimal(base_size = 12) +
      scale_x_continuous(labels = function(x) paste0(abs(x), "%")) +
      scale_fill_manual(values = c("black", "#FFCD00")) +
      labs(x = "",
           y = "",
           title = "Population structure in Bexar County",
           fill = "",
           caption = "Data source: 2020 decennial Census")
  })
  
  # Render race map
  race <- get_acs(
    geography = "tract",
    variables = c(
      hispanic = "DP05_0071P",
      white = "DP05_0077P",
      black = "DP05_0078P",
      native = "DP05_0079P",
      asian = "DP05_0080P",
      year = 2021
    ),
    state = "TX",
    county = "Bexar",
    geometry = TRUE
  )
  
  # Reproject the sf layer to WGS84 datum
  race <- st_transform(race, "+proj=longlat +datum=WGS84")
  
  # Reactive function that filters for the selected group in the drop-down menu
  group_to_map <- reactive({
    filter(race, variable == input$group)
  })
  
  output$race_map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.4936, lat = 29.4241, zoom = 10)
  })
  
  observeEvent(input$group, {
    pal <- colorNumeric("viridis", group_to_map()$estimate)
    
    leafletProxy("race_map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = group_to_map(),
                  color = ~pal(estimate),
                  weight = 0.5,
                  fillOpacity = 0.7,
                  smoothFactor = 0.2,
                  label = ~paste(variable, ": ", estimate, "%")) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = group_to_map()$estimate,
        title = "% of population"
      )
  })
  
  #Linechart
  ages <- c(0:99, rep(100, 3))
  
  male_vars2 <- paste0("PCT012", str_pad(3:105, 3, "left", "0"))
  female_vars2 <- paste0("PCT012", 107:209)
  
  names(male_vars2) <- ages
  names(female_vars2) <- ages
  
  all_vars <- c(male_vars2, female_vars2)
  
  pull00 <- get_decennial(
    geography = "county",
    state = "TX",
    county = "Bexar",
    variables = all_vars, 
    year = 2000
  ) %>% 
    summarize(value = sum(value, na.rm = TRUE),
              .by = variable) %>% 
    mutate(year = "2000")
  
  pull10 <- get_decennial(
    geography = "county",
    state = "TX",
    county = "Bexar",
    variables = all_vars, 
    year = 2010
  ) %>% 
    summarize(value = sum(value, na.rm = TRUE),
              .by = variable) %>% 
    mutate(year = "2010")
  
  male_vars20 <- paste0("PCT12_", str_pad(3:105, 3, "left", "0"), "N")
  female_vars20 <- paste0("PCT12_", 107:209, "N")
  
  names(male_vars20) <- ages
  names(female_vars20) <- ages
  
  all_vars20 <- c(male_vars20, female_vars20)
  
  pull20 <- get_decennial(
    geography = "county",
    state = "TX",
    county = "Bexar",
    variables = all_vars20,
    year = 2020,
    sumfile = "dhc"
  ) %>% 
    summarize(value = sum(value, na.rm = TRUE),
              .by = variable) %>% 
    mutate(year = "2020")
  
  all_years <- bind_rows(pull00, pull10, pull20)
  
  output$line_chart <- renderPlot({
    ggplot(all_years, aes(x = as.numeric(variable), y = value, color = year, group = year)) +
      geom_line(size = 1.2) + 
      theme_minimal() +
      scale_color_brewer(palette = "Set1") +
      scale_y_continuous(labels = scales::label_comma()) +
      labs(y = "Population",
           x = "Single-year age",
           color = "Year",
           title = glue::glue("Age distribution in Bexar County")) +
      theme(legend.position = "bottom")
    # You can customize the plot according to your data and requirements
  })
  
  # Render median age map
  age <- get_acs(
    geography = "tract",
    state = "TX",
    county = "Bexar",
    variables = "B01002_001",
    year = 2021,
    geometry = TRUE
  )
  
  output$age_map <- renderLeaflet({
    leaflet(age) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.4936, lat = 29.4241, zoom = 10) %>%
      addPolygons(
        fillColor = ~colorNumeric(
          palette = "BrBG",
          domain = estimate,
          na.color = "transparent",
          reverse = TRUE
        )(estimate),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        label = ~paste0("Median Age: ", estimate),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = colorNumeric(
          palette = "BrBG",
          domain = age$estimate,
          na.color = "transparent",
          reverse = TRUE
        ),
        values = age$estimate,
        title = HTML("Median age<br/>2017 - 2021 ACS"),
        opacity = 0.7
      )
  })
  
  # Render median income map
  output$income_map <- renderLeaflet({
    income_tract <- get_acs(
      geography = "tract",
      variables = c(median_income = "B19013_001"),
      state = "TX",
      county = "Bexar",
      geometry = TRUE
    )
    
    income_tract$estimate <- as.numeric(income_tract$estimate)
    income_tract <- income_tract[!is.na(income_tract$estimate), ]
    
    leaflet(income_tract) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.4936, lat = 29.4241, zoom = 10) %>%
      addPolygons(
        fillColor = ~colorQuantile("Blues", income_tract$estimate)(estimate),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        label = ~paste0("Median Income: $", estimate),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = colorQuantile("Blues", income_tract$estimate),
        values = income_tract$estimate,
        title = "Median Income",
        label = ~paste0("Income: ", estimate),
        opacity = 0.7
      )
  })
  
  # Render educational level map
  edu_shape <- get_acs(
    geography = "tract",
    variables = "DP02_0068P",
    state = "TX",
    county = "Bexar",
    year = 2021,
    geometry = TRUE
  )
  
  output$education_map <- renderLeaflet({
    leaflet(edu_shape) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.4936, lat = 29.4241, zoom = 10) %>%
      addPolygons(
        fillColor = ~colorNumeric(
          palette = "viridis",
          domain = estimate,
          na.color = "transparent"
        )(estimate),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        label = ~paste0("Education Level: ", estimate, "%"),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = colorNumeric(
          palette = "viridis",
          domain = edu_shape$estimate,
          na.color = "transparent"
        ),
        values = edu_shape$estimate,
        title = "Education Level",
        opacity = 0.7
      ) %>%
      addControl(
        position = "topright",
        html = '<div style="padding: 5px; background-color: white; font-size: 12px;">
                <strong>Education Level</strong><br/>
                Percentage of population 25+ with at least a 4-year degree or higher
              </div>'
      )
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
