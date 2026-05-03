library(leaflet)
library(tigris)
library(sf)
library(dplyr)
library(viridis)
library(stringr)

options(tigris_use_cache = TRUE)

Data_Violations <- read.csv("Food_Service_Establishment__Last_Inspection_20260205.csv")

Data_Violations_Cleaned <- Data_Violations %>%
  dplyr::mutate(across(c(FACILITY, ADDRESS,
                         FACILITY.ADDRESS, CITY,
                         OPERATION.NAME,
                         PERMITTED...D.B.A.,
                         PERMITTED..CORP..NAME,
                         PERM..OPERATOR.LAST.NAME,
                         PERM..OPERATOR.FIRST.NAME), toupper))

Data_Violations_County <- Data_Violations_Cleaned %>%
  select(-ADDRESS, -LAST.INSPECTED, -VIOLATIONS, -DESCRIPTION,
         -X.LOCAL.HEALTH.DEPARTMENT, -CITY, -FACILITY.ADDRESS, -ZIP.CODE,
         -NYSDOH.GAZETTEER..1980., -MUNICIPALITY, -OPERATION.NAME,
         -PERMIT.EXPIRATION.DATE, -PERMITTED...D.B.A., -PERMITTED..CORP..NAME,
         -NYS.HEALTH.OPERATION.ID, -INSPECTION.TYPE, -INSPECTION.COMMENTS,
         -FOOD.SERVICE.FACILITY.STATE, -Location1, -PERM..OPERATOR.LAST.NAME,
         -PERM..OPERATOR.FIRST.NAME)

Data_County_Renamed <- Data_Violations_County %>%
  rename(Critical = TOTAL...CRITICAL.VIOLATIONS,
         Critical_Not_Corrected = TOTAL..CRIT...NOT.CORRECTED.,
         Non_Critical = TOTAL...NONCRITICAL.VIOLATIONS)

county_stats <- Data_County_Renamed %>%
  group_by(COUNTY) %>%
  summarise(
    Total_Violations = sum(Non_Critical, na.rm = TRUE),
    N_Restaurants    = n(),
    Prevalence       = round(Total_Violations / N_Restaurants, 3),
    Mean_Violations  = round(mean(Non_Critical, na.rm = TRUE), 2)
  ) %>%
  mutate(COUNTY = stringr::str_to_title(COUNTY))

ny_counties <- tigris::counties(state = "NY", cb = TRUE, year = 2022) %>%
  st_transform(4326) %>%
  mutate(NAME = case_when(
    NAME == "St. Lawrence" ~ "St Lawrence",
    TRUE ~ NAME
  )) %>%
  left_join(county_stats, by = c("NAME" = "COUNTY"))

pal <- colorNumeric(
  palette = c("#313695", "#4575b4", "#74add1", "#abd9e9",
              "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026"),
  domain  = ny_counties$Mean_Violations,
  na.color = "#cccccc"
)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Georgia', serif; background-color: #1a1a2e; color: #eaeaea; }
    h2 { text-align: center; padding: 20px; color: #f0c040; letter-spacing: 2px; font-size: 1.4em; }
    #info_box { background: rgba(255,255,255,0.08); border-left: 4px solid #f0c040;
      border-radius: 6px; padding: 16px 20px; margin: 12px 20px; font-size: 1em; min-height: 60px; }
    .info-label { color: #f0c040; font-weight: bold; }
    #map { border-radius: 10px; }
    .footer { text-align: center; color: #666; font-size: 0.75em; padding: 10px; }
    details { margin: 0 20px 10px 20px; border-left: 4px solid #f0c040; padding-left: 10px; }
    summary { font-size: 1em; padding: 5px 0; }
  "))),
  
  h2("New York State - Non-Critical Food Safety Violations"),
  
  tags$details(
    tags$summary("About this map", style = "color: #f0c040; cursor: pointer;"),
    p("Research Question: How does non-critical food safety violations vary by county in New York State?",
      style = "color: #f0c040; font-weight: bold;"),
    p("Author: Philip Aquila Salvatore Tapan Dahal"),
    p("The data analysed is about food safety violations in restaurants across New York State,
      provided by the New York State Department of Health and last updated February 1, 2026.
      The data covers all of New York State, updated monthly since January 29, 2013,
      with approximately 21,745 observations."),
    p(tags$a("Data Source", href = "https://health.data.ny.gov/Health/Food-Service-Establishment-Last-Inspection/cnih-y5dw/about_data",
             target = "_blank", style = "color: #f0c040;")),
    p(tags$a("Github Repository", href = "https://github.com/pad238-art/R-Homework",
             target = "_blank", style = "color: #f0c040;")),
    p("This application shows a heatmap of Non-Critical Food Safety Violations in New York State
      (excluding New York City) comparing each county. The closer to red the county colour,
      the higher the violations relative to the number of restaurants.
      Clicking on a county shows: the county name, number of restaurants, total violations,
      mean violations, and prevalence (violations/restaurants)."),
    p("AI Disclosure Statement: This application was developed using code from Claude.
      The code was reviewed and tested before deployment."),
    style = "color: #eaeaea; padding: 10px;"
  ),
  
  leafletOutput("map", height = "560px"),
  div(id = "info_box", uiOutput("county_info")),
  div(class = "footer",
      "Click any county to see its violation prevalence | 
       Color scale: cold (blue) = fewer violations, hot (red) = more violations")
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(ny_counties) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addPolygons(
        fillColor        = ~pal(Mean_Violations),
        fillOpacity      = 0.8,
        color            = "#ffffff",
        weight           = 1,
        opacity          = 0.5,
        highlightOptions = highlightOptions(
          weight = 3, color = "#f0c040", fillOpacity = 0.95, bringToFront = TRUE
        ),
        label   = ~paste0(NAME, ": Mean = ", round(Mean_Violations, 2)),
        layerId = ~NAME
      ) %>%
      addLegend(pal = pal, values = ~Mean_Violations,
                title = "Mean Non-Critical<br>Violations",
                position = "bottomright", opacity = 0.8)
  })
  
  clicked_county <- reactiveVal(NULL)
  
  observeEvent(input$map_shape_click, {
    clicked_county(input$map_shape_click$id)
  })
  
  output$county_info <- renderUI({
    county <- clicked_county()
    if (is.null(county)) {
      return(HTML("<span style='color:#aaa;'>Click a county on the map to see its details.</span>"))
    }
    row <- county_stats %>% filter(COUNTY == county)
    if (nrow(row) == 0) {
      return(HTML(paste0("<b>", county, "</b>: No data available.")))
    }
    HTML(paste0(
      "<span class='info-label'>County:</span> ", county, "&emsp;",
      "<span class='info-label'>Restaurants:</span> ", row$N_Restaurants, "&emsp;",
      "<span class='info-label'>Total Violations:</span> ", row$Total_Violations, "<br>",
      "<span class='info-label'>Mean Violations:</span> ", row$Mean_Violations, "&emsp;",
      "<span class='info-label'>Prevalence (violations/restaurant):</span> ",
      "<b style='color:#f0c040; font-size:1.1em;'>", row$Prevalence, "</b>"
    ))
  })
}

shinyApp(ui, server)
