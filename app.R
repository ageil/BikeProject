# ui <- fluidPage(
#     leafletOutput("mymap")
# )
# 
# server <- function(input, output) {
#     
#     output$mymap <- renderLeaflet({
#         leaflet() %>%
#              addProviderTiles("Stamen.TonerLite", 
#                               options = providerTileOptions(noWrap = TRUE)
#                               )
#     })
# }
# 
# shinyApp(ui=ui, server=server)

library(shiny)
library(shinydashboard)
library(googlesheets)
library(leaflet)
library(ggplot2)
library(lubridate)

# Load data from Google
bike <- gs_key("1lGbXMISTa2iBD5xEwufN6cj1BCoLpwTQazxouSBnM0s") # register googlesheet
data <- gs_read(bike, range="A1:S79") # load data form googlesheet


# format:
data$time <- hms(data$time)
data$date <- mdy(data$date)
geodata <- subset(data, complete.cases(data[,18:19])) # only complete lat/lng


shinyApp(
    ui <- dashboardPage(
        dashboardHeader(title="Cycling across America",
                        titleWidth=240),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Dashboard", tabName="dashboard", icon=icon("dashboard")),
                menuItem("Widgets", tabName="widgets", icon=icon("th"))
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName="dashboard",
                        fluidRow(leafletOutput("myMap"))
                        ),
                tabItem(tabName="widgets",
                        tags$h2("Widgets tab content"))
            )
        )
    ),

    server <- function(input, output) {
        
        map <- leaflet() %>% 
            addTiles() %>% 
            addCircleMarkers(lng = geodata$longitude, lat = geodata$latitude, 
                             radius=1)
        
        output$myMap = renderLeaflet(map)
    }
)




