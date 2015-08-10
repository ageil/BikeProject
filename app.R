####################
### Bike project ###
####################

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
                        tags$h2("Widgets tab content"),
                        selectInput(inputId="yvar", 
                                    label = "Choose a variable to plot",
                                    choices=c("Distance" = "Distance (km)",
                                              "Average speed" = "Average speed (km/h)",
                                              "Top speed" = "Top speed (km/h)",
                                              "Altitude up" = "Altitude up (m)",
                                              "Altitude down" = "Altitude down (m)",
                                              "Average climb" = "Average climb (%)",
                                              "Average descent" = "Average descent (%)",
                                              "Max climb" = "Max climb (%)",
                                              "Max descent" = "Max descent (%)",
                                              "Total distance" = "Total distance (km)",
                                              "Total altitude up" = "Total altitude up (m)",
                                              "Total altitude down" = "Total altitude down (m)")
                                    ),
                        plotOutput(outputId = "dayplot")
                        )
                )
            )
        ),

    
    
    server <- function(input, output) {
        
        map <- leaflet() %>% 
            addProviderTiles("Thunderforest.Outdoors",
                             options=providerTileOptions(noWrap=TRUE)) %>%
            addCircleMarkers(lng = geodata$longitude, lat = geodata$latitude, 
                             radius=1)
        
        output$myMap <- renderLeaflet(map)
        
        output$dayplot <- reactivePlot(function() {
            if (input$yvar == "Distance (km)") {
                plotdata <- data.frame(day = data$day, var = data$distance)
            } else if (input$yvar == "Average speed (km/h)") {
                plotdata <- data.frame(day = data$day, var = data$avg_speed)
            } else if (input$yvar == "Top speed (km/h)") {
                plotdata <- data.frame(day = data$day, var = data$top_speed)
            } else if (input$yvar == "Altitude up (m)") {
                plotdata <- data.frame(day = data$day, var = data$alt_up)
            } else if (input$yvar == "Altitude down (m)") {
                plotdata <- data.frame(day = data$day, var = data$alt_down)
            } else if (input$yvar == "Average climb (%)") {
                plotdata <- data.frame(day = data$day, var = data$avg_climb)
            } else if (input$yvar == "Average descent (%)") {
                plotdata <- data.frame(day = data$day, var = data$avg_descent)
            } else if (input$yvar == "Max climb (%)") {
                plotdata <- data.frame(day = data$day, var = data$max_climb)
            } else if (input$yvar == "Max descent (%)") {
                plotdata <- data.frame(day = data$day, var = data$max_descent)
            } else if (input$yvar == "Total distance (km)") {
                plotdata <- data.frame(day = data$day, var = cumsum(ifelse(is.na(data$distance), 0, data$distance)))
            } else if (input$yvar == "Total altitude up (m)") {
                plotdata <- data.frame(day = data$day, var = cumsum(ifelse(is.na(data$alt_up), 0, data$alt_up)))
            } else if (input$yvar == "Total altitude down (m)") {
                plotdata <- data.frame(day = data$day, var = cumsum(ifelse(is.na(data$alt_down), 0, data$alt_down)))
            }
            
            p <- ggplot(plotdata, aes(day, var)) +
                geom_point() +
                geom_smooth() +
                xlab("Day") +
                ylab(input$yvar)
            print(p)
        })
    }
)




