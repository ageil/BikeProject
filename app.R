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
data <- gs_read(bike, range="A1:S79") # load data from googlesheet; specific rows to avoid bug


# Format
data$time <- hms(data$time) # cannot ggplot type "period"?
data$date <- mdy(data$date)
geodata <- subset(data, complete.cases(data[,18:19])) # only complete lat/lng (leaflet doesnt handle NA yet)


shinyApp(
    ui <- dashboardPage(
        dashboardHeader(title="Cycling across America",
                        titleWidth=240),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Map", tabName="map", icon=icon("globe")),
                menuItem("Statistics", tabName="statistics", icon=icon("area-chart")),
                menuItem("About", tabName="about", icon=icon("info"))
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName="map",
                        fluidRow(leafletOutput("myMap"))
                        ),
                tabItem(tabName="statistics",
                        tags$h2("Statistics"),
                        selectInput(inputId="yvar", 
                                    label = "Choose a variable to plot",
                                    choices=c("Total distance" = "Total distance (km)",
                                              "Total altitude up" = "Total altitude up (m)",
                                              "Total altitude down" = "Total altitude down (m)",
                                              "Distance" = "Distance (km)",
                                              "Average speed" = "Average speed (km/h)",
                                              "Top speed" = "Top speed (km/h)",
                                              "Altitude up" = "Altitude up (m)",
                                              "Altitude down" = "Altitude down (m)",
                                              "Average climb" = "Average climb (%)",
                                              "Average descent" = "Average descent (%)",
                                              "Max climb" = "Max climb (%)",
                                              "Max descent" = "Max descent (%)")
                                    ),
                        plotOutput(outputId = "dayplot")
                        ),
                tabItem(tabName="about",
                        fluidRow(
                            box(title="About", 
                                "In the summer of 2014, I bought a bicycle and a 
                                flight ticket to Miami. Almost 7000 km later, I 
                                rode across the Golden Gate Bridge and down the 
                                steep hills of Lombard Street. Every night along 
                                the way, when setting up camp, I would note down 
                                the numbers collected on my bike computer during 
                                the day. This is an experimental attempt at 
                                visualizing some of those numbers.")
                        )
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
        
        output$dayplot <- renderPlot({
            if (input$yvar == "Distance (km)") {
                plotdata <- data.frame(day = data$day, var = data$distance, State=data$state)
            } else if (input$yvar == "Average speed (km/h)") {
                plotdata <- data.frame(day = data$day, var = data$avg_speed, State=data$state)
            } else if (input$yvar == "Top speed (km/h)") {
                plotdata <- data.frame(day = data$day, var = data$top_speed, State=data$state)
            } else if (input$yvar == "Altitude up (m)") {
                plotdata <- data.frame(day = data$day, var = data$alt_up, State=data$state)
            } else if (input$yvar == "Altitude down (m)") {
                plotdata <- data.frame(day = data$day, var = data$alt_down, State=data$state)
            } else if (input$yvar == "Average climb (%)") {
                plotdata <- data.frame(day = data$day, var = data$avg_climb, State=data$state)
            } else if (input$yvar == "Average descent (%)") {
                plotdata <- data.frame(day = data$day, var = data$avg_descent, State=data$state)
            } else if (input$yvar == "Max climb (%)") {
                plotdata <- data.frame(day = data$day, var = data$max_climb, State=data$state)
            } else if (input$yvar == "Max descent (%)") {
                plotdata <- data.frame(day = data$day, var = data$max_descent, State=data$state)
            } else if (input$yvar == "Total distance (km)") {
                plotdata <- data.frame(day = data$day, var = cumsum(ifelse(is.na(data$distance), 0, data$distance)), State=data$state)
            } else if (input$yvar == "Total altitude up (m)") {
                plotdata <- data.frame(day = data$day, var = cumsum(ifelse(is.na(data$alt_up), 0, data$alt_up)), State=data$state)
            } else if (input$yvar == "Total altitude down (m)") {
                plotdata <- data.frame(day = data$day, var = cumsum(ifelse(is.na(data$alt_down), 0, data$alt_down)), State=data$state)
            }
            
            p <- ggplot(plotdata, aes(day, var, fill=State, color=State)) +
                geom_bar(stat="identity") +
                xlab("Day") +
                ylab(input$yvar) +
                scale_x_continuous(breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
                scale_fill_brewer(palette="Paired", type="qual") +
                scale_color_brewer(palette="Paired", type="qual")
                # scale_fill_brewer(type="qual")
            print(p)
        })
    }
)




