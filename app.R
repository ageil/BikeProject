####################
### Bike project ###
####################

library(shiny)
library(shinydashboard)
library(googlesheets)
library(leaflet)
library(ggplot2)
library(scales)
library(lubridate)


# Load data from Google
bike <- gs_key("1lGbXMISTa2iBD5xEwufN6cj1BCoLpwTQazxouSBnM0s") # register googlesheet
data <- gs_read(bike, range="A1:S79") # load data from googlesheet; specific rows to avoid bug


# Format
data$time <- hms(data$time) # cannot ggplot type "period"?
data$date <- mdy(data$date)
geodata <- subset(data, complete.cases(data[,18:19])) # only complete lat/lng (leaflet doesnt handle NA yet)
geodata$popup <- ifelse(is.na(geodata$description)==TRUE, 
                        yes=paste0("Day", " ", geodata$day, " ", "(", geodata$date, ")", "<br>",
                                   geodata$place, ", ", geodata$state), 
                        no=paste0("Day", " ", geodata$day, " ", "(", geodata$date, ")", "<br>",
                                  geodata$place, ", ", geodata$state, "<br>",
                                  geodata$description))

shinyApp(
    ui <- dashboardPage(
        dashboardHeader(title="Cycling across America",
                        titleWidth=240),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Map", tabName="map", icon=icon("globe")),
                menuItem("Charts", tabName="charts", icon=icon("area-chart")),
                menuItem("About", tabName="about", icon=icon("info"))
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName="map",
                        fluidRow(box(width=12, 
                                     leafletOutput("myMap", 
                                                   width="100%", 
                                                   height="500")
                                     )
                                 ),
                        fluidRow(box(width=6, 
                                     title="Visualizing a long bicycle ride", 
                                     "In the summer of 2014, I bought a bicycle 
                                     and a flight ticket to Miami. Almost 7000 km 
                                     later, I rode across the Golden Gate Bridge 
                                     and over the steep hills of Lombard Street, 
                                     San Francisco.", 
                                     br(), br(), 
                                     "Every night along the way, when setting up 
                                     camp, I would mark down the numbers collected 
                                     on my bike computer during that day and estimate
                                     my position on the map.", 
                                     br(), br(), 
                                     "This is an initial, and very experimental 
                                     attempt at visualizing some of those numbers."),
                               box(width=6,
                                   title="Navigating this website",
                                   "The red dots in the map above indicate all the 
                                   place places I've been staying for the night 
                                   during the trip. You can click the points to see
                                   the exact name of the place along with any brief 
                                   notes for that day.",
                                   br(), br(), 
                                   "If you want to further explore some of the data 
                                   connecting these points, the 'Charts'-tab on the 
                                   sidebar to the left will allow you to do so. 
                                   Finally, if you're more curious about this website 
                                   or who I am, check out the 'About'-tab.",
                                   br(), br(), 
                                   "Keep in mind, you can always close the sidebar 
                                   to enlarge the map by clicking the little 
                                   'menu'-icon in the bar at the very top.")
                               )
                        ),
                tabItem(tabName="charts",
                        tags$h2("Charts"),
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
                        box(width=12, 
                            plotOutput(outputId = "dayplot", 
                                       width="100%",
                                       height="400"))
                        ),
                tabItem(tabName="about",
                        fluidRow(
                            box(width=6, 
                                title="Who am I?", 
                                "My name is Anders, and when I'm not riding a bicycle
                                around the world, I'm a political science student
                                at Aarhus University, Denmark.",
                                br(), br(),
                                "Lately, I've been very engaged in the world of 
                                data science and learning R. In fact, this website
                                is part of my course project for the 
                                'Data Products'-class in the Data Science
                                Specialization offered by Johns Hopkins University 
                                at Coursera.",
                                br(), br(),
                                "My plan is to gradually expand on this basic website
                                framework, so it may be used as a statistical platform 
                                for future bicycle trips."
                                ),
                            box(width=6,
                                title="How does it work?",
                                "This entire website was built with the statistical
                                programming software R using Shiny. The data is
                                automatically collected from a Google spreadsheet,
                                and updated whenever new data is available.",
                                br(), br(),
                                "My intention is to build upon this framework,
                                so that I - during future trips - can just type
                                the daily data into a spreadsheet on my iPad, 
                                and have it automatically update the website 
                                with the more current information, whenever 
                                the iPad gains access to the internet.",
                                br(), br(),
                                "Perhaps in the more distant future, I'll 
                                invest in a GPS that can track my actual route,
                                rather than just my manually estimated campsites
                                to further increase the precision of the data 
                                (and ease the burden on me).")
                            )
                        )
                    )
                )
        ),

    
    
    server <- function(input, output) {
        
#         output$cal <- renderMenu({
#             sidebarMenu(
#                 menuItem("Cal item", icon=icon("calendar"))
#             )
#         })
        
        map <- leaflet() %>% 
            addProviderTiles("Thunderforest.Outdoors",
                             options=providerTileOptions(noWrap=TRUE)) %>%
            addCircleMarkers(lng = geodata$longitude, 
                             lat = geodata$latitude, 
                             radius=2, 
                             color="red", 
                             popup= geodata$popup)
        
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
                plotdata <- data.frame(day = data$day, var = abs(data$avg_descent), State=data$state)
            } else if (input$yvar == "Max climb (%)") {
                plotdata <- data.frame(day = data$day, var = data$max_climb, State=data$state)
            } else if (input$yvar == "Max descent (%)") {
                plotdata <- data.frame(day = data$day, var = abs(data$max_descent), State=data$state)
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
                scale_y_continuous(breaks=pretty_breaks(10)) +
                scale_fill_brewer(palette="Paired", type="qual") +
                scale_color_brewer(palette="Paired", type="qual")
            print(p)
        })
    }
)




