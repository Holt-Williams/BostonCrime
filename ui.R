#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(highcharter)
library(wordcloud2)

# Define UI for application that draws a histogram
htmlTemplate("template.html",
             button = actionButton("action", "Action"),
             
             plottest = plotOutput("distPlot"),
             mindate = textOutput("minDate", inline =TRUE),
             maxdate = textOutput("maxDate", inline =TRUE),
             district = textOutput("district", inline =TRUE),
             district2 = textOutput("district2", inline =TRUE),
             streetname = textOutput("streetName", inline =TRUE),
             
             nrows = textOutput("nRows", inline =TRUE),
             nrowsunique = textOutput("nRowsUnique", inline =TRUE),
             countdistricts = textOutput("countDistricts", inline =TRUE),
             
             dangerstreets = plotOutput("dangerStreets"),
             sliderDate =  dateRangeInput("daterange", "",
                                          start = "2015-06-15",
                                          end   = "2020-06-16",
                                          min    = "2015-06-15",
                                          max    = "2020-06-16"
                                          ),
             
             topstreets = selectInput(inputId = "topstreets", label = "Select Street", choices = NULL),
             offensegroupsstreets = plotOutput("offensegroupsStreets"),
             
             districtselector = selectInput(inputId = "district", label = "", choices = NULL),
             
             mymap = plotOutput("mymap",  width = "100%"),
             
             timeplot = plotOutput("timePlot"),
             
             
             incidentyear= plotOutput("incidentsYear"),
             
             shootingplot= plotOutput("shootingPlot"),
             sliderDate2 =  dateRangeInput("daterange2", "Date range:",
                                          start = "2015-06-15",
                                          end   = "2020-06-17",
                                          min    = "2015-06-15",
                                          max    = "2020-06-17"
                                        ),
             
            shootingplotbar= highchartOutput("shootingPlotBar"),
            radiobox = radioButtons("radio", label = h3("Radio buttons"),
                          choices = list("No Highlights" = "1", "Highlight Increasing Offenses" = "2")) 
             
             ) 

