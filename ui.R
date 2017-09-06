
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(
  
navbarPage("PAT Index GOATs",

tabPanel("Charts",                  
  
  # Application title
  titlePanel("PAT Index's GOAT List - Charts"),
  helpText("After simulating a drill between all teams from 1996 - 2015, these graphs were created to 
            summarize the results.  They show the number of teams in the simulation that had a greater than 
            1% chance of winning or placing in the drill.  Graphs are organized by teams or years.  Organized by Team, the graph shows how many great teams raced for each town.  
            Organized by Year, the graph shows the number of super-competitive teams racing each individual season."
  ),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput('contest', "Select Contest or Overall Place",
                  list("Overall" = "Overall", "Three Man Ladder" = "Three Man Ladder", "B Ladder" = "B Ladder", "C Ladder" = "C Ladder", "C Hose" = "C Hose",
                       "B Hose" = "B Hose", "Efficiency" = "Efficiency", "Motor Pump" = "Motor Pump", "Buckets" = "Buckets"),
                  selected = "Overall"),
      selectInput('x_axis', "Teams or Years", 
                  list("Teams", "Years")),
      selectInput('Top5v1st', "View First or Top 5",
                  list("First Place" = "FirstPlace", "Top 5" = "Top5")),
      br(),
      HTML("<a href='https://patindex.wordpress.com/patindexgoatlist/'>Return to Site</a>")
    ),            
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput('Plot')
    )
  )
  ),
tabPanel("Tables",
         fluidPage(
           titlePanel("PAT Index's Greatest of all Time List"),
           helpText("The table below shows the results of a simulation between all teams between 1996 - 2015.  The figures shown 
                    indicate the percentage of tournaments won by the team in the simulation."),
           fluidRow(
             column(4, offset=1,
                    fluidRow(selectInput("year", label=h3("Choose Year(s)"),
                                         choices = list("All"= 0, 1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,
                                                        2007,2008,2009,2010,2011,2012,2013,2014,2015))
                    ),
                    fluidRow(
                      checkboxGroupInput("checkGroup2", label=h3("Display:"),
                                         choices = list("Chance for First Place" = 1, "Chance for Top 5" = 2),
                                         selected = c(1,2,3,4))),
                    fluidRow(br(),HTML("<a href='https://patindex.wordpress.com/patindexgoatlist/'>Return to Site</a></p>"),br())
                    ),
             column(4,
                    checkboxGroupInput("checkGroup", label=h3("Pick which contests to display"),
                                       choices = list("Overall" = 9, "Three Man Ladder" = 1, "B Ladder" = 2, "C Ladder" = 3, "C Hose" = 4,
                                                      "B Hose" = 5, "Efficiency" = 6, "Motor Pump" = 7, "Buckets" = 8),
                                       selected = 9))
             
             
             ),
           
           fluidRow(
             column(11,offset=.5,
                    DT::dataTableOutput("table")
             )
           )
         )
)))
