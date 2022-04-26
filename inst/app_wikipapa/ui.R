library(shinydashboard)
library(tidyverse)
library(rwikipapa)
library(googlesheets4)
library(summarytools)
library(janitor)
library(scales) #para la escala o unidades de escala
library(cowplot) # for theme_minimal_hgrid()
library(colorspace) # for darken()
library(plotly)
library(systemfonts)
library(extrafont)
library(jsonlite)
library(curl)
library(anytime)
library(magrittr)
library(flextable)
library(dplyr)


dashboardPage(
    title="WikiPAPA Dashboard",
    
    dashboardHeader(
        title = tags$a(href='https://wikipapa.org/',
                       tags$img(src='wikipapa-logo.png', height = 30))
    ),
    dashboardSidebar(
        sidebarMenu(
            br(),
            #menuItem("About", tabName = "about", icon = icon("home")),
            menuItem("Catalog data", tabName = "catalog", icon = icon("leaf")),
            menuItem("Observation data", tabName = "observation", icon = icon("user"))
        )
    ),
    dashboardBody(
        
        tags$head(tags$style(HTML(
            '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
        tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Statistical dashboard </span>\');
      })
     ')),
        
        tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #E7154A;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #E7154A;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #E7154A;
                              }        

        
                              '))),
        
        
        tabItems(
            tabItem(tabName = "about",
                    h2("About WikiPAPA dashboard")
            ),
            
            tabItem(tabName = "catalog",
                    h2("Catalog data"),
                    fluidRow(
                        tabBox(
                            title = "Catalog data",
                            id = "tabset1", height = "500px", width = 12,
                            tabPanel(
                                "Tab1",
                                fluidRow(
                                    column(width = 4,
                                           uiOutput("grp1_dsb_wkp")
                                           #selectInput("region", "Region:", 
                                           #   choices=colnames(WorldPhones))
                                           
                                    ),
                                    column(width = 4,
                                           uiOutput("grp2_dsb_wkp")
                                           # selectInput("variable2", "Variable2:",
                                           #             c("Cylinders" = "cyl",
                                           #               "Transmission" = "am",
                                           #               "Gears" = "gear"))
                                    ),
                                    column(width = 4,
                                           selectInput("variable3", "Variable3:",
                                                       c("Cylinders" = "cyl",
                                                         "Transmission" = "am",
                                                         "Gears" = "gear"))
                                    )
                                ),
                                fluidRow(
                                    column(width = 12,
                                           plotOutput("bargraph")
                                    )
                                )
                            )
                        )
                    )
            ),
            
            tabItem(tabName = "observation",
                    h2("Observation data"),
                    fluidRow(
                        tabBox(
                            title = "Observation data",
                            id = "tabset1", height = "500px", width = 12,
                            tabPanel(
                                "Tab1",
                                "First tab content"
                            )
                        )
                    )
            )
        )
    )
)



