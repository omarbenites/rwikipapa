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
      menuItem("Observation data", tabName = "observation", icon = icon("user")),
      menuItem("Manual", tabName = "manual", icon = icon("book"))
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
              fluidRow(
                box(
                  title = "Catalog data",
                  id = "tabset1", width = 12,
                  tabPanel(
                    "Tab1",
                    fluidRow(
                      
                      
                      column(width = 4,
                             uiOutput("var_ctl_wkp")
                      ),
                      column(width = 4,
                             uiOutput("grp_ctl_wkp")
                      ),
                      column(width = 4,
                             uiOutput("div_ctl_wkp")
                      )
                    ),
                    fluidRow(
                      
                      column(width = 12,
                             #shinycustomloader::withLoader(plotOutput('plot_ctl_wkp'), type = "html", loader = "loader4"),
                             plotOutput("plot_ctl_wkp"),
                             plotlyOutput("myplot2")
                      )
                    )
                  )
                )
              )
      ),
      
      tabItem(tabName = "observation",
              fluidRow(
                box(
                  title = "Observation data",
                  id = "tabset2", width = 12,
                  tabPanel(
                    "Tab2",
                    fluidRow(
                      column(width = 4,
                             uiOutput("grp1_dsb_wkp")
                      ),
                      column(width = 4,
                             uiOutput("grp2_dsb_wkp")
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
      tabItem(tabName = "manual",
              fluidRow(
              column(width = 12,style = "background-color:#FFFFFF;",
                    includeMarkdown("include.md")
                    #shiny::includeHTML("www/manual_wikipapa.txt")
              )
              )
      )
      
    )
  )
)



