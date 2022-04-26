library(shinydashboard)

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
                             selectInput("region", "Region:", 
                                         choices=colnames(WorldPhones))
                      ),
                      column(width = 4,
                             selectInput("variable2", "Variable2:",
                                         c("Cylinders" = "cyl",
                                           "Transmission" = "am",
                                           "Gears" = "gear"))
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
                             plotOutput("phonePlot")
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

server <- function(input, output) {
  
  
  observe({
    
    shiny::withProgress(message = "Loading Data from WikiPapa",value= 0,{
      
      #NOTE: To use pepa report package we need R 3.3.0 or more.
      #NOTE Finally, we always need pandoc installer.
      #ToDo: In case of poor conection print a message and do not show anything
      
      incProgress(1/5, detail = paste("..."))
      
      white_list <- brapi::ba_db()
      #establish connection
      incProgress(3/5, detail = paste("..."))
      sp_base_credentials <- white_list$sweetpotatobase
      trial_table <- brapi::ba_trials(con = sp_base_credentials)
      
      out <- list(sp_base_credentials  = sp_base_credentials , trial_table = trial_table)
      incProgress(5/5, detail = paste("..."))
      
      values$hot_bdata <- out
    })
    
  })
  
  
  output$phonePlot <- renderPlot({
    
    # Render a barplot
    barplot(WorldPhones[,input$region]*1000, 
            main=input$region,
            ylab="Number of Telephones",
            xlab="Year")
  })
  
  
  
  
}


