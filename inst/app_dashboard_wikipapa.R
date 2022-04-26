link_db_catalogo_variedades <- "https://docs.google.com/spreadsheets/u/1/d/1QW-w6ZMHnV59VbzLr6XN_RPvPcHTX-bMZoPWfFDAkGE/edit?usp=drive_web&ouid=105557885030805715232"
datos <- googlesheets4::read_sheet(link_db_catalogo_variedades) %>% 
               janitor::clean_names()

#source("funciones/summary.R")


library(tidyverse)
library(shiny)
ui <- fluidPage(
  selectInput("var1", "Group by", choices = names(datos),selected = "especie",multiple = FALSE),
  selectInput("var2", "Variable", choices = names(datos),selected = "habito_de_crecimiento"),
  tableOutput("output")
  #shiny::observeEvent()
)
server <- function(input, output, session) {
  
  datos <- reactive({
    
    link_db_catalogo_variedades <- "https://docs.google.com/spreadsheets/u/1/d/1QW-w6ZMHnV59VbzLr6XN_RPvPcHTX-bMZoPWfFDAkGE/edit?usp=drive_web&ouid=105557885030805715232"
    datos <- googlesheets4::read_sheet(link_db_catalogo_variedades) %>% 
      janitor::clean_names()
    
  })
  
  smry_datos <- reactive({
    
    out <- get_summary_varcat(datos(), input$var1, input$var2) 
    out
    
  }) 
  
  output$output <- renderTable(smry_datos())
}

shinyApp(ui,server)
