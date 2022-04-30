library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
df <- tibble(x=seq(20), y=seq(20), type=c(rep("a", 10), rep("b", 10)))

ui <- fluidPage(
  mainPanel(
    plotOutput("myplot" ),
    
    plotlyOutput("myplot2" )
    
  )
)

server <- function(input, output) {
  myplot <- reactive({
    gpl1 <- df%>%
      ggplot(aes(x = x, y = y, fill=type)) +
      geom_col()+
      theme(legend.position="top")+
      xlab("x")+
      ylab("y")+
      labs(title = NULL)
    gpl1
  })
  
  myplot2 <- reactive({
    gpl2 <- df%>%
      ggplot(aes(x = x, y = y, fill=type)) +
      geom_col()+
      theme(legend.position="top")+
      xlab("x")+
      ylab("y")+
      labs(title = NULL)
    ggplotly(gpl2) %>% 
      layout(legend = list(orientation = 'h', x = 0.45, y = 1.1))
  })
  output$myplot <- renderPlot({
    myplot()
  })
  output$myplot2 <- renderPlotly({
    myplot2()
  })
}

shinyApp(ui = ui, server = server)