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

