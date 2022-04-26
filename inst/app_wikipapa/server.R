server <- function(input, output, session) {
    
    values <- reactiveValues(fileInput = NULL)
    
    observe({
        
        shiny::withProgress(message = "Loading Data from WikiPapa",value= 0,{
            
            #NOTE: To use pepa report package we need R 3.3.0 or more.
            #NOTE Finally, we always need pandoc installer.
            #ToDo: In case of poor conection print a message and do not show anything
            
            incProgress(1/5, detail = paste("..."))
            
            #establish connection
            obs_data <- get_observation_data(url = "https://wikipapa.org/api/export/",
                                             call = "observations-data",
                                             idate = "2022-04-11"
                                             )
            incProgress(3/5, detail = paste("..."))
            out <- obs_data #list(sp_base_credentials  = sp_base_credentials , trial_table = trial_table)
            incProgress(5/5, detail = paste("..."))
            
            values$hot_bdata <- out
        })
        
        output$grp1_dsb_wkp  <- renderUI({
            
            #req(input$connect_single_sbase)
            
            wkp_obs_data <- values$hot_bdata
            variables <- names(wkp_obs_data)
            
            selectInput('wkp_grp1', 'Select group', c(Choose='', variables), selectize=TRUE)
            
            
        })
        
        
        output$grp2_dsb_wkp  <- renderUI({
            
            #req(input$connect_single_sbase)
            
            wkp_obs_data <- values$hot_bdata
            variables <- names(wkp_obs_data)
            
            selectInput('wkp_grp2', 'Select variable', c(Choose='', variables), selectize=TRUE)
            
            
        })
        
        
        
    })
    
    
    output$bargraph <- renderPlot({
        
        req(input$wkp_grp1)
        obs_data <- values$hot_bdata
        # Render a barplot
        vcaf_sum <- get_summary_varcat(obs_data,input$wkp_grp1) %>% 
                                filter(category!="Sin identificar")
        
        vcaf_graf <- ggplot(data = vcaf_sum, aes(x = reorder(category, freq), y = freq)) +
            geom_bar(stat = "identity",fill="#6598C7") +
            geom_text(
                aes(label = freq),  
                hjust = -0.1, color = "black", size = 4,
                fontface = "bold"
            ) + 
            coord_flip()+
            scale_y_continuous(limits = c(NA, max(vcaf_sum$freq)+10 ), expand = c(0.01,0.01)) +
            #scale_x_continuous(expand = c(.01, .01))+
            scale_fill_identity(guide = "none") +
            xlab("Institution")+
            ylab("Number of varieties") + 
            labs(title = "Number of observations by institutions",
                 subtitle = "WikiPapa Contest",
                 caption= paste0( "N.Obs=", nrow(obs_data),". Source: wikipapa.org" ))+
            theme_minimal() +
            #ylim(NA, 63) +
            theme(
                axis.ticks.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                plot.title = element_text(size = 14, margin = margin(10, 0, 0, 0)),
                plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0),
                                             color = "gray"),
                #axis.text.x = element_blank(),
                axis.text.y = element_text(size = 10, color = "#323232")
            )
        vcaf_graf
        
        
        # barplot(WorldPhones[,input$region]*1000, 
        #         main=input$region,
        #         ylab="Number of Telephones",
        #         xlab="Year")
    })
    
    
    
    
}

