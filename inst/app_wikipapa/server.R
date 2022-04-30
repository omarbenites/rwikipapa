server <- function(input, output, session) {
    
    values <- reactiveValues(fileInput = NULL)
    
    user_data <- reactiveValues(users = NULL)
    userurl <- "https://wikipapa.org/api/variety-observation/users"
    
    varie_values <- reactiveValues(variedades = NULL)
     
    observe({
        
        shiny::withProgress(message = "Loading Data from WikiPapa",value= 0,{
            
            #NOTE: To use pepa report package we need R 3.3.0 or more.
            #NOTE Finally, we always need pandoc installer.
            #ToDo: In case of poor conection print a message and do not show anything
            
            #Get variety data
            user_data$data <- fromJSON(userurl)$data
            
            
            
            incProgress(1/5, detail = paste("..."))
            
            #establish connection
            obs_data <- get_observational_data(url = "https://wikipapa.org/api/export/",
                                             call = "observations-data",
                                             idate = "2022-04-11"
                                             )
            
            varie_data <- get_catalogue_data(url = "https://wikipapa.org/api/export/",
                               call="varieties-data"
                               )
            varie_values$hot_varie <- varie_data
            
            incProgress(3/5, detail = paste("..."))
            out <- obs_data #list(sp_base_credentials  = sp_base_credentials , trial_table = trial_table)
            incProgress(5/5, detail = paste("..."))
            
            values$hot_bdata <- out
        })
        
        output$grp1_dsb_wkp  <- renderUI({
            
            #req(input$connect_single_sbase)
            
            wkp_obs_data <- values$hot_bdata
            variables <- names(wkp_obs_data)
            variables <- c("afiliacion" , "sex" , "is_original",
                           "variety_name", "username", "user_id",
                           "farmer_name")
            
            selectInput('wkp_grp1', 'Select group', c(Choose='', variables), selectize=TRUE)
            
        })
        
        
        output$grp2_dsb_wkp  <- renderUI({
            
            #req(input$connect_single_sbase)
            
            wkp_obs_data <- values$hot_bdata
            variables <- names(wkp_obs_data)
            variables <- c("afiliacion" , "sex" , "is_original",
                           "variety_name", "username", "user_id",
                           "farmer_name")
            
            selectInput('wkp_grp2', 'Select variable', c(Choose='', variables), selectize=TRUE)
            
        })
        
    })
    
################# Observational Data ##########################
        
    output$bargraph <- renderPlot({
        
        req(input$wkp_grp1)
        
        obs_data <- values$hot_bdata
        
        if(input$wkp_grp1=="afiliacion"){
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
                     subtitle = "Bioversity Contest",
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
            
           out <-  vcaf_graf
        }     
                
       if(input$wkp_grp1=="sex"){
            
            sex_data <- obs_data %>% filter(sex!="anonymous", sex!="O", !is.na(sex), sex!="")
            
            psex_sum <- get_summary_varcat(sex_data, "sex") %>%  
                                    filter(category!="Sin identificar")
            psex_sum$category <- c("Masculine", "Femenine")
            
            psex_graf <- ggplot(data=psex_sum)+       # reference to data
                geom_col(                 # geometry - a shape
                    aes(x= category,   # aesthetics - x, y, and color values
                        y=percent_valid, fill= category)   
                )+
                geom_text(
                    aes(x = category,y=percent_valid ,label =  paste0(percent_valid,"%")),  
                    vjust = -0.5, color = "black", size = 4,
                    fontface = "bold"
                ) +
                scale_y_continuous(limits = c(NA, 100)) +
                
                #coord_flip() +
                #  +
                #scale_x_continuous(expand = c(.01, .01))+
                #scale_fill_identity(guide = "none") +
                scale_color_manual(labels = c("Masculine", "Femenine"))+
                xlab("Sex")+
                ylab("Percentage") + 
                labs(title = "Distribution of biodiversity potato farmers by sex",
                     subtitle = "Bioversity Contest",
                     caption= 
                         paste0( "N.Obs=", nrow(sex_data),". Source: wikipapa.org" ))+
                theme_minimal() +
                #ylim(NA, 63) +
                theme(
                    axis.ticks.x = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    plot.title = element_text(size = 14, margin = margin(10, 0, 0, 0)),
                    plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0),
                                                 color = "gray"),
                    #axis.text.x = element_blank(),
                    axis.text.y = element_text(size = 10, color = "#323232")
                )+
                guides(fill=guide_legend(title=""))
            out <-  psex_graf
            #ggplotly(psex_graf)
       }
            
        if(input$wkp_grp1=="is_original"){
            destnew_sum <- get_summary_varcat(obs_data, catvar = "is_original") %>% 
                                    filter(category!="Sin identificar") %>% 
                                    mutate(proc = 
                                               case_when(
                                                    category == "FALSE"~"New variety",
                                                    category == "TRUE"~"Standardized variety",
                                           )  
                                    )
            destnew_graf <- ggplot(data=destnew_sum)+       # reference to data
                geom_col(                 # geometry - a shape
                    aes(x= proc,   # aesthetics - x, y, and color values
                        y=percent_valid, fill= proc)   
                )+
                geom_text(
                    aes(x = proc, y=percent_valid ,
                        label = paste0(percent_valid,"%", " (", freq, ")" ) ),  
                    vjust = -0.5, color = "black", size = 4,
                    fontface = "bold"
                ) +
                scale_y_continuous(limits = c(NA, 100 ))+ 
                #coord_flip() +
                #  +
                #scale_x_continuous(expand = c(.01, .01))+
                #scale_fill_identity(guide = "none") +
                xlab("Variety")+
                ylab("Percentage") + 
                labs(title = "Distribution of standardized varieties and new varieties",
                     subtitle = "Bioversity Contest",
                     caption= paste0( "N.Obs=", nrow(obs_data),". Source: wikipapa.org" ))+
                theme_minimal() +
                #ylim(NA, 63) +
                theme(
                    axis.ticks.x = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    plot.title = element_text(size = 14, margin = margin(10, 0, 0, 0)),
                    plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0),
                                                 color = "gray"),
                    #axis.text.x = element_blank(),
                    axis.text.y = element_text(size = 10, color = "#323232")
                )+
                guides(fill=guide_legend(title=""))
            out <-  destnew_graf
            
        }
        
        if(input$wkp_grp1=="variety_name"){
            
            varcolect_sum <-  get_summary_varcat(obs_data, catvar = "variety_name")
            
            varcolect_top10 <- varcolect_sum %>% 
                                        slice(1:10) %>%
                                        mutate(category =  str_to_title(category)) 
            
            varcolect_graf <- ggplot(data = varcolect_top10, 
                                     aes(x = reorder(category, freq), y = freq)) +
                geom_bar(stat = "identity",fill="#f5b51e") +
                geom_text(
                    aes(label = freq),  
                    hjust = -0.5, color = "black", size = 4,
                    fontface = "bold"
                ) + 
                coord_flip()+
                scale_y_continuous(limits = c(NA, max(varcolect_top10$freq)+5), expand = c(0.01,0.01)) +
                #scale_x_continuous(expand = c(.01, .01))+
                scale_fill_identity(guide = "none") +
                xlab("Number of collected varieties")+
                ylab("Variety") + 
                
                labs(title = "Top 10 of collected potato varieties",
                     subtitle = "Bioversity Contest",
                     caption= paste0( "N.Obs=", nrow(obs_data),". Source: wikipapa.org" ))+
                theme_minimal() +
                #ylim(NA, 63) +
                theme(
                    axis.ticks.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    plot.title = element_text(size = 14, margin = margin(10, 0, 0, 0)),
                    plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
                    #axis.text.x = element_blank(),
                    axis.text.y = element_text(size = 10, color = "#323232")
                )#+
            #      gghighlight(user_id == 57, user_id ==81 )
           out <-  varcolect_graf
            
            
        } 
        
        if(input$wkp_grp1=="afiliacion" && input$wkp_grp2=="username"){
            
            pafcol_group <- get_summary_groupvar(dfr = obs_data, group = input$wkp_grp1, variable = input$wkp_grp2) %>% 
                                    filter(afiliacion!="Otros varios")
            
            pafcol_sum <- pafcol_group %>% group_by(afiliacion) %>% 
                mutate(ncolegio = n()) %>% select(afiliacion, ncolegio) %>% distinct() %>%                          ungroup() %>% 
                mutate(percent_valid = ncolegio/sum(ncolegio)*100) %>% 
                as.data.frame() %>% 
                mutate(percent_valid=round(percent_valid,2))
            
            pafcol_graf <- ggplot(data = pafcol_sum, aes(x = reorder(afiliacion, percent_valid),
                                                        y = percent_valid)) +
                geom_bar(stat = "identity",fill="#428b7b") +
                geom_text(
                    aes(label = paste0(percent_valid,"%")),  
                    hjust = -0.1, color = "black", size = 4,
                    fontface = "bold"
                ) + 
                coord_flip()+
                scale_y_continuous(limits = c(NA, 100), expand = c(0.01,0.01)) +
                #scale_x_continuous(expand = c(.01, .01))+
                scale_fill_identity(guide = "none") +
                xlab("Number of participants")+
                ylab("Participants") + 
                labs(title = "Distribution of Participants by Institutions",
                     subtitle = "Bioversity Contest",
                     caption= paste0( "N.Obs=", nrow(obs_data),". Source: wikipapa.org"))+
                theme_minimal() +
                #ylim(NA, 63) +
                theme(
                    axis.ticks.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    plot.title = element_text(size = 13, margin = margin(10, 0, 0, 0)),
                    plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
                    #axis.text.x = element_blank(),
                    axis.text.y = element_text(size = 10, color = "#323232")
                )
            
           out <-  pafcol_graf
        }
        
        if(input$wkp_grp1=="user_id"){
            
            user_data <- user_data$data
            
            cpo_trim <- obs_data %>% 
                filter(afiliacion!="UN. Daniel Alcides Carrion") %>% 
                filter(afiliacion!="Cesar Pérez Arauco") %>% 
                filter(afiliacion!="Otros varios")
            
            cpo <- get_summary_varcat(cpo_trim, "user_id")
            names(cpo)[1] <- "user_id"
            cpo[,"user_id"] <- as.integer(cpo[,"user_id"])
            join_cpo_users <- left_join(user_data,cpo) %>% 
                filter(!is.na(freq)) %>% 
                arrange(desc(freq)) %>% 
                filter(username!="Ana Micaela") %>% 
                filter(username!="ana") %>% 
                filter(username!="yhedali")  
            
            cop_top10 <- join_cpo_users %>% slice(1:10) %>% 
                mutate(
                    first_name =case_when(
                        first_name == "rosendo"~"Gabriel",
                        TRUE ~ as.character(first_name)
                    )
                ) %>% 
                mutate(Nombre = paste(first_name, last_name) %>% str_to_title())
            
            b1 <- ggplot(data = cop_top10, aes(x = reorder(Nombre, freq), y = freq)) +
                geom_bar(stat = "identity",fill="#5b83c2") +
                geom_text(
                    aes(label = freq),  
                    hjust = -0.1, color = "black", size = 4,
                    fontface = "bold"
                ) + 
                coord_flip()+
                scale_y_continuous(limits = c(NA, max(cop_top10$freq)+13), expand = c(0.01,0.01)) +
                #scale_x_continuous(expand = c(.01, .01))+
                scale_fill_identity(guide = "none") +
                xlab("Number of potato observations") +
                ylab("Participants") + 
                
                labs(title = "Participants with the most collected potato observations",
                     subtitle = "Bioversity Contest",
                     caption= paste0( "N.Obs=", nrow(obs_data),". Source: wikipapa.org" ))+
                theme_minimal() +
                #ylim(NA, 63) +
                theme(
                    axis.ticks.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    plot.title = element_text(size = 14, margin = margin(10, 0, 0, 0)),
                    plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
                    #axis.text.x = element_blank(),
                    axis.text.y = element_text(size = 10, color = "#323232")
                )#+
            #      gghighlight(user_id == 57, user_id ==81 )
            
            out <- b1
            
        }
        
        if(input$wkp_grp1=="farmer_name"){
            
            nobagri_sum <- get_summary_varcat(dfr = obs_data, catvar = "farmer_name")  %>%                       filter(category!="Sin identificar") %>% 
                filter(category!="Otros Varios")
            
            nobagri_graf <- ggplot(data = nobagri_sum, aes(x = reorder(category, freq), y = freq)) +
                geom_bar(stat = "identity",fill="#5b83c2") +
                geom_text(
                    aes(label = freq),  
                    hjust = -0.1, color = "black", size = 4,
                    fontface = "bold"
                ) + 
                coord_flip()+
                scale_y_continuous(limits = c(NA, max(nobagri_sum$freq)+13), expand = c(0.01,0.01)) +
                #scale_x_continuous(expand = c(.01, .01))+
                scale_fill_identity(guide = "none") +
                xlab("Number of observations") +
                ylab("Farmers") + 
                
                labs(title = "Number of observations per farmer",
                     subtitle = "Bioversity Contest",
                     caption= paste0( "N.Obs=", nrow(obs_data),". Source: wikipapa.org" ))+
                theme_minimal() +
                #ylim(NA, 63) +
                theme(
                    axis.ticks.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    plot.title = element_text(size = 14, margin = margin(10, 0, 0, 0)),
                    plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
                    #axis.text.x = element_blank(),
                    axis.text.y = element_text(size = 10, color = "#323232")
                )#+
            #      gghighlight(user_id == 57, user_id ==81 )
            
            out <- nobagri_graf
            
            
        }
        
        
        out
        # barplot(WorldPhones[,input$region]*1000, 
        #         main=input$region,
        #         ylab="Number of Telephones",
        #         xlab="Year")
    })
    
###########End Observational Data ###    
    
##### Catalogue Data ###########################################
    
    output$var_ctl_wkp  <- renderUI({
        
        #req(input$connect_single_sbase)
        
        varie_data <- varie_values$hot_varie
        variables <- names(varie_data)
        #variables <- c("afiliacion" , "sex" , "is_original",
        #               "variety_name", "username", "user_id",
        #               "farmer_name")
        
        selectInput('sel_wkp_ctl_var', 'Select variable', 
                    c(Choose='', variables), selectize=TRUE)
        
    })
    
    
    output$grp_ctl_wkp  <- renderUI({
        
        varie_data <- varie_values$hot_varie
        variables <- names(varie_data)
        
        #wkp_obs_data <- values$hot_bdata
        #variables <- names(wkp_obs_data)
        #variables <- c("afiliacion" , "sex" , "is_original",
        #               "variety_name", "username", "user_id",
        #               "farmer_name")
        
        selectInput('sel_wkp_ctl_grp', 'Grouped by', c(Choose='', variables), selectize=TRUE)
        
    })
    
    output$div_ctl_wkp  <- renderUI({
        
        varie_data <- varie_values$hot_varie
        variables <- names(varie_data)
        
        #wkp_obs_data <- values$hot_bdata
        #variables <- names(wkp_obs_data)
        #variables <- c("afiliacion" , "sex" , "is_original",
        #               "variety_name", "username", "user_id",
        #               "farmer_name")
        
        selectInput('sel_wkp_ctl_div', 'Divided by', c(Choose='', variables), selectize=TRUE)
        
    })
    
    
    ss <- reactive({
        
        req(input$sel_wkp_ctl_var)
        
        
        varie_data <- varie_values$hot_varie
        ctl <- varie_data
        quant <- c("tuber_yield",
                   "zinc_concentration_in_fresh_weight",
                   "potassium_concentration_in_fresh_weight",
                   "phosphorus_concentration_in_fresh_weight",
                   "vitamin_C_concentration_in_fresh_weight")
        # if(is.element(input$sel_wkp_ctl_var,quant) && class(ctl[,input$sel_wkp_ctl_var])=="numeric" && input$sel_wkp_ctl_grp!="" && 
        #    input$sel_wkp_ctl_div!="") {
        #     
        #     varie_data <- varie_values$hot_varie
        #     
        #     out <-  plot_box(variable = input$sel_wkp_ctl_var, 
        #                      group = input$sel_wkp_ctl_grp, 
        #                      dots = "yes", 
        #                      facet = input$sel_wkp_ctl_div,
        #                      dfr = varie_data
        #     )
            
        #}
        
        
        if(!is.null(input$sel_wkp_ctl_var) && class(varie_data[,input$sel_wkp_ctl_var])=="character"){
          
            smry_varcat <- get_summary_varcat(dfr =varie_data, catvar = input$sel_wkp_ctl_var)
            #print(smry_varcat)
            
            out <- bar_horizontal(smry_varcat, "category" ,"percent_total")        
            
        }
        if(input$sel_wkp_ctl_var!="" && input$sel_wkp_ctl_grp!="" && class(varie_data[,input$sel_wkp_ctl_var])=="character") {
            
            #req(input$sel_wkp_ctl_grp)
            
            varie_data <- varie_values$hot_varie
            
            
            smry_groupvar <- get_summary_groupvar(dfr = varie_data,
                                                  group= input$sel_wkp_ctl_grp , 
                                                  variable= input$sel_wkp_ctl_var
                                                 )
            print(smry_groupvar)
            #categories <-  smry_varcat
            
            out <- bar_stacked_horizontal(smry_groupvar, 
                                          x= input$sel_wkp_ctl_grp, 
                                          y="percent_answers", 
                                          variable=input$sel_wkp_ctl_var
                                         )
        }
       
        if(class(varie_data[,input$sel_wkp_ctl_var])=="numeric") {

            varie_data <- varie_values$hot_varie

            if(input$sel_wkp_ctl_grp!=""){
                out <- ggplot(varie_data, aes_string(shQuote(""), input$sel_wkp_ctl_var, color = input$sel_wkp_ctl_grp) ) +
                    geom_boxplot() + geom_jitter()
            } else{
                out <- ggplot(varie_data, aes_string(shQuote(""), input$sel_wkp_ctl_var) ) +
                    geom_boxplot() + geom_jitter()  
            }
            
            
            if(input$sel_wkp_ctl_div!=""){
              out <-   out +  facet_wrap(input$sel_wkp_ctl_div)
            }
        }
         
        out
           
       
        
    })
    
    output$plot_ctl_wkp<- renderPlot({
        ss()
    })
    
    output$myplot2 <- renderPlotly({
        ggplotly(ss())
    })
    
    
################################################################    
    

    
    
    
        
    
    
    
    
        
}

