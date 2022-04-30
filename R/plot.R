#' Horizontal bar chart
#' @param smry_data summary data
#' @param x x variable
#' @param y y variable
#' @param div_by face wrapp according to an atribute
#' @export

bar_horizontal <- function(smry_data, x, y, div_by =NULL ){
  
  b1 <- ggplot(data = smry_data, 
               aes(x = reorder(stringr::str_wrap(get(x), 20), get(y)), y = get(y))
  ) + 
    geom_col(fill="#ffd450",width = 0.7) +
    geom_text(
      aes(label = paste0(get(y),"%")),  
      hjust = -0.1, color = "black", size = 4,
      fontface = "bold"
    ) +
    #lim(NA, 62)+
    scale_y_continuous(limits = c(NA, 100), expand = c(0.01,0.01),
                       labels = scales::percent_format(accuracy = 1, scale = 1)
    ) +
    #scale_x_continuous(expand = c(.01, .01))+
    scale_fill_identity(guide = "none") +
    coord_flip() +
    xlab("")+
    ylab("Porcentaje por cada especie") + 
    ggtitle(label = "Distribucion of species",subtitle = "Base de datos de WikiPapa.org")+
    theme_minimal() +
    #ylim(NA, 63) +
    theme(
      axis.ticks.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.title = element_text(size = 18, margin = margin(10, 0, 0, 0)),
      plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
      #axis.text.x = element_blank(),
      axis.text.y = element_text(size = 10, color = "#323232")
    )
  # if(!is.null(div_by) || div_by!=""){
  #   b1 <- b1 + facet_wrap(div_by)  
  # }
  b1
  
  
}



#' Stacked Bar Chart
#' @param smry_data summary data
#' @param x group variable
#' @param y freq or count variable
#' @param variable variable or trait
#' @param categories vector of categories
#' @param color_categories color for each category
#' @export

bar_stacked_horizontal <- function(smry_data, x, y, variable,
                                   categories=c("Erecto", "Semi-erecto",
                                     "Decumbente", "Postrado",
                                     "Semi-arrosetado","Arrosetado"),
                                   color_categories = c("#BADF26", "#5CC665",       
                                                       "#22A385", "#297C8D",      
                                                       "#3B548A", "#472576") 
                                   
                                   ){
  
  values <- color_categories
  names(values) <- categories
    
  out <- smry_data %>%
            ggplot(aes(x = get(x), 
                       y = get(y),
                       fill = get(variable))) +
            geom_col() +
            geom_text(aes(label = percent_answers_label),
                      position = position_stack(vjust = 0.5),
                      color = "white",
                      fontface = "bold") +
            coord_flip() +
            scale_x_discrete() +
            #scale_fill_manual(breaks = categories,
            #                 values = values
            #                 ) +
            # scale_fill_manual(breaks =
            #                     c("Erecto", "Semi-erecto",
            #                       "Decumbente", "Postrado",
            #                       "Semi-arrosetado","Arrosetado"),
            #                   values =
            #                     c("Erecto" = "#BADF26" ,         "Semi-erecto" = "#5CC665",
            #                       "Decumbente" = "#22A385"  ,    "Postrado" = "#297C8D",
            #                       "Semi-arrosetado" = "#3B548A", "Arrosetado" = "#472576"
            #                     )
            # ) +        
            
            scale_fill_viridis_d() +
            labs(title = NULL)+#)"How good is the education at your school?",
            xlab("")+
            ylab("Porcentaje por cada especie") + 
            ggtitle(label = "Distribucion of species",subtitle = "Base de datos de WikiPapa.org")+
            
    
    
    
            #x = NULL,
            #fill = NULL) +
            theme_minimal() +
            theme(axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  panel.grid = element_blank(),
                  legend.position = "top") 
  
}
#example
#hdc_summary <- get_summary_groupvar(ctl, group = Tuber_shape, Frost_tolerant)
#v1 <- bar_stacked_horizontal(hdc_summary, "Tuber_shape", "percent_answers", "Frost_tolerant")
#

#source
#como usar ggplot en shiny: get y data[[""]]
#https://stackoverflow.com/questions/63734097/why-do-i-have-to-use-aes-string-with-ggplot-in-shiny

#' Plot box
#' @param variable variable
#' @param group group
#' @param dots character Whether yes, show dots
#' @param facet character facet off  
#' @param dfr data
#' @export

plot_box <- function(variable, group = NULL, dots = c("no", "yes"), 
                     facet="", dfr) {
  
  dots <- match.arg(dots)
  
  if (dots == "no") {
    if(is.null(group)) {
     out <-  ggplot(dfr, aes_string(shQuote(""), variable)) +
        geom_boxplot(na.rm = TRUE) +
        xlab("")
    } else {
      out <- ggplot(dfr, aes_string(group, variable, color = group)) +
        geom_boxplot(na.rm = TRUE)
      
    }
    
  } else {
    
    if(is.null(group)) {
     out <- ggplot(dfr, aes_string(shQuote(""), variable)) +
        geom_boxplot(na.rm = TRUE) +
        xlab("") +
        geom_jitter(width = 0.35)
      
    } else {
      out <- ggplot(dfr, aes_string(group, variable,color = group)) +
        geom_boxplot(na.rm = TRUE) +
        geom_jitter(width = 0.35)
    }
  }
  
  out + theme_minimal() +
        theme(axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              panel.grid = element_blank(),
              legend.position = "top") 
  
  if(facet!=""){
   out +  facet_wrap(facet)  
  }
  out
  
}




