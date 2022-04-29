library(tidyverse)
library(googlesheets4)
library(summarytools)
library(janitor)
library(scales)
library(cowplot) # for theme_minimal_hgrid()
library(colorspace) # for darken()

library(plotly)
library(systemfonts)
library(extrafont)

link_db_catalogo_variedades <- "https://docs.google.com/spreadsheets/u/1/d/1QW-w6ZMHnV59VbzLr6XN_RPvPcHTX-bMZoPWfFDAkGE/edit?usp=drive_web&ouid=105557885030805715232"
datos <- googlesheets4::read_sheet(link_db_catalogo_variedades) %>% 
                        janitor::clean_names()

#source("R/especies.R")

# Analsis exploratorio ----------------------------------------------------

# 1. Hacer un resumen estadistico por variable
## 1.1 Si variable tiene categorias, ver las categorias
species <- get_species_ctlg(datos, "especie")
  
 variable <- "especie"
  #1.1.1 Disgregar los datos por categorias
  #1.1.2 Ver la cantidad de datos por categorias 
  #1.1.3 Ver la cantidad de datos faltantes
summary_variable <- get_summary_varcat(datos,catvar = "especie",total = FALSE )                 

  #1.1.4 Visualizar la variable con los datos faltantes
visdat::vis_miss(datos[,"especie"])


# 1.1.5.Realizar el grafico segun el objetivo
##Objetivo: ver la cantidad de grupos



# summary_variable <- summary_variable %>% 
#                               arrange(desc(freq)) %>% 
#                               mutate(across(.cols = 3:6 , round, 1 ))

##Diagrama de barras (conteo de variedades)
b1 <- ggplot(data = summary_variable, aes(x = reorder(category, freq), y = freq)) + 
       geom_bar(stat = "identity",fill="#E6B82C") +
       geom_text(
         aes(label = freq),hjust = -0.5,
         position = position_dodge(width = 1)
         ) + 
       coord_flip() 
b1 <- ggplotly(b1)
b1

##Diagrama de barras (porcentaje de variedades)

summary_variable2 <- get_summary_varcat(dfr = datos,catvar = "especie",total = FALSE )

b1 <- ggplot(data = summary_variable2, 
             aes(x = reorder(stringr::str_wrap(category, 20), percent_total), y = percent_total)
             ) + 
  geom_col(fill="#ffd450",width = 0.7) +
  geom_text(
            aes(label = paste0(percent_total,"%")),  
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
  ggtitle(label = "Distribución de especies",subtitle = "Base de datos de WikiPapa.org")+
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

b1





# 1.2 Si la variables es categorica ordinal (likert)

#source: https://rfortherestofus.com/2021/10/diverging-bar-chart/
#        https://peltiertech.com/diverging-stacked-bar-charts/
#        https://medium.com/nightingale/seven-different-ways-to-display-likert-scale-data-d0c1c9a9ad59
#        https://r-charts.com/part-whole/diverging-bar-chart-ggplot2/


hdc_summary <- get_summary_groupvar(datos, group = especie, habito_de_crecimiento)

# hdc_summary <- datos %>%  group_by(especie, habito_de_crecimiento) %>% 
#                           count(name = "nhdc") %>% 
#                           group_by(especie) %>% 
#                           mutate(percent_answers = nhdc / sum(nhdc)) %>% 
#                           ungroup() %>% 
#                           mutate(percent_answers_label = percent(percent_answers, accuracy = 1))
                              
## Gráfico de resumen por grupo y variable

b2 <- hdc_summary %>%
          ggplot(aes(x = especie, 
                     y = percent_answers,
                     fill = habito_de_crecimiento)) +
          geom_bar() +
          geom_text(aes(label = percent_answers_label),
                    position = position_stack(vjust = 0.5),
                    color = "white",
                    fontface = "bold") +
          coord_flip() +
          scale_x_discrete() +
         # scale_fill_manual(breaks = c("Very bad", "Bad", "Medium", "Good", "Very Good"),
         #            values = c(
         #              "Very bad" = "#9FA1FF",
         #              "Bad" = "#D7D8FF",
         #              "Medium" = "#F1F1F1",
         #              "Good" = "#EAE090",
         #              "Very Good" = "#B9AD00"
         #            )) %>% 
          scale_fill_manual(breaks =
                      c("Erecto", "Semi-erecto",
                        "Decumbente", "Postrado",
                        "Semi-arrosetado","Arrosetado"),
                    values =
                      c("Erecto" = "#BADF26" ,         "Semi-erecto" = "#5CC665",
                        "Decumbente" = "#22A385"  ,    "Postrado" = "#297C8D",
                        "Semi-arrosetado" = "#3B548A", "Arrosetado" = "#472576"
                        )
                      # c("Erecto" = "#9FA1FF" ,         "Semi-erecto" = "#CED1FF",
                      #   "Decumbente" = "#E9EAFF"  ,    "Postrado" = "#F6EEBF",
                      #   "Semi-arrosetado" = "#E4D97F", "Arrosetado" = "#B9AD00"
                      #   )
                       ) +
          #c("#9FA1FF", "#CED1FF", "#E9EAFF", "#F6EEBF", "#E4D97F", "#B9AD00")
          #scale_fill_viridis_d() +
          labs(title = NULL)+#)"How good is the education at your school?",
               #x = NULL,
               #fill = NULL) +
          theme_minimal() +
          theme(axis.text.x = element_blank(),
                axis.title.x = element_blank(),
                panel.grid = element_blank(),
                legend.position = "top") 

#plotly
b2 %>% layout(legend = list(orientation = 'v', x = 1, y = 0.5))



# b22 <- hdc_summary %>%
#                 ggplot(aes(x = especie, 
#                            y = percent_answers,
#                            fill = habito_de_crecimiento)) +
#                 geom_col() +
#                 geom_text(aes(label = percent_answers_label),
#                           position = position_stack(vjust = 0.5),
#                           color = "white",
#                           fontface = "bold") +
#                 coord_flip() +
#                 scale_x_discrete() +
#                 scale_fill_manual(breaks = c("Very bad", "Bad", "Medium", "Good", "Very Good"),
#                                   values = c("#9FA1FF = Erecto",          "#CED1FF = Semi-erecto",    
#                                              "#E9EAFF = Decumbente",      "#F6EEBF = Postrado",       
#                                              "#E4D97F = Semi-arrosetado", "#B9AD00 = Arrosetado"
#                                             )   
#                                   ) +
#                 labs(title = "How good is the education at your school?",
#                      x = NULL,
#                      fill = NULL) +
#                 theme_minimal() +
#                 theme(axis.text.x = element_blank(),
#                       axis.title.x = element_blank(),
#                       panel.grid = element_blank(),
#                       legend.position = "top")


## shiny plotly poner legende encima del grafico
##https://stackoverflow.com/questions/69306154/r-shiny-plotly-ggplotly-moves-the-legend-to-the-right


##Sources
#1.Encajar labels largos en el gráfico
#https://stackoverflow.com/questions/41568411/how-to-maintain-size-of-ggplot-with-long-labels
#encajar texto encima de las barras
#https://stackoverflow.com/questions/40211451/geom-text-how-to-position-the-text-on-bar-as-i-want

#https://www.cedricscherer.com/2021/07/05/a-quick-how-to-on-labelling-bar-graphs-in-ggplot2/

#https://thomasadventure.blog/posts/labels-ggplot2-bar-chart/

#https://spectrum.adobe.com/page/color-for-data-visualization/#Usage-guidelines

#https://blog.datawrapper.de/which-color-scale-to-use-in-data-vis/

#https://stackoverflow.com/questions/35090883/remove-all-of-x-axis-labels-in-ggplot