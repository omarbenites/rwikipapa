library(tidyverse)
library(googlesheets4)
library(summarytools)
library(janitor)

library(cowplot) # for theme_minimal_hgrid()
library(colorspace) # for darken()

library(plotly)
library(systemfonts)
library(extrafont)

link_db_catalogo_variedades <- "https://docs.google.com/spreadsheets/u/1/d/1QW-w6ZMHnV59VbzLr6XN_RPvPcHTX-bMZoPWfFDAkGE/edit?usp=drive_web&ouid=105557885030805715232"
datos <- googlesheets4::read_sheet(link_db_catalogo_variedades) %>% 
                        janitor::clean_names()

source("R/especies.R")

# Analsis exploratorio ----------------------------------------------------

# 1. Hacer un resumen estadistico por variable
## 1.1 Si variable tiene categorias, ver las categorias
species <- get_species_ctlg(datos, "especie")
  
 variable <- "especie"
  #1.1.1 Disgregar los datos por categorias
  #1.1.2 Ver la cantidad de datos por categorias 
  #1.1.3 Ver la cantidad de datos faltantes
summary_variable <- get_summary_category(datos, variable)                 

  #1.1.4 Visualizar la variable con los datos faltantes
visdat::vis_miss(datos[,"especie"])


# 1.1.5.Realizar el grafico segun el objetivo
##Objetivo: ver la cantidad de grupos


summary_variable <- summary_variable %>% 
                              arrange(desc(freq)) %>% 
                              mutate(across(.cols = 3:6 , round, 1 ))

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

b1 <- ggplot(data = summary_variable[-1,], 
             aes(x = reorder(stringr::str_wrap(category, 20), percent_total), y = percent_total)
             ) + 
  geom_col(fill="#E1B378",width = 0.7) +
  geom_label(
            aes(label = paste0(percent_total,"%")),  
                hjust = 0, color = "black", size = 4,
                fontface = "bold", family = "Calibri"
            ) +
  #scale_y_continuous(labels = )+
  coord_flip() +
  xlab("Especies") + ylab("Porcentaje") +
  theme_minimal()+
  ylim(NA, 63) +
  theme(
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  )
  
  theme(axis.text.x = element_text(size=4, color = "black"), axis.text.y = element_text(size=4))+ 
theme(
  plot.title = element_text(size = 18, margin = margin(10, 0, 0, 0)),
  plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
  panel.background = element_rect(fill = NA),
  panel.grid.major = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 11, margin = margin(0, 5, 0, 0)),
) 

  # geom_text(
  #           aes(label = scales::percent(percent_total, accuracy = 1)),
  #           hjust = 1.5
  #          ) + 
  #coord_flip() 
b1



# 1.2 Si la variables es numerica









##Sources
#1.Encajar labels largos en el gráfico
#https://stackoverflow.com/questions/41568411/how-to-maintain-size-of-ggplot-with-long-labels
#encajar texto encima de las barras
#https://stackoverflow.com/questions/40211451/geom-text-how-to-position-the-text-on-bar-as-i-want

#https://www.cedricscherer.com/2021/07/05/a-quick-how-to-on-labelling-bar-graphs-in-ggplot2/

#https://thomasadventure.blog/posts/labels-ggplot2-bar-chart/