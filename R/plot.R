# 
# b2 <- hdc_summary %>%
#   ggplot(aes(x = xvar, 
#              y = yvar,
#              fill = habito_de_crecimiento)) +
#   geom_col() +
#   geom_text(aes(label = yvar_label),
#             position = position_stack(vjust = 0.5),
#             color = "white",
#             fontface = "bold") +
#   coord_flip() +
#   scale_x_discrete() +
#   scale_fill_manual(breaks =
#                       c("Erecto", "Semi-erecto",
#                         "Decumbente", "Postrado",
#                         "Semi-arrosetado","Arrosetado"),
#                     values =
#                       c("Erecto" = "#BADF26" ,         "Semi-erecto" = "#5CC665",
#                         "Decumbente" = "#22A385"  ,    "Postrado" = "#297C8D",
#                         "Semi-arrosetado" = "#3B548A", "Arrosetado" = "#472576"
#                       )
#   ) +
#   labs(title = NULL)+#)"How good is the education at your school?",
#   theme_minimal() +
#   theme(axis.text.x = element_blank(),
#         axis.title.x = element_blank(),
#         panel.grid = element_blank(),
#         legend.position = "top") 