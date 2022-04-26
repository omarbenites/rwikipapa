#' Mutate missing category's label
#'
#' @param variable data.frame data from the catalogue
#' @param label character new label for the missing category
#' @importFrom dplyr case_when mutate
#' @export 
#' 
mutate_missing_catlabel <- function(dfr, variable, label = "No identificado"){
  
  out <- datos[,variable] %>% pull()
  out <- case_when(
                    is.na(out)~"Sin identificar",
                    out=="<NA>"~"Sin identificar",
                    TRUE~as.character(out)
                  ) 
  dfr[,variable] <- out
#TODO: MODIFICAR OUT EN LA COLUMNA VARIABLE
  
}
  
  
  # dfr <- dfr %>%
  #           mutate(
  #             variable = case_when(
  #                         is.na(variable)~"Sin identificar",
  #                         variable=="<NA>"~"Sin identificar",
  #                         TRUE~as.character(variable)
  #                         ) 
  #           )
          



get_species_ctlg <- function(data, colname = "especie"){
  
  check_missing_data(data)
  check_colname_exist(data,colname)
  
  species <- unique(datos[,colname] %>% unlist())
  #If species == NA, write "no-identificado" non-identified specie
  species <- case_when(is.na(species) ~ "no-identificado", TRUE~ as.character(species))
  
  return(species)
  
} 

check_colname_exist <- function(data, colname){
  if(!is.element(colname,names(data))){
    rlang::abort(message = "Selecionar una variable existente", class = "try-error")
    return()  
  }
}

check_missing_data <- function(data){
  if(nrow(data)==0){
    rlang::abort(message = "No hay datos en la tabla",class= "try-error")
    return() 
  }
}