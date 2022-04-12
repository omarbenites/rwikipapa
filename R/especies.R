
get_summary_category <- function(datos, variable, order = TRUE){
  summary_variable <- summarytools::freq(datos[,variable]) 
  summary_variable <- as.data.frame.matrix(summary_variable) %>% 
                      rownames_to_column(var = "category") %>% 
                      janitor::clean_names() %>% 
                      mutate(
                             category = case_when(
                                         is.na(category)~"Sin identificar",
                                         category=="<NA>"~"Sin identificar",
                                         TRUE~as.character(category)
                                        ) 
                            )
 
  
  
}



mutate_missing_category <- function(data, label){
    datos <- datos %>% mutate( 
                              
                              )
  
  
}






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