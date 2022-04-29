#' Get obsevational data from WikiPapa Plataform
#' @author Omar Benites
#' @description Observational data from the variety scouting
#' @param url character url path
#' @param call character api call name
#' @param idate character date format yyyy-mm-dd
#' @export
#' 

get_catalogue_data <- function(url="https://wikipapa.org/api/export/", 
                                   call="varieties-data", 
                                   idate = "2022-04-11" ){
  
  
  #obsurl <- "https://wikipapa.org/api/export/observations-data"
  #obs_data<- jsonlite::fromJSON(obsurl)$data 
  ctlurl <- paste0(url,call)
  ctl_data <- jsonlite::fromJSON(ctlurl)$data
  ctl_data <- ctl_data %>% 
                    mutate(Tuber_yield = as.numeric(Tuber_yield)) %>% 
                    #mutate(Iron_concentration_in_fresh_weight = as.numeric(Iron_concentration_in_fresh_weight)) %>%       
                    mutate(Zinc_concentration_in_fresh_weight = as.numeric(Zinc_concentration_in_fresh_weight)) %>%                
                    mutate(Potassium_concentration_in_fresh_weight= as.numeric(Potassium_concentration_in_fresh_weight)) %>% 
                    mutate(Phosphorus_concentration_in_fresh_weight = as.numeric(Phosphorus_concentration_in_fresh_weight)) %>% 
                    mutate(Vitamin_C_concentration_in_fresh_weight = as.numeric(Vitamin_C_concentration_in_fresh_weight)) %>% 
                    janitor::clean_names()
    
  #raw_data <- obs_data
  #ctl_data$observation_date <- anytime::anydate(ctl_data$observation_date)
  ctl_data
  
}

