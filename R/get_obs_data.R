#' Get obsevational data from WikiPapa Plataform
#' @author Omar Benites
#' @description Observational data from the variety scouting
#' @param url character url path
#' @param call character api call name
#' @param idate character date format yyyy-mm-dd
#' @export
#' 

get_observation_data <- function(url="https://wikipapa.org/api/export/", 
                                 call="observations-data", 
                                 idate = "2022-04-11" ){
  #obsurl <- "https://wikipapa.org/api/export/observations-data"
  #obs_data<- jsonlite::fromJSON(obsurl)$data 
  obsurl <- paste0(url,call)
  obs_data <- jsonlite::fromJSON(obsurl)$data  
  raw_data <- obs_data
  obs_data$observation_date <- anytime::anydate(obs_data$observation_date)
  obs_data <- obs_data %>% dplyr::filter(observation_date >= {{idate}}) %>% 
    #filter(!user_id %in% c(48,40,49,38,47,39,35 )) %>% 
    mutate(variety_name = tolower(variety_name)) %>% 
    mutate(is_original = as.character(is_original)) %>% 
    mutate(observation_notes = tolower(observation_notes)) %>% 
    mutate(
      afiliacion = case_when( 
        str_detect(observation_notes,"alfonso ugarte")~ "IE Alfonso Ugarte",
        str_detect(observation_notes,"alfonso")~ "IE Alfonso Ugarte",
        str_detect(observation_notes,"au pau")~ "IE Alfonso Ugarte",
        str_detect(observation_notes,"au psau")~ "IE Alfonso Ugarte",
        str_detect(observation_notes,"undac")~ "UN. Daniel Alcides Carrion",
        str_detect(observation_notes,"daniel")~ "UN. Daniel Alcides Carrion",
        str_detect(observation_notes,"ramon")~ "IE Ramon Castilla",
        str_detect(observation_notes,"castilla")~ "IE Ramon Castilla",
        str_detect(observation_notes,"castillo")~ "IE Ramon Castilla",
        str_detect(observation_notes,"san francisco")~ "San Francisco",
        str_detect(observation_notes,"sf")~ "San Francisco",
        str_detect(observation_notes,"agroindustrial")~ "IE Agroindustrial",
        str_detect(observation_notes,"agroindustrial")~ "IE Agroindustrial",
        str_detect(observation_notes,"c\\u00e9sar p\\u00e9rez arauco")~ "Cesar P\\u00e9rez Arauco",
        TRUE ~ "Otros varios"
      )
    ) %>% slice(1:803)
}

