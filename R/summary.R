#' Summary of categorical variables
#' 
#' @param dfr data.frame data from WikiPapa platform
#' @param catvar character vector categorical variable
#' @param total logical If TRUE include total number of observations
#' @importFrom summarytools freq
#' @importFrom tibble rownames_to_column
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate case_when arrange
#' @export
#' 

get_summary_varcat <- function(dfr, catvar, total = FALSE){
  
  summary_catvar <- summarytools::freq(dfr[,catvar]) 
  summary_catvar <- as.data.frame.matrix(summary_catvar) %>% 
                          rownames_to_column(var = "category") %>% 
                          janitor::clean_names() %>% 
                          mutate(
                            category = case_when(
                              is.na(category)~"Sin identificar",
                              category=="<NA>"~"Sin identificar",
                              TRUE~as.character(category)
                            ) 
                          )
  
  summary_catvar <- summary_catvar %>% 
    arrange(desc(freq)) %>% 
    mutate(across(.cols = 3:6 , round, 1 ))
  
  if(isFALSE(total)){
    summary_catvar <- summary_catvar[-1,]
  }
  return(summary_catvar)
}


#' Get summary of variables from Wikipapa data
#' 
#' @param dfr data data.frame or tibble
#' @param group character vector used to aggregate data by groups
#' @param variable chr variable
#' @export
#' 

get_summary_groupvar <- function(dfr, group, variable ){
  
  out <- dfr %>% group_by( across({{group}}), across({{variable}}) ) %>% 
                 count(name = "ncont") %>%  
                 group_by(across({{group}})) %>% 
                 mutate(percent_answers = ncont / sum(ncont)) %>% 
                 ungroup() %>% 
                 mutate(percent_answers_label = percent(percent_answers, accuracy = 1))
  return(out)
}




#https://github.com/anastasia-lucas/rladies_shinydashboards/blob/main/solutions/app_advanced.R
# https://www.bioinfo-scrounger.com/archives/NSE-mode-dplyr/
#   https://clarewest.github.io/blog/post/making-tables-shiny/
#   https://www.bryanshalloway.com/2020/06/25/using-across-to-build-functions-with-dplyr-with-notes-on-legacy-approaches/
#   https://mastering-shiny.org/action-tidy.html
# https://debruine.github.io/shinyintro/contingency.html