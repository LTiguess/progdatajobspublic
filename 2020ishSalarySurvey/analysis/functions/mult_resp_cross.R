# for multiple response questions, allow you to look at responses by particular categorical variables 
#' @param x data frame 
#' @param question_prefix each response to a multi-select survey question has its own column and 
#  all the responses have the same prefix. 
#' @param cross_var variable you want to cross the survey question by  
#' @param lvls = level order in x (if you want it ordered). needs to be character vector 


mult_resp_cross <- function(x, question_prefix, cross_var, lvls = NULL){
  
  # create a df that subsets to the right columns 
  df <- x %>% 
    select(contains(question_prefix), {{cross_var}}) %>% 
    rename(cross_var = {{cross_var}})
  
  # create a df of responses. each response will have the same cross_var of 'All'
  # so you are able to compare responses overall and by the cross variable values 
  all <- df %>% 
    mutate(cross_var = 'All')
    
  
   df2 <- df %>% 
     # add in the df that doesnt break down results by cross variable values  
     bind_rows(all)  %>% 
      mutate_at(vars(contains(question_prefix)), list(~as.logical(.))) %>%
      gather(key = var, value = response, -cross_var) %>%
     # for each question x cross variable, count number of "TRUE" results and calculate percent
     group_by(cross_var, var) %>% 
     mutate(total_yes = sum(response == TRUE, na.rm = TRUE)) %>% 
     ungroup() %>% 
     count(var, cross_var, total_yes) %>% 
     mutate(pct = formattable::percent(total_yes/n, 0), 
    # add n-size, so we can create a label for the table 
    cross_var_bin = paste0(cross_var, "\n (n = ", n, ")")) %>%
     # remove groups with less than 5 people in them 
     filter(n >= 6) %>% 
     # add in cleanned response name
     left_join(
       col_names,
       by = c("var" = "col_name_final")
     ) %>% 
     select(response_name, cross_var_bin, pct)
   
   if(! is.null(lvls)){
     df2 <- df2 %>%
       mutate(
         response_name = factor(
           response_name,
           levels = lvls)
       ) %>% 
       arrange(response_name)
   }
   
   df2 <- df2 %>% 
     spread(key = cross_var_bin, value = pct)

   # return fully-formatted counts  
   df2 <- df2 %>% 
     # add some nice formatting 
     mutate(across(2:ncol(.), color_bar("lightgray"))) %>% 
     rename(' ' = response_name) %>%
     kable(
       "html", 
       escape = F, 
       booktabs = T, 
       # first col is left aligned, all others are right
       align = paste0(
         "l", 
         paste0(rep("r", ncol(.) - 1), collapse = ""))
     ) %>%
     kable_styling("hover") 
   
  return(df2)

}