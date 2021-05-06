# Function: Crosstab of two categorical variables 
#' @param x data frame 
#' @param question each response to a multi-select survey question has its own column and 
#  all the responses have the same prefix. 
#' @param cross_var variable you want to cross the survey question by  
#' @param order_by = either by  prespecifed levels, or alphabetical (ascending)
#' @param lvls = level order in x (if you want it ordered). needs to be character vector 
#' @param last_ord = whatever you order by, the inputs here are put in the bottom of the ordering
cat_cross <- function(x, question, cross_var,
                      order_by = "alpha", 
                      lvls = NULL, 
                      last_ord = NULL){
  
  # create a df that subsets to the right columns 
  df <- data.frame(x) %>%
    select({{question}},
           {{cross_var}}) %>% 
    filter(!is.na({{question}}),
           !is.na({{cross_var}}))  %>% 
    rename(question = {{question}},
      cross_var = {{cross_var}})
  
  # create a df of responses. each response will have the same cross_var of 'All'
  # so you are able to compare responses overall and by the cross variable values 
  all <- df %>% 
    mutate(cross_var = 'All')
  
  
  df2 <- df %>% 
    # add in the df that doesnt break down results by cross variable values  
    bind_rows(all)  %>% 
    # for each question x cross_Var pair, calculate the number of people who picked a paricular response 
    count(question, cross_var) %>% 
    # calculate the number of people that have a particular category value 
    group_by(cross_var)  %>% 
    mutate(total_by_cross = sum(n)) %>% 
    ungroup() %>% 
    # remove perfer not to answer responses 
    filter(!cross_var %in% "Prefer not to answer") %>% 
    # remove groups with less than 5 people in them 
    filter(total_by_cross >= 6)

  
  
  # Order by output by prespecified order, by alphabetic order 
  if(order_by == "levels") {
    # order by prespecified levels
    df2 <- df2 %>%
      mutate(
        question = factor(question,
                          levels = c(lvls[!lvls %in% last_ord], last_ord))
      ) %>%
      arrange(question)
  } else if (order_by == "alpha") {
    df2 <- df2 %>%
      arrange(question) # asc order
  } else {
    stop("argument 'order_by' value must either be 'levels', or 'alpha'")
  }
  
  # calculate percents + clean-up table
    df2 <- df2 %>%
    # within each variable value, calculate the percent of people who selected a particular response 
    mutate(pct = n/total_by_cross, 
           # add n-size, so we can create a label for the table 
           cross_var_bin =  paste0(cross_var, "\n(n = ", total_by_cross, ")")) %>%
   # replace any NAs with 0s 
 #   mutate(pct = ifelse(is.na(pct), 0, formattable::percent(pct, 0))) %>%
    mutate(pct = formattable::percent(pct, 0)) %>%
    select(question, cross_var_bin, pct) %>%
    spread(key = cross_var_bin, value = pct)
    
    # change NAs to 0
    df2 <- df2 %>%
      mutate(across(2:ncol(df2), ~ replace_na(., 0L))) 
    
    # return fully-formatted counts  
    df2 <- df2 %>% 
    # add some nice formatting 
      mutate(across(2:ncol(.), color_bar("lightgray"))) %>% 
      rename(' ' = question) %>%
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
