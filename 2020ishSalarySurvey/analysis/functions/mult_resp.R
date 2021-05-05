# for multiple response questions 
# helpful resource: https://stackoverflow.com/questions/9265003/analysis-of-multiple-response
# to do, make it easy to clean-up names + make it easy to order the variables

mult_resp <- function(df, question.prefix, raw_df = F,
                      order_prop = F, last_ord = NULL) {
  # x = data frame 
  # question.prefix = charater string
  # raw_df = if T, return markdown, if F return HTML
  # order_prop = if T, order by proprotion, if F, order alphabetical
  # last_ord = whatever you order by, the inputs here are put in the bottom of the ordering
  
  
  
  # get the number of cases (total people who took the survey)
  n_cases <- nrow(df) 
  
  
  # create a df of people who answered this question 
  df2 <- 
    # select the columns we are interested in analysing 
    select(df, contains(question.prefix)) %>%
    mutate_all(list(~as.logical(.)))  %>%
    mutate(
      # for each respondent, count the number of 'True' values 
      n_qs_answered = rowSums(., na.rm = TRUE))  %>% 
    # filter anyone who didn't anwser the question 
    filter(n_qs_answered > 0)
  
  # get count of people who anwsered the question 
  n_res <- nrow(df2)
  
  # for each response, get total number of responses 
  freq <- df2 %>% select(-n_qs_answered) %>%
    gather("response", "answer_q") %>%
    group_by(response) %>%
    summarize(
      unweighted_prop  = sum(answer_q, na.rm = T)/ n_res,
      unweighted_n = sum(answer_q, na.rm = T)) %>% 
    # add in cleanned response name
    left_join(
      col_names,
      by = c("response" = "col_name_final")
    )
  
  
  
  # Order if applicable
  if(order_prop == TRUE){
    freq <- freq %>%
      arrange(desc(unweighted_prop))
    var_char <- as.character(freq$response)
    freq$response <- factor(
      freq$response,
      levels = c(var_char[! var_char %in% last_ord], last_ord))
    freq <- freq %>%
      arrange(response)
  }
  
  freq <- freq %>%
    select(response_name, unweighted_prop, unweighted_n) %>%
    mutate(unweighted_prop = formattable::percent(unweighted_prop, 0), 
           unweighted_n = formattable::digits(unweighted_n, 0)
    )
  
  
  if(raw_df == TRUE){
    freq
  } else {
    freq %>% 
      mutate(unweighted_prop  = color_bar("lightgray")(unweighted_prop))  %>% 
      rename(' ' = response_name,
             'Proportion' = unweighted_prop, 
             'N' = unweighted_n) %>%
      # formattable::formattable(list(
      #   Proportion = formattable::proportion_bar()),
      #   align = c('l', 'r', 'r')
      kable("html", escape = F, booktabs = T, align = "lrr") %>%
      kable_styling("hover")  
      
  }
}

