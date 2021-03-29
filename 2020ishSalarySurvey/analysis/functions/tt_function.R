# Functions - Clean Table with Counts + Proportions 
tt <- function(x, weights = NULL,
               lvls = NULL, recode_na = 'Missing',
               raw_df = F,
               order_prop = F, last_ord = 'Missing'){
  
  # x = vector of values
  # weights = weights
  # lvls = level order in x (if you want it ordered)
  # necode_na = how to recode NA values, defaults to 'Refused'
  # raw_df = if T, return markdown, if F return HTML
  # order_prop = if T, order by proportion, if F and lvls is null, order alphabetical
  # last_ord = whatever you order by, the inputs here are put in the bottom of the ordering
  
  # conditions to run
  stopifnot(#is.vector(x),
    length(x) > 0
  )
  
  # if weights is null, set to 1
  if(is.null(weights)) weights <- rep(1, length(x))
  
  if(is.factor(x) & sum(is.na(x)) > 0){
    # if x is already a factor, don't do any sorting on the levels 
    x <- factor(x, levels = c(levels(x), recode_na))
  }
  
  if(is.character(x) & sum(is.na(x)) > 0){
    # default to sorting by alphabetical in x is a character vector
    # if order_prop = T, this gets overwritten later on  
    x <- factor(x, levels = sort(c(unique(x), recode_na)))
  }
  
  # add ordered factor
  if(!is.null(lvls)){
    if(sum(is.na(x)) > 0) lvls <- c(lvls, recode_na)
    x <- factor(x, levels = lvls, ordered = T)
  } 
  
  # add NA values
  if(!is.null(recode_na) & sum(is.na(x)) > 0) x[is.na(x)] <- recode_na
  
  # create crosstab
  df <- data.frame(variable = x) %>%
    # group by variable
    group_by(variable) %>%
    # get raw counts
    summarise(unweighted_n = n()) %>%
    ungroup() %>%
    tidyr::complete(variable, fill = list(unweighted_n = 0)) %>%
    # create probabilities
    mutate(unweighted_prop = unweighted_n/sum(unweighted_n))
  
  # Order if applicable
  if(order_prop == TRUE){
    df <- df %>%
      arrange(desc(unweighted_prop))
    var_char <- as.character(df$variable)
    df$variable <- factor(
      df$variable,
      levels = c(var_char[! var_char %in% last_ord], last_ord))
    df <- df %>%
      arrange(variable)
  }

  df <- df %>%
    select(variable, unweighted_prop, unweighted_n) %>%
    mutate(unweighted_prop = formattable::percent(unweighted_prop, 0),
           unweighted_n = formattable::digits(unweighted_n, 0))
  
  
  if(raw_df == TRUE){
    df
  } else {
 #   Returns the HTML stuff
    df %>%
      rename(' ' = variable,
             'Proportion' = unweighted_prop,
             'N' = unweighted_n) %>%
      formattable::formattable(list(
        Proportion = formattable::proportion_bar()),
        align = c('l', 'r', 'r'))
  }
  

# 
# # Returns the HTML stuff
# .pretty_tt <- function(df){
#   
#   df %>%
#     rename(' ' = variable,
#            'Proportion' = unweighted_prop,
#            'N' = unweighted_n) %>%
#     formattable::formattable(list(
#       Proportion = formattable::proportion_bar()),
#       align = c('l', 'r', 'r'))
#   
#   
# }
}
