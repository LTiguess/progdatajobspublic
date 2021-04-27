# Function: Produces crosstable of salary with a 
salary_cross <- function(x, 
                         variable,
                         salary,
                         salary_bin, 
                         salary_bin_numeric,
                         order_by, # N, levels, alpha
                         lvls = NULL,
                         last_ord = NULL
                         ){
# x = data frame 
# variable = categorical variable, you can calculating salary info for each catogory
# salary = numeric value of salary 
# salary_bin = binned salary value 
  
# salary_bin_numeric = value of bin when you do as.numeric(df$salary_bin)
  
# order_by = either by N (decending), prespecifed levels, or alphabetical (ascending)
# lvls = level order in x (if you want it ordered). needs to be character vector 
# last_ord = whatever you order by, the inputs here are put in the bottom of the ordering

  
# clean df   
df <- data.frame(x) %>%
  select({{variable}},
         {{salary}},
         {{salary_bin}}, 
         {{salary_bin_numeric}}
         ) %>% 
  rename(variable = {{variable}}, 
         salary = {{salary}}, 
         salary_bin = {{salary_bin}},
         salary_bin_numeric = {{salary_bin_numeric}}
         )  %>% 
  # remove anyone that is missing the categorical variable and/or salary data 
  filter(!is.na(variable),
         !is.na(salary),
         !is.na(salary_bin), 
         !is.na( salary_bin_numeric))

# crosswalk to go from numeric salary bin to salary bin 

salary_bin_crosswalk <- df %>% distinct(salary_bin, salary_bin_numeric)
  

# create crosstab 
cross_table <- df  %>%
  group_by(variable) %>%
  summarise(
    mean = mean(salary),
    median_numeric = round(median(salary_bin_numeric), 0),
    median = median(salary), 
    n = n()
  ) %>% 
  # remmove if n is less than or equal to 5 
  filter(n >= 6) %>% 
  ## add in the bin range associated with the median value
  left_join(
    salary_bin_crosswalk,
    by = c("median_numeric" = "salary_bin_numeric")
  )


# Order by output by mean, prespecified order, by alphabetic order 
if(order_by == "n"){
  # order by mean
  cross_table <- cross_table %>% 
    arrange(desc(n))
} else if (order_by == "levels") {
   # order by prespecified levels
  cross_table <- cross_table %>%
    mutate(
      variable = factor(variable,
             levels = c(lvls[!lvls %in% last_ord], last_ord))
      ) %>%
    arrange(variable)
} else if (order_by == "alpha") {
    cross_table <- cross_table %>%
      arrange(variable) # asc order
  } else {
    stop("argument 'order_by' value must either be 'n', 'levels', or 'alpha'")
  }


# clean-up output 
clean_table <- cross_table %>%
  # select only the necessary columns
  select(variable, n, mean,  median
         #, median_bin = salary_bin
         ) %>%  
  mutate(mean = formattable::currency(mean, digits = 0), 
         median = formattable::currency(median, digits = 0))  %>%  
  # mutate(mean = ifelse(n >= 5, formattable::currency(mean), NA),
  #        median = ifelse(n >= 5, median, NA)) %>%
  
  # clean-up output
  rename('Category' = variable,
         'N' = n,
         'Mean' = mean,
         'Median' = median
        # 'Median Bin' = median_bin
        ) %>%
  formattable::formattable(
    list(N = formattable::proportion_bar()),
    align = c('l', 'r', 'r', 'r'
            #  , 'r'
              ))  
# %>% 
#   kbl() %>%
#   kable_minimal()

return(clean_table)

}




# test <- formatter("span",
#                   style = x ~ ifelse(cyl < 5, "formattable::currency(cyl)", "-"))
# 
# yesno <- function(x) ifelse(x, "yes", "no")
# formattable(mtcars, list(cyl = test, qsec = top10red, am = yesno))
# 
