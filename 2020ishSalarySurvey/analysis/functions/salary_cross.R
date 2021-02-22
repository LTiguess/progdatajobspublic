# to do: clean out mean salary field 
salary_cross <- function(x, variable, salary, salary_bin, salary_bin_numeric){
  # x = data frame 
  # variable = what you are looking into in terms of salary 
  # salary = numeric value of salary 
  # salary_bin = binned salary value 
  # salary_bin_numeric = value of bin when you do as.numeric(df$salary_bin)
df2 <- x %>%
  select(variable, salary, salary_bin, salary_bin_numeric) %>%
  # remove anyone that is missing gender or salary data 
  filter(!is.na(variable), !is.na(salary), !is.na(salary_bin)) %>%
  # order df, so that smallest salaries are at the top 
  arrange((salary))

cross <- df2  %>%
  group_by(variable) %>%
  summarise(
    mean = mean(salary),
    median_numeric = round(median(salary_bin_numeric), 0),
    n = n()
  ) %>% 
  ## add in the bin range associated with the median value 
  left_join(
    salary_bin_crosswalk, 
    by = c("median_numeric" = "compensation_bin_numeric")
  ) %>% 
  # select only the necessary columns 
  select(variable, n, mean, median = salary_bin) %>%
  # clean-up output
  rename('Variable' = variable,
         'N' = n, 
         'Mean' = mean, 
         'Median' = median) %>%
  formattable::formattable(
    list(N = formattable::proportion_bar()), 
    # Mean = formattable::currency(Mean),
    align = c('l', 'r', 'r', 'r'))

print(cross)

}

