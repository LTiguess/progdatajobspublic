# Function: Crosstab of two categorical variables 


tmp <- df %>% 
  select(num_staffers, gender_bin)  %>% 
  filter(!is.na(num_staffers), !is.na(gender_bin))  %>% 
  # group by variable
  group_by(gender_bin, num_staffers) %>%
  # get raw counts
  summarise(unweighted_n = n()) %>%
  # create probabilities
  mutate(unweighted_prop = unweighted_n/sum(unweighted_n))


