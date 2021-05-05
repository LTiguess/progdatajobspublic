# Function: Create Scatterplot of Salary Crossed by a Variable
#' @param x data frame 
#' @param variable
#' 
plot_salary <- function(dat, variable){
  
  df <- dat %>%
    select(
      {{variable}}, 
      compensation, 
      years_worked_political_data, 
      years_worked_political, 
      years_worked_data, 
      years_worked_total,
      work_hist_eligible
    ) %>% filter(
      work_hist_eligible == TRUE, 
      !is.na(.data[[variable]]),
      grepl('Prefer not to answer', .data[[variable]]) == FALSE
    )
  
  ## creates chart
  plot <- df %>% 
    ggplot(aes(x = years_worked_total, y = compensation, color = .data[[variable]])) +
    geom_jitter(stat = "identity", width = 2, height = 2) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_light()+
    theme(legend.title=element_blank())+
    xlab("Years of Experience")+
    ylab("Salary") + 
    scale_y_continuous(labels=scales::dollar_format())
  
  return(plot)
} 
