# Creates a historgram and plots (roundest to the nearest whole number) median
# removes missing values

median_hist <- function(x, x_label, med_label, med_label_x_cord, med_label_y_cord){
  ggplot(
    as.data.frame(x[!is.na(x)]),
    aes(
      x = x[!is.na(x)])) +
    geom_histogram(binwidth = 1) +
    geom_vline(xintercept = 
                 median(
                   x[!is.na(x)]),
               color = "black") + 
    annotate(geom = "text", 
             label = paste(
               med_label,
               round(median(x[!is.na(x)])) 
               ),
             x =  med_label_x_cord,  
             y = med_label_y_cord
             ) +
    xlab(x_label) +
    ylab("Count") + theme_bw()
}

