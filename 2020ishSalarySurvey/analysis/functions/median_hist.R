# Creates a historgram and plots (roundest to the nearest whole number) median
# removes missing values

median_hist <- function(x, x_label, med_label){
  ggplot(
    as.data.frame(x[!is.na(x)]),
    aes(
      x = x[!is.na(x)])) +
    geom_histogram(binwidth = 1) +
    geom_vline(xintercept = 
                 median(
                   x[!is.na(x)]),
               color="light blue") + 
    annotate(geom = "text", label= paste(med_label, round(median(
      x[!is.na(x)]))) , x = 13, y = 25)+
    xlab(x_label) +
    ylab("Count") + theme_bw()
  
}