boots_ci <- function(x, alpha, B){
  # Define a function to calculate the mean
  mean_func <- function(data, index) {return(mean(data[index]))}
  
  boot_results = boot(data = x, statistic = mean_func, R = B)
  #res = boot.ci(boot_results, conf = 1-alpha, type = 'perc')$percent[4:5]
  res = boot.ci(boot_results, conf = 1-alpha, type = 'basic')$basic[4:5]
  return(res=res)
}