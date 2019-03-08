

#' A function showing descriptive statistics
#'
#' @param num_list 
#'
#' @return A list of descriptive info
#' @export
#'
#' @examples
descriptive_stats<-function(num_list){
  # Central location
  mean_value = mean(num_list)
  median_value = median(num_list)
  mode_value = as.numeric(names(which.max(table(num_list))))
  
  # Variability
  Var = var(num_list)
  std_Dev = Var^0.5
  min_max = range(num_list)
  data_range = max(num_list)-min(num_list)
  first_quar = quantile(num_list,0.25)
  third_quar = quantile(num_list,0.75)
  IQR = third_quar - first_quar
  
  tenth_quar = quantile(num_list, 0.1)
  ninetieth_quar = quantile(num_list,0.9)
  
  #Tail behavior
  skewness = sum((num_list - mean_value)^3/std_Dev^3 )/(length(num_list)-2)
  
  n = length(num_list)
  skewness2 = (n^2/((n-1)*(n-2))) * (1/n) * sum((num_list - mean_value)^3) /(sum((num_list - mean_value)^2)/(n-1))^1.5
  
  excess_kurt = sum((num_list - mean_value)^4/std_Dev^4)/(length(num_list)-3) -3
  # A different equation 
  left = n * (n+1) * sum((num_list - mean_value)^4)
  left = left/((n-1)*(n-2)*(n-3)*((sum((num_list-mean_value)^2)/(n-1))^2))
  excess_kurt2 = left - 3*(n-1)^2/((n-2)*(n-3))
  
  return (list(Mean=mean_value, Median = median_value, Mode = mode_value,
               Variance = Var, Standard_Deviation = std_Dev, Min_Max = min_max, Range = data_range,
               first_quartile = first_quar, third_quartitle = third_quar, IQR = IQR,
               tenth_quartile = tenth_quar,ninetieth_quartile = ninetieth_quar,
               Skewness = skewness, Excess_kurtosis = excess_kurt,
               Skewness2 = skewness2, Excess_kurtosis2 = excess_kurt2
  ))
  
}
