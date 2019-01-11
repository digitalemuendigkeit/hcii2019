# Function to convert factor variabls from surveymonkey spss file to usable factors
srvmky_convert_factor <- function(x, ordered = T, rev = F) {
  result <- factor(x, labels = get_labels(x), 
                   levels = 1:length(get_labels(x)), ordered = ordered )
  if(rev) {
    result <- fct_rev(result)
  }
  result
}