vector_to_str <- function(x) {
  require(stringr)
  lgt <- length(x)
  str_result <- c()
  if (lgt == 0) {
    str_result <- "No strings to display"
  }
  else if (lgt == 1) {
    str_result <- x[1]
  }
  else if (lgt == 2) {
    str_result <- str_c(x[1], " and ", x[2])
  }
  else {
    for(i in 1:lgt) {
      if (i != lgt) {
        str_ind <- str_c(x[i], ", ")
      }
      else {
        str_ind <- str_c("and ", x[i])
      }
      str_result <- str_c(str_result, str_ind)
    }
  }
  return(str_result)
}