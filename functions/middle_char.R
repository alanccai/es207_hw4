middle_char <- function(x) {
  # when string length is even, subset the two middle characters
  if (str_length(x) %% 2 == 0) {
    mid_char_ind <- str_length(x)/2
    mid_char <- str_sub(x, mid_char_ind, mid_char_ind+1)
  }
  # when string length is odd, subset only the middle character
  else {
    mid_char_ind <- (str_length(x)/2) + 0.5
    mid_char <- str_sub(x, mid_char_ind, mid_char_ind)
  }
  return(mid_char)
}