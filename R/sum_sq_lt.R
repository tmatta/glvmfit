#' Sum of squares from lower triangle 

sum_sq_lt <- function(sq_mat) {
  
  lt_vec <- sq_mat[lower.tri(sq_mat)]

  sum_sq_lt <- sum(lt_vec^2) 
  
  return(sum_sq_lt)

}