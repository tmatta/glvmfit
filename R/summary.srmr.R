
summary.srmr <- function(obj) {

  df_labs <- names(obj$srmr[!is.na(obj$srmr)])  
  
  srmr_vec <- obj$srmr[!is.na(obj$srmr)]  
  p_vec <- obj$size[!is.na(obj$srmr)]

  srmr_df <- data.frame("srmr" = srmr_vec, "p" = p_vec)

  rownames(srmr_df) <- df_labs
  
  return(srmr_df)

}
