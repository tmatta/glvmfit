
# summary.resid_fit <- function(object) {
# 
#   df_labs <- names(object$srmr[!is.na(object$srmr)])  
#   
#   srmr_vec <- object$srmr[!is.na(object$srmr)]  
#   p_vec <- object$size[!is.na(object$srmr)]
# 
#   srmr_df <- data.frame("srmr" = srmr_vec, "p" = p_vec)
# 
#   rownames(srmr_df) <- df_labs
#   
#   return(srmr_df)
# 
# }
