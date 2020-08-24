# Retrieve sample and model-implied moments from lavaan
get_moments <- function(lavaan_obj, exo){
  
  if (exo == TRUE) {
    x_loc <- lavaan_obj@SampleStats@x.idx[[1]]
    
    # These will be used for unit checks, someday.
    x_mean <- lavaan_obj@SampleStats@mean.x[[1]]
    x_cov <- lavaan_obj@SampleStats@cov.x[[1]]
  
    sample_mean  <- lavaan_obj@SampleStats@mean[[1]][-x_loc]
    sample_cov   <- lavaan_obj@SampleStats@cov[[1]][-x_loc, -x_loc]
    implied_mean <- lavaan_obj@Fit@Mu.hat[[1]][-x_loc]
    implied_cov  <- lavaan_obj@Fit@Sigma.hat[[1]][-x_loc, -x_loc]
  
  }  else {

    sample_mean  <- lavaan_obj@SampleStats@mean[[1]]
    sample_cov   <- lavaan_obj@SampleStats@cov[[1]]
    implied_mean <- lavaan_obj@Fit@Mu.hat[[1]]
    implied_cov  <- lavaan_obj@Fit@Sigma.hat[[1]]
  

  }


  
  moments_out <- list(S = sample_cov, 
                      Sigma = implied_cov, 
                      ybar = sample_mean, 
                      mu = implied_mean)
  
  return(moments_out)

}
