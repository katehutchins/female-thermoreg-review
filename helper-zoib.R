get_all_coef_samples_zoib <- function(fit){
  
  # fix names
  new_names_prefix <- with(fit,
                           c(
                             rep("b_", ncol(Xb)),
                             rep("d_", ncol(Xd)),
                             rep("b0_", ncol(Xb0)),
                             rep("b1_", ncol(Xb1))
                           ))
  
  new_names <- paste0(new_names_prefix,colnames(fit$coeff[[1]]))
  
  # combine chains
  cmb_samples <- runjags::combine.mcmc(fit$coeff)
  colnames(cmb_samples) <- new_names
  
  return(cmb_samples)
  
}

# This uses the posterior fitted values (fitted to data) to approximate the 
# mean proportion at each value of the 'by' varaible.
# It will be slightly different if other covariates are being used.
get_all_fitted_vals_by_zoib <- function(fit, data, by){
  
  # combine chains
  runjags::cmb_samples <- combine.mcmc(fit$ypred)
  
  stopifnot( length(unique(data[,by])) < nrow(data))

  id_groups <- split(1:nrow(data), data[,by])
  
  mean_over_data_groups <- sapply(id_groups, function(id){
    
    rowMeans(cmb_samples[,id])
    
  })
  
  colnames(mean_over_data_groups) <- 
    paste0(by, "_", colnames(mean_over_data_groups))
  
  mcmc(mean_over_data_groups)  
    
}


get_fitted_values_by_var_zoib <- function(fit, new_data){
  
  y_var <- all.vars(update(fit$model, .~0))
  
  predvals <- pred.zoib(object = fit, xnew = new_data, summary = T)$pred
  
  mcsamples <- as.mcmc.list(lapply(predvals, function(x) mcmc(t(x))))
  
  for(i in 1:length(mcsamples)){
    colnames(mcsamples[[i]]) <- paste("row", 1:ncol(mcsamples[[i]]), sep = "_")
  }
  
  mcsamples
  
}


