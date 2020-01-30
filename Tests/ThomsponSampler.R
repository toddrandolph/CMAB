### fit_predict function
# just calls fitBLR and predictProbs

# dummy data
x = c(1)
fit <- matrix(c(-1.3, -2.2, -1.2,
                0.24, 0.26, 0.12),
              nrow = 2, ncol = 3, byrow = T)
prob <- .12
# dummy results_df
results_df <- sample_n(ResultsPop, size = 1000)




fitPredict <- function(data, x){
  
  equation <- paste(colnames(data[2]), paste(colnames(data[3:ncol(data)]), collapse = " + "), sep = " ~ ")
  
  fit <- fitBLR(data = data, equation = equation)
  
  prob <- predictProb(posterior = fit, x = x)[1]
  
  # add way to put ALL coeffs and SDs in the df
  temp_df <- data.frame(prob = prob, params = as.list(fit))
  colnames(temp_df) <- c("prob", paste0(c("b", "sd"), rep(1:ncol(fit), each = 2), separate = ""))
  
  return(temp_df)
  
}

chooseBandit <- function(results_df, x, buffer_size = 400){
  
  df <- tail(results_df, n = buffer_size)
  
  size_check <- df %>% count(Arm, Reward)
  
  if(nrow(df) == k*2){
    
    # choose through TS 
    ts_model_df <- df %>% 
      group_by(Arm) %>% 
      mutate(results = fitPredict(.,x)) %>% 
      ungroup
    
    bandit <- ts_model_df %>% filter(which.max(prob))
    
  } else {
    
    # randomly choose
    bandit <- sample(1:k,1)
    
  }
  
  return(bandit)
}