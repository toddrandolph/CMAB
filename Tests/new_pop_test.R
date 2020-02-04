library(tidyverse)

# number of arms/creatives
bandits <- 3

# population size
pop_size <- 1000000

# number of features
feats <- 2

overall_truth <- c(0.05, 0.10, 0.15)

feature_proportions <- list(X1 = list(type = "discrete",
                                      values = c("M", "F"),
                                      overall_prop = c(.5,.5),
                                      band1 = c(0.80, 0.20),
                                      band2 = c(0.30, 0.70),
                                      band3 = c(0.50,0.50)),
                            X2 = list(type = "discrete",
                                      values = c(1,2,3),
                                      band1 = c(0.60,0.10,0.30),
                                      band2 = c(0.30,0.30,0.40),
                                      band3 = c(0.25,0.05,0.70))
                            )
                          #  #     X3 = list(type = "continuous",
                          #  limits = c(18,100),
                          #  dist_of_values_quants = c(.05,.05,.1,.1,.2,.2,.1,.1,.05,.05),
                          #  # dist_params = c(0.3,0), # mean and variance
                          #  band1 = estBetaParams(.01, .005),
                          #  band2 = estBetaParams(.10, .010),
                          #  band3 = estBetaParams(0.00, 0.00))

features <- data.frame(X1 = rep(NA, pop_size),
                       X2 = rep(NA, pop_size),
                       X3 = rep(NA, pop_size))

generateReward <- Vectorize(function(k){
  
  sample(c(1,0), 1, replace = T, 
         prob = c(overall_truth[k], 1-overall_truth[k]))
  
})

Pop <- data.frame(User = 1:pop_size, k = sample(1:bandits, size = pop_size, replace = T)) %>% 
  group_by(k) %>% 
  mutate(reward = generateReward(k)) %>% 
  ungroup %>% 
  bind_cols(features)

Pop_test <- Pop %>% 
  gather(key, value, -c(1:3)) %>% 
  mutate(feat_number = as.numeric(str_extract(key, "\\d")))
  sample_n(400) %>% 
  filter(!(key %in% "X3")) %>% 
  mutate(value = generateX(key, reward))
  
# write function
k = 1
x = 1
reward = 0
  
generateX <- function(x, reward, k){
  
  x_type <- unname(unlist(feature_proportions[[x]][1]))
  
  if(x_type %in% "discrete"){
    
    x_values <- unname(unlist(feature_proportions[[x]][2]))
    k_props <- unname(unlist(feature_proportions[[x]][k + 3]))
    
    if(reward == 1){
      
      k_props <- unname(unlist(feature_proportions[[x]][k + 3]))
      
      value <- sample(x_values, 1, replace = T, prob = k_props)
      
    } else {
      
      value <- 
      
    }
    
  } 
  
  if(reward == 1){
    
    value <- sample(feature_proportions[])
    
  }
  
  sample(feature_proportions)
  
}

generateValues <- function(df){
  
  if(!(as.character(df$type) %in% c("discrete","continuous"))) stop("type must == 'discrete' or 'continuous'")
  
  if(as.character(df$type) %in% "discrete"){
    
    values <- sample(x = df$values, size = pop_size, prob = df$dist_of_values, replace = TRUE)
    
  } else {
    
    if(sum(df$dist_of_values_quants) != 1) stop("continuous: distribution of quantiles must sum to 1")
    
    values <- replicate(n = pop_size, sampleCont(limits = df$limits, dist_of_values = df$dist_of_values_quants))
  }
  
}

### to consider: 
#     - say the proportion of reward == 1 for bandit 3 is 80% M and 20% F.
#     - then, if we assume that the M/F split is about 50/50 OVERALL, the remaining 
#         amount with reward == 0 must account for this 50/50. 
#         (i.e. there should be slighty more Females than Males in the rest of the population, 
#           leading to an OVERALL 50/50 split) 
#           -- add this overall balance to the feature_proportions list, as it can change 
#              (i.e. maybe we assume the M/F split is 60/40)
