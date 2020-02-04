library(tidyverse)

bandits <- 3

# population size
pop_size <- 1000000

# number of features
feats <- 2

overall_truth <- c(0.05, 0.10, 0.15)

feature_proportions <- list(X1 = list(type = "discrete",
                                      values = c("M", "F"),
                                      overall_prop = c(.50,.50),
                                      band1 = c(0.80, 0.20),
                                      band2 = c(0.30, 0.70),
                                      band3 = c(0.50,0.50)),
                            X2 = list(type = "discrete",
                                      values = c(1,2,3),
                                      overall_prop = c(.30,.50,.20),
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


# divisibility check

if(pop_size %% bandits == 0) {
        pop_size <-  pop_size
    } else {
        pop_size <- max(c(1:pop_size)[c(1:pop_size) %% bandits== 0])
}

reward_vector <- NULL

for(i in 1:bandits){
  
  yes <- rep(1, times = (pop_size/bandits)*overall_truth[i])
  no <- rep(0, times = (pop_size/bandits)-length(yes))
  new <- c(yes,no)
  reward_vector <- c(reward_vector, new)
  
}


df <- data.frame(User = 1:pop_size,
                 k = rep(c(1:bandits), each = pop_size %/% bandits),
                 reward = reward_vector)

tests <- df %>% filter(k == 1) %>% 
  mutate(X1 = ifelse(reward == 1, 
                     sample(c("M", "F"), size = n(), replace = T, prob = c(.80,.20)),
                     sample(c("M", "F"), size = n(), replace = T, prob = c(.46,.49))
                     ))

# X1 test
k = 1
x = 1

generateX <- function(x, k, reward){
  
    values <- unlist(feature_proportions$X1[2])
    overall_prob <- overall_truth[k]
    reward_prop <- unlist(feature_proportions$X1[3+k])
    overall_prop <- unlist(feature_proportions$X1[3])
      
    remaining_prop <- NULL
  
  for(i in 1:length(values)){
    new <- overall_prop[i] - overall_prob * reward_prop[i]
    remaining_prop[i] <- new
  }
  
  if(reward == 1){
    x <- sample(x = values, size = 1, replace = T, prob = reward_prop)
  } else {
    x <- sample(x = values, size = 1, replace = T, prob = remaining_prop)
  }
  
  return(x)
}