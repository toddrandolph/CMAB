# number of arms/creatives
bandits <- 3

# population size
pop_size <- 1000000

# number of features
feats <- 2

overall_truth <- c(0.05, 0.10, 0.15)

feature_proportions <- list(X1 = list(type = "discrete",
                                      values = c("M", "F"),
                                      band1 = c(0.80, 0.20),
                                      band2 = c(0.30, 0.70),
                                      band3 = c(0.50,0.50)),
                            X2 = list(type = "discrete",
                                      values = c(1,2,3),
                                      band1 = c(0.60,0.10,0.30),
                                      band2 = c(0.30,0.30,0.40),
                                      band3 = c(0.25,0.05,0.70)))
#  #     X3 = list(type = "continuous",
#  limits = c(18,100),
#  dist_of_values_quants = c(.05,.05,.1,.1,.2,.2,.1,.1,.05,.05),
#  # dist_params = c(0.3,0), # mean and variance
#  band1 = estBetaParams(.01, .005),
#  band2 = estBetaParams(.10, .010),
#  band3 = estBetaParams(0.00, 0.00))


allocateClicks <- function(k){
  
  sample(c(1,0), 1, replace = T, prob = c(overall_truth[k], 1-overall_truth[k]))
  
}

allocateClicks <- Vectorize(allocateClicks)

Pop <- data.frame(User = 1:pop_size, k = sample(1:bandits, size = pop_size, replace = T)) %>% 
  group_by(k) %>% 
  mutate(reward = allocateClicks(k)) 