### Dummy Population ###
library(tidyverse)

# Set up the population & run logistic regression to get realistic estimates of the parameters

# number of arms/creatives
bandits <- 3

# population size
pop_size <- 1000000

# number of features
feats <- 3


estBetaParams <- function(mu, var) {
  
  if(mu > 1) stop("mu must be between 0 and 1")
  if(var > mu*(1-mu)) stop("var must be bewtween 0 and 1*(1-mu)")
  
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  
  unname(rbind(alpha, beta))
  
}

sampleCont <- function(limits, dist_of_values, quant_number = 10){
  
  range_value <- max(limits)/quant_number
  
  quants <- quantile(limits, probs = seq(0,1,.1))[-1]
  
  # samples a quantile based on value dists
  samp <- sample(quants, 1, replace = T, prob = dist_of_values)
  
  # randomly sample within the quantile
  runif(1, as.numeric(samp-range_value), as.numeric(samp)) 
  
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

feature_probs <- list(X1 = list(type = "discrete",
                                values = c("M", "F"),
                                dist_of_values = c(.5,.5),
                                band1 = c(0.10, 0.05),
                                band2 = c(0.05, 0.15),
                                band3 = c(0.00,0.00)),
                      X2 = list(type = "discrete",
                                values = c(1,2,3),
                                dist_of_values = c(.3,.5,.2),
                                band1 = c(0.10,0.10,0.01),
                                band2 = c(0.00,0.00,0.10),
                                band3 = c(0.01,0.05,0.00)),
                      X3 = list(type = "continuous",
                                limits = c(18,100),
                                dist_of_values_quants = c(.05,.05,.1,.1,.2,.2,.1,.1,.05,.05),
                                # dist_params = c(0.3,0), # mean and variance
                                band1 = estBetaParams(.01, .005),
                                band2 = estBetaParams(.10, .010),
                                band3 = estBetaParams(0.00, 0.00))
) 



# initiate the population 
Pop <- data.frame(User = 1:pop_size, k = sample(1:bandits, size = pop_size, replace = T))

feature_df <- bind_cols(lapply(feature_probs, function(x) generateValues(x)))

Pop <- bind_cols(Pop, feature_df)

# start here
test <- Pop %>% 
  mutate(reward = sample(c(1,0), size = n(), replace = T, prob = c(.02, .98)))

samp <- test %>% 
  filter(k == 3)

m1 <- glm(reward ~ X1 + X2 + X3, data = samp, family = binomial)
summary(m1)

# function to predict prob based on Xs

test_df <- slice(Pop, 1:5)

testF <- test_df %>%
  filter(k == 1)
mutate(reward = sample(c(1,0), size = n(), replace = T, prob = c(.015, 1-.015)))


bandit_dfs <- list()

for(i in bandits){
  
  df <- Pop %>%
    filter(k == i) %>%
    mutate(Reward = ifelse(Feat1 == 0,
                           sample(c(1,0), size = n(), replace = T, prob = c(probs[1,i], 1 - probs[1,i])),
                           sample(c(1,0), size = n(), replace = T, prob = c(probs[2,i], 1 - probs[2,i])))
    )
  
  bandit_dfs[[i]] <- df
  
}

# ResultsPop <- bind_rows(SubPop1, SubPop2)

# rn logistic regression on SubPops
Ad1_model <- glm(Reward ~ Feat1, family = binomial, data = SubPop1)
Ad2_model <- glm(Reward ~ Feat1, family = binomial, data = SubPop2)