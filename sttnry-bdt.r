# k-armed bandit problem: testbed
# Ko Byeongmin
# Jan 25 2021

## functions

### generating testbed for each run

bdtProbGen <- function(steps, k, silent = T) {
  trueval <- rnorm(k, mean = 0, sd = 1) # true value of each actions
  reward <- matrix(nrow=steps, ncol=k) # matrix storing all rewards
  for (i in 1:k) {
    reward[,i] = rnorm(n = steps, mean = trueval[i], sd = 1)
  }
  if (silent == T) {
    
  } else {
    print("The true values of each actions are")
    print(trueval)
  }
  return(list("rewards" = reward, "truevals" = trueval))
}

### takes an action and reports the corresponding reward

bandit <- function(reward, time, action) {
  return(reward[time, action])
}

### breaking ties at random
#### author: Brian Ripley
#### source: https://cran.r-project.org/web/packages/nnet/index.html

which.is.max <- function(x)
{
  y <- seq_along(x)[x == max(x)]
  if(length(y) > 1L) sample(y, 1L) else y
}

### Obtaining mode
#### source: https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

### bandit algorithm

algo.bandit = function(steps, k, epsilon, silent = F) {
  Prob = bdtProbGen(steps = steps, k = k)
  Q <- rep(0, k) # initializing
  N <- rep(0, k) # initializing
  Result = 0 # initializing
  ChosenAction = rep(0, steps)
  OptimalCount = 0
  FollowingReward = rep(0, steps)
  OptimalActionRate = rep(0, steps) # mark!
  for (i in 1:steps) {
    coin <- rbinom(n = 1, size = 1, prob = epsilon)
      if (coin == 1) { # takes random action w/ prob epsilon
        A <- sample(1:k, size = 1)
        } else {
        A <- which.is.max(Q)
      }
    ChosenAction[i] = A # records the action chosen
    if (ChosenAction[i] == which.max(Prob$truevals)) {
      OptimalCount = OptimalCount + 1
    } else {
      OptimalCount = OptimalCount + 0
    }
    OptimalActionRate[i] = OptimalCount / i 
    FollowingReward[i] = bandit(reward = Prob$rewards, time = i, action = A)
    R <- FollowingReward[i] 
    N[A] <- N[A] + 1
    Q[A] <- Q[A] + (1/N[A]) * (R - Q[A])
    Result = Result + R
  }
  if (silent == F) {
    print("Your score is")
    print(Result)
    print("The true values of each actions are")
    print(Prob$trueval)
    print("The most frequently chosen action is")
    print(getmode(ChosenAction))
    print("The optimal action was")
    print(which.max(Prob$trueval))
    print("The proportion of optimal selection was")
    print(N[which.max(Prob$trueval)]/steps)
  } else {}
  return(
    list("cumulativeReward"= Result,
         "chosenAction" = ChosenAction,
         "followingReward" = FollowingReward,
         "optimalActionRateRec" = OptimalActionRate,
         "optimalActionRate" = OptimalActionRate[steps]
#         "optimalActionRate" = N[which.max(Prob$trueval)]/steps
         )
    )
}

## example of a run

myalgo <- algo.bandit(steps = 1000, k = 10, epsilon = 0.01, silent = F)
myalgo$optimalActionRate
plot(myalgo$optimalActionRateRec)

## 2000 runs

### parameters

num.k <- 10 # number of actions (bandits)
# num.epsilon <- 0.01 # chance of taking a random action
num.steps <- 2000 
dormammu <- 2000 # the number of runs 

### initializing record of performance

RewardComparison <- data.frame(matrix(data = 0, nrow = num.steps, ncol = 3))
colnames(RewardComparison) <- c("0.1", "0.01", "greedy")

OptimalActionRateComparison <- data.frame(matrix(data = 0, nrow = num.steps, ncol = 3))
colnames(OptimalActionRateComparison)<- c("0.1", "0.01", "greedy")

### beginning run (takes a while)

for (i in 1:dormammu) {
  algo1 <- algo.bandit(steps = num.steps, k = num.k, epsilon = 0.1, silent = T)
  algo2 <- algo.bandit(steps = num.steps, k = num.k, epsilon = 0.01, silent = T)
  algo3 <- algo.bandit(steps = num.steps, k = num.k, epsilon = 0, silent = T)
  
  RewardComparison[, 1] = RewardComparison[, 1] + algo1$followingReward
  RewardComparison[, 2] = RewardComparison[, 2] + algo2$followingReward 
  RewardComparison[, 3] = RewardComparison[, 3] + algo3$followingReward 
  
  OptimalActionRateComparison[, 1] = OptimalActionRateComparison[, 1] + algo1$optimalActionRateRec
  OptimalActionRateComparison[, 2] = OptimalActionRateComparison[, 2] + algo2$optimalActionRateRec
  OptimalActionRateComparison[, 3] = OptimalActionRateComparison[, 3] + algo3$optimalActionRateRec
}

### computing average performance

RewardComparison = RewardComparison / dormammu
OptimalActionRateComparison = OptimalActionRateComparison / dormammu

### plotting

plot(1:num.steps, RewardComparison$`0.1`, type = "l", ylim = c(0, 2),
     xlab = "Steps", ylab = "Average Reward", las = 1,
     main = "Average Reward on each step")
lines(RewardComparison$`0.01`, col = "blue")
lines(RewardComparison$greedy, col = "red")
legend("bottomright", legend = c("e=0.1", "e=0.01", "e=0 (greedy)"),
       col = c("black", "blue", "red"), lty=1, cex = 0.8)


plot(1:num.steps, OptimalActionRateComparison$`0.1`, type = "l",
     xlab = "Steps", ylab = "Proportion of optimal action", las = 1,
     main = "Proportion of optimal action on each step")
lines(OptimalActionRateComparison$`0.01`, col = "blue")
lines(OptimalActionRateComparison$greedy, col = "red")
legend("bottomright", legend = c("e=0.1", "e=0.01", "e=0 (greedy)"),
       col = c("black", "blue", "red"), lty=1, cex = 0.8)

