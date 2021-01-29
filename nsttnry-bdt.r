# k-armed bandit problem: testbed (nonstationary problems)
# Ko Byeongmin
# Jan 25 2021

## functions

### generating testbed of nonstationary k-bandit problems

bdtProbGen_nstn <- function(steps, k) {
  trueval = matrix(nrow = steps, ncol = k)
  trueval[1, ] <- rep(0, k) # initial true value of each actions
  for (i in 2:steps) {
    trueval[i, ] <- trueval[i-1, ] + rnorm(n=k, mean=0, sd=0.01)
  }
  reward <- matrix(nrow=steps, ncol=k) # matrix storing all rewards
  for (i in 1:k) {
    reward[,i] = rnorm(n = steps, mean = trueval[,i], sd = 1)
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

### bandit algorithm: simple average selection

algo.bandit = function(steps, k, epsilon, silent = F) {
  Prob = bdtProbGen_nstn(steps = steps, k = k)
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
    FollowingReward[i] = bandit(reward = Prob$rewards, time = i, action = A)
    R <- FollowingReward[i] 
    N[A] <- N[A] + 1
    Q[A] <- Q[A] + (1/N[A]) * (R - Q[A])
    Result = Result + R
    if (Prob$truevals[i, ChosenAction[i]] == Prob$truevals[i, which.max(Prob$truevals[i, ])]) {
      OptimalCount = OptimalCount + 1
    } else {
      OptimalCount = OptimalCount + 0
    }
    OptimalActionRate[i] = OptimalCount / i 
  }
  if (silent == F) {
    print("Your score is")
    print(Result)
    print("The rate of optimal choice was")
    print(OptimalCount / steps)
  } else {}
  return(
    list("cumulativeReward"= Result,
         "chosenAction" = ChosenAction,
         "followingReward" = FollowingReward,
         "optimalActionRateRec" = OptimalActionRate,
         "optimalActionRate" = OptimalActionRate[steps],
         "trueval" = Prob$truevals
    )
  )
}

### bandit algorithm: exponential recency-weighted average selection

algo.bandit.cst = function(steps, k, epsilon, alpha, silent = F) {
  Prob = bdtProbGen_nstn(steps = steps, k = k)
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
    FollowingReward[i] = bandit(reward = Prob$rewards, time = i, action = A)
    R <- FollowingReward[i] 
    N[A] <- N[A] + 1
    Q[A] <- Q[A] + alpha * (R - Q[A])
    Result = Result + R
    if (Prob$truevals[i, ChosenAction[i]] == Prob$truevals[i, which.max(Prob$truevals[i, ])]) {
      OptimalCount = OptimalCount + 1
    } else {
      OptimalCount = OptimalCount + 0
    }
    OptimalActionRate[i] = OptimalCount / i 
  }
  if (silent == F) {
    print("Your score is")
    print(Result)
    print("The rate of optimal choice was")
    print(OptimalCount / steps)
  } else {}
  return(
    list("cumulativeReward"= Result,
         "chosenAction" = ChosenAction,
         "followingReward" = FollowingReward,
         "optimalActionRateRec" = OptimalActionRate,
         "optimalActionRate" = OptimalActionRate[steps],
         "trueval" = Prob$truevals
    )
  )
}

## showcase

myalgo <- algo.bandit(steps = 1000, k = 10, epsilon = 0.01, silent = F)
myanotheralgo <- algo.bandit.cst(steps = 1000, k = 10, epsilon = 0.01, alpha = 0.1, silent = F)

## 2000 runs

### parameters

num.k <- 10 # number of actions (bandits)
num.epsilon <- 0.1 # chance of taking a random action
num.steps <- 10000 # choose a large number
num.alpha <- 0.1
dormammu <- 2000 # the number of runs

### initializing record of performance

RewardComparison <- data.frame(matrix(data = 0, nrow = num.steps, ncol = 2))
colnames(RewardComparison) <- c("smplavg", "expwgtavg")

OptimalActionRateComparison <- data.frame(matrix(data = 0, nrow = num.steps, ncol = 2))
colnames(OptimalActionRateComparison) <- c("smplavg", "expwgtavg")

### beginning run (takes VERY LONG)

for (i in 1:dormammu) {
  algo1 <- algo.bandit(steps = num.steps, k = num.k, epsilon = num.epsilon, silent = T)
  algo2 <- algo.bandit.cst(steps = num.steps, k = num.k, epsilon = num.epsilon,
                           alpha = num.alpha, silent = T)
  
  RewardComparison[, 1] = RewardComparison[, 1] + algo1$followingReward
  RewardComparison[, 2] = RewardComparison[, 2] + algo2$followingReward
  
  OptimalActionRateComparison[, 1] = OptimalActionRateComparison[, 1] + algo1$optimalActionRateRec
  OptimalActionRateComparison[, 2] = OptimalActionRateComparison[, 2] + algo2$optimalActionRateRec
  
}

### computing average performance

RewardComparison = RewardComparison / dormammu
OptimalActionRateComparison = OptimalActionRateComparison / dormammu

### plotting

plot(1:num.steps, RewardComparison$smplavg, type = "l",
     xlab = "Steps", ylab = "Average Reward", las = 1,
     main = "Average Reward on each step")
lines(RewardComparison$expwgtavg, col = "blue")
legend("bottomright", legend = c("Sample average", "Exp. weighted average"),
       col = c("black", "blue"), lty=1, cex = 0.8)

plot(1:num.steps, OptimalActionRateComparison$smplavg, type = "l",
     xlab = "Steps", ylab = "Proportion of optimal action", las = 1,
     main = "Proportion of optimal action on each step")
lines(OptimalActionRateComparison$expwgtavg, col = "blue")
legend("topright", legend = c("Sample average", "Exp. weighted average"),
       col = c("black", "blue"), lty=1, cex = 0.8)
