# functions to run book chapter 9 simulation.  

# requires package "ineq" which provides calculation for gini coefficient
# then syntax is just ineq(x, type="Gini")
# load with source() base function, then syntax to run everything is simulate.ch9(x) where x = number of runs

# this may help my BS: http://www.win-vector.com/blog/2012/10/error-handling-in-r/

# outtermost function: pick a number of runs, get the output from each run, makes row of data frame, and save that to a csv + returns it.
# calls function outer.wrapper() which is just a single run of the simulation
# saves (runs (rounded up to next 1000))/1000  csv files to disk, each with 
# results of 1000 runs, one per row. 
# also saves a list of file names, and returns that list for post-processing.

# QUESTION: I'm generating a lot of groupwise stuff using tapply (e.g. groupwise power means, groupwise goods means)
# am I sure that R will preserve the numerical ordering of groups in the ultimate vector?  
# I'm PRETTY sure it will: tapply coerces factors, and as.factor doc says it preserves order treated as character.  
# SURELY numbers when treated as characters preserve their natural order.   at least to first digit.

simulate.ch9 <- function(runs) {
  # library(compiler)
  # enableJIT(3)
  # not sure if that's a good idea...
  library(ineq)  # for later
  by1k <- round(runs/1000) + 1
  output.fnames <- vector(mode = "character", length = by1k)
  for (j in 1:by1k) {
    results <- data.frame(matrix(NA, nrow=1000, ncol=27))
    colnames(results) <- c("run.id", "goods.mean.elite", "goods.mean.mass", "goods.sd.mass", "goods.gini.mass", 
                           "goods.gini.all", "power.mean.elite", "power.mean.mass", "power.sd.mass", "power.gini.mass", 
                           "power.gini.all", "num.subgroups", "subgroups.max.members", "subgroups.min.members", 
                           "subgroup.mean.members", "groupwise.goods.gini", "groupwise.power.gini", 
                           "trust", "commitment", "penalty", "errorvar", "decay", "power.decay", "shockvar", "rounds", 
                           "attempts", "ending.trust")
    for (i in 1:1000) {
      run.res <- c(i,outer.wrapper())
      results[i,] <- run.res
    }
    increment.name <- sprintf("run%s_%s.csv", i, timeRR())
    write.csv(results, increment.name, row.names=FALSE)  # does this handle column names right?
    # save.image()
    output.fnames[j] <- increment.name
  }
  write(output.fnames, file = "simul_filenames.txt")  # does this handle column names right?
  return(output.fnames)
  # then I can just pass the output of this function to another function that reassembles all the 
  # csv files into a data frame for analysis, and also to concatenate into one csv.
}

# just clean up the system time for the file
timeRR <- function() {
  watchglance <- Sys.time()
  poseR <-strptime(watchglance, "%Y-%m-%d %H:%M:%S")
  cleantime <- paste(poseR$mday, poseR$hour, poseR$min, poseR$sec, sep="")
  return(cleantime)
}
  

# need to write something to put the files back together for analysis, but this is low priority


# next function takes all the parameter functions, passes it to a function called inner.wrapper(), and then takes the output and gives it to simulate.ch9
# calls: dist.goods(), dist.power(), dist.groups(), and inner.wrapper()
# called by: simulate.ch9
# returns results of one run of the simulation

outer.wrapper <- function() {
  goods.v <- dist.goods()
  power.v <- dist.power()
  # goods and power are both going to be vectors of length 1,100 to be matched by indexing to citizens, where elites are first 100
  subgroups.num <- sample(2:5, 1)
  subgroups.dist <- dist.groups(subgroups.num)
  trust.v <- runif(1, .1, .9)
  commitment.v <- runif(1, 0, 1)
  penalty.v <- runif(1, .5, 5)
  errorvar.v <- runif(1, .01, .2)
  decay.v <- sample(c(0, .01), 1)
  power.decay.v <- sample(c(0, .01, .05, .1), 1)
  shockvar.v <- runif(1, .05, .6)
  iniparams <- list(goods = goods.v, power = power.v, numgroups = subgroups.num, groupassgs = subgroups.dist, 
                    trust = trust.v, commitment = commitment.v, penalty = penalty.v, errorvar =  errorvar.v, 
                    decay = decay.v, power.decay = power.decay.v, shockvar = shockvar.v)
  one.run <- inner.wrapper(iniparams)
  return(one.run)
}

# get a distribution of goods to hand off to outer.wrapper()
# calls nothing I've defined, called by outer.wrapper()
# returns vector of length 1100, distribution of goods for each citizen.  First 100 are elite.

dist.goods <- function() {
  elite.goods <- sample(1000:4000, 1)
  goods.perelite <- elite.goods/100
  elite.dist <- rep(goods.perelite, 100)
  mass.goods <- 10000-elite.goods
  mass.goods.toalloc <- mass.goods-1000
  mass.dist <- rep(1, 1000)
  # for each good to allocate, picks a random citizen to allocate to.
  mass.rand <- sample(1:1000, mass.goods.toalloc, replace=TRUE)
  for (i in 1:length(mass.rand)) {
    mass.dist[mass.rand[i]] <- mass.dist[mass.rand[i]]+1
  }
  goods.dist <- c(elite.dist, mass.dist)
  return(goods.dist)
}

# get a distribution of power to hand off to outer.wrapper()
# calls nothing I've defined, called by outer.wrapper()
# returns vector of length 1100, distribution of power for each citizen.  First 100 are elite.

dist.power <- function() {
  elite.power <- sample(2000:4900, 1)
  power.perelite <- elite.power/100
  elite.dist <- rep(power.perelite, 100)
  mass.power <- 10000-elite.power
  mass.power.toalloc <- mass.power-1000
  repeat 
  {
    mass.dist <- rep(1, 1000)
    # for each power unit to allocate, picks a random citizen to allocate to.
    mass.rand <- sample(1:1000, mass.power.toalloc, replace=TRUE)
    for (i in 1:length(mass.rand)) {
      mass.dist[mass.rand[i]] <- mass.dist[mass.rand[i]]+1
    }
    if (all(mass.dist < power.perelite)) 
    {
      break
    }
  } 
  
  power.dist <- c(elite.dist, mass.dist)
  return(power.dist)
}

# assigns each member of the mass to a group, and assigns each member of elite to group 0 (no group)
# returns a vector of length 1100 with group assignments
# calls nothing, called by outer.wrapper()

dist.groups <- function(groupnum) {
  
  elitegroups <- rep(0, 100)
  massgroups <- rep(1, 1000)
  if (groupnum > 1) {
    massgroups <- sample(1:groupnum, 1000, replace=TRUE)
  }
  group.dist <- c(elitegroups, massgroups)
  return(group.dist)
}


inner.wrapper <- function(iniparams) {
  goods.mean.elite <- mean(iniparams$goods[1:100])
  goods.mean.mass <- mean(iniparams$goods[101:1100])
  goods.sd.mass <- sd(iniparams$goods[101:1100])
  goods.gini.mass <- Gini(iniparams$goods[101:1100])
  goods.gini.all <- Gini(iniparams$goods)
  power.mean.elite <- mean(iniparams$power[1:100])
  power.mean.mass <- mean(iniparams$power[101:1100])
  power.sd.mass <- sd(iniparams$power[101:1100])
  power.gini.mass <- Gini(iniparams$power[101:1100])
  power.gini.all <- Gini(iniparams$power)
  subgroups.max.members <- max(table(iniparams$groupassgs[iniparams$groupassgs > 0]))
  subgroups.min.members <- min(table(iniparams$groupassgs[iniparams$groupassgs > 0]))
  subgroups.mean.members <- mean(table(iniparams$groupassgs[iniparams$groupassgs > 0]))
  groupwise.goods.gini <- Gini(tapply(iniparams$goods[101:1100], iniparams$groupassgs[iniparams$groupassgs > 0], mean))
  groupwise.power.gini <- Gini(tapply(iniparams$power[101:1100], iniparams$groupassgs[iniparams$groupassgs > 0], mean))
  
  # I'm going to need this later and do not want to have to calculate it every round
  bribe.fracmatrix <- bribe.fracmatrix.make(iniparams$numgroups)
  
  
  # now go actually run the simulation and get number of rounds
  outcome <- run.simul(iniparams, bribe.fracmatrix)  # now produces 3-item vector with rounds, number of coup attempts, and ending trust
  rounds <- outcome[1]
  attempts <- outcome[2]
  ending.trust <- outcome[3]
  
  # then take everything and give it back to the outer wrapper in the right order to go out to the concatenator
  results <- c(goods.mean.elite, goods.mean.mass, goods.sd.mass, goods.gini.mass, 
               goods.gini.all, power.mean.elite, power.mean.mass, power.sd.mass, power.gini.mass, 
               power.gini.all, iniparams$numgroups, subgroups.max.members, subgroups.min.members, 
               subgroups.mean.members, groupwise.goods.gini, groupwise.power.gini, iniparams$trust, 
               iniparams$commitment, iniparams$penalty, iniparams$errorvar, iniparams$decay, iniparams$power.decay, 
               iniparams$shockvar, rounds, attempts, ending.trust)  
  return(results)
}


# FOR ACTUAL WORKING FUNCTION remember to constrain all values that need to be constrained 
# no probabilities above 1 or below 0 etc.


# EVERYTHING AFTER THIS ACTUALLY RUNS THE SIMUL, now that all parameters are set.  
# called by: inner.wrapper()
# calls evaluate.elite(), evaluate.masses(), evaluate.outcome(), update.trust(), update.power()
# returns num.rounds, the number of rounds a given simulation lasted before the elite overthrew.

run.simul <- function(iniparams, bribe.fracmatrix) {
  iniparams$working.trust <- iniparams$trust
  iniparams$working.power <- iniparams$power
  iniparams$groupwise.goods.means <- tapply(iniparams$goods[101:1100], iniparams$groupassgs[101:1100], mean)
  attempts <- 0
  for (i in 1:1000) {
    num.rounds <- i
    iniparams$groupwise.powersum <- tapply(iniparams$working.power, iniparams$groupassgs, sum)
    elite.act <- evaluate.elite(iniparams, bribe.fracmatrix)
    # elite.act should be a vector with first item indicating first 1, 2, or 3 for status quo, overthrow w/o bribe, 
    # or overthrow w/ bribe, then remainder of items is one slot for each groups indicating bribe or not. 
    attempts <- ifelse(elite.act[1] == 1, attempts, attempts + 1)
    mass.acts <- evaluate.masses(iniparams, elite.act)
    # mass.acts should be a two-item vector w/ number of resisters and total power of resisters.
    round.outcome <- evaluate.outcome(elite.act, mass.act, iniparams$working.power) 
    # 1 if maintain, 0 if overthrow
    
    if (round.outcome == 0) break 
    
    # set new parameters...danger, danger in the side effects department??
    # to contain problems and enable checking only work with working.trust and working.power
    # not sure if this will work scope-wise.  honestly kind of tempted to just set trust and power in global environment.
    
    iniparams$working.trust <- update.trust(iniparams, elite.act, mass.acts)
    iniparams$working.power <- decay.power(iniparams) 
    iniparams$working.power <- update.power(iniparams) 
  }
  
  outcome <- c(num.rounds, attempts, iniparams$working.trust)
  return(outcome)
}

# EVERY FUNCTION AFTER THIS RUNS EVERY ROUND, CONTRIBUTES TO THE EVALUATION OF A SINGLE ROUND.

# calculate the choice of the elite.  
# algorithm: first estimate the trust parameter, then calculate expected utility for doing nothing, 
# for overthrowing, and for bribing.  Then choose the act that maximizes. Return a vector described in above 
# function --- all zeroes in everything after first place if there are no bribes
# called by: run.simul(). Calls: maximize.bribe(). Returns: vector where first element is decision: 
# decisions are 1: status quo, no bribes, 2: overthrow w/ no bribe, 3: pay some bribes then overthrow.  
# rest of vector elements are amount of bribe for each group. 

evaluate.elite <- function(iniparams, bribe.fracmatrix) {
  trust.estimate <- min(max(iniparams$working.trust + rnorm(1, 0, iniparams$errorvar), 0) ,1)
  commitment.estimate <- min(max(iniparams$commitment + rnorm(1, 0, iniparams$errorvar), 0) ,1)
  status.quo.util <- iniparams$goods[1]
  bestbribe <- maximize.bribe(iniparams, trust.estimate, commitment.estimate, bribe.fracmatrix)
  # best bribe will be a vector of length num.subgroups + 1 with the utility for best bribe 
  # in first spot and the bribes for each group (total) in remaining spots.
  nonelite.power.proportion <- sum(iniparams$working.power[101:1100])/sum(iniparams$working.power)
  topple.util <- calc.topple.util(iniparams, trust.estimate)
  options <- c(status.quo.util, topple.util, bestbribe[1])
  choice <- which.max(options)
  ifelse(choice == 3, bribes <- bestbribe[2:length(bestbribe)], bribes <- rep(0, iniparams$numgroups))
  decision <- c(choice, bribes)
  return(decision)
}

# GENERATE MATRIX OF ALL POSSIBLE BRIBES as fractions of budget
# ACTUALLY RETURNS A DATA FRAME: is that ok?
bribe.fracmatrix.make <- function(numgroups){
  
  to.combine <- seq(from = 0, to = .95, by = .05)
  
  args.topass <- rep("to.combine", numgroups)
  allbribes <- expand.grid(mget(args.topass))
  betterbribes <- allbribes[rowSums(allbribes) < 1, ]
  bestbribes <- betterbribes[rowSums(betterbribes) > 0,]
  return(bestbribes)
  # this is truly ugly and needs work: 
  # 1. maybe use do.call rather than this perverse args.topass stuff?
  # 2. maybe just superassign this sucker to the global environment for numgroups 1:5 then grab whichever 
  # I need rather than having each iteration compute the same bloody thing?
}


# this function will multiple budget by fractions in the fracmatrix, then pass rowwise with applyto bribe.ufunction()
# the latter will return a utility for each row, then this function will pick and return the max.

maximize.bribe <- function(iniparams, trust.estimate, commitment.estimate, bribe.fracmatrix) {
  budget <- 10000 - sum(iniparams$goods[1:100])
  bribemat <- bribe.fracmatrix * budget
  utilities <- apply(bribemat, 1, bribe.ufunction, inipar = iniparams, trust.est = trust.estimate, commitment.est = commitment.estimate)
  toputil <- max(utilities)
  topbribe <- bribemat[which.max(utilities),]
  bestbribe <- c(toputil, topbribe)
  return(bestbribe)
}




# expected utility for throwing a coup with no bribery
calc.topple.util <- function(iniparams, trust.estimate) {
  # maximum pay is 100, i.e., one elite share of total goods.  hard-coding this now, but may generalize later 
  # so as to vary that specification, depending on time
  mass.power.share <- sum(iniparams$working.power[101:11000]) / sum(iniparams$working.power)
  probwin <- 1 - (trust.estimate * mass.power.share)
  topple.util <- 100 * probwin
  return(topple.util)
}


# mass.acts should be a two-item vector w/ number of resisters and total power of resisters.
evaluate.masses <- function(iniparams, elite.act) {
  if(elite.act[1] == 1) {
    return(c(0,0))
  } else {
    goodstemp <- iniparams$goods[101:1100]
    powertemp <- iniparams$power[101:1100]
    groupstemp <- iniparams$groupassgs[101:1100]
    masspeeps <- rbind(goodstemp, powertemp, grouptemp)
    indivcalc <- apply(masspeeps, 2, resistYN, inip = iniparams, eliteac = elite.act)    # resistyn will be 0 if no resist 1 if resist
    numresisters <- sum(indivcalc)
    totalpower <- sum[powertemp[indivcalc == 1]]
    mass.act <- c(numresisters, totalpower)
    return(mass.act)
  }
}

# applies to a vector of size 3 "citizen" where first is that citizen's goods, second is that citizen's power, third is group ID
# returns 1 if resists, 0 if no resist.
resistYN <- function(citizen, inip, eliteac) {
  fair.feeling <- citizen[1] - (sum(inip$goods[101:1100]) / 1000)
  personal.trust.est <- min(max(inip$working.trust + rnorm(1, 0, inip$errorvar), 0) ,1)
  personal.commitment.est <- min(max(inip$commitment + rnorm(1, 0, inip$errorvar), 0) ,1)
  my.commitment <- (inip$commitment + (personal.trust.est * fair.feeling)) / 2
  my.commitment <- min(my.commitment, 1)
  bribes <- elite.act[-1]
  mybribe <- bribes[citizen[3]] / length(inip$groupassgs[inip$groupassgs == citizen[3]])
  takebribe.util <- my.commitment * mybribe
  bigsum <- generalized.bribe.sigmasum(bribes, inipar)
  unbribed.powersum <- generalized.nobribe.powersum(bribes, inipar)
  problose <- 1 - (personal.trust.est * unbribed.powersum) - (personal.commitment.est * personal.trust.est * bigsum)
  fight.util <- citizen[1] - (problose * inip$penalty)
  decision <- ifelse(takebribe.util > fight.util, 0, 1)
  return(decision)
}



# evaluate.outcome
# 1 if maintain, 0 if overthrow
# also does the curly bracket have to be RIGHT after paren in if statement?
evaluate.outcome <- function(elite.act, mass.act, working.power) {
  if(elite.act[1] == 1) {  # don't overthrow
    outcome <- 1
  } else {
    prob.mass.win <- mass.act[2] / sum(working.power)
    dieroll <- runif(1)
    outcome <- ifelse(dieroll >= prob.mass.win, 1, 0)
  }
  return(outcome)
}

# update.trust
# if elites throw coup and lose, trust becomes proportion of mass who resisted.  
# if elites do not coup then trust goes up by decay times trust
# if elites coup and win then the run ends anyway and working.trust is discarded. 
# so really only meaningful is elites coup or they don't
# so function needs to get the outcome and the proportion of resisters, plus the decay.  

update.trust <- function(iniparams, elite.act, mass.acts) {
  trustme <- iniparams$working.trust
  if(elite.act[1] == 1) {  
    trustme <- trustme + trustme * iniparams$decay
  } else {
    trustme <- mass.acts[1]/1000
  }
  return(trustme)
}


# update.power
# choose two groups randomly (incl elite, who are group 0) (use sample(, replace=FALSE))
# pick a proportion from rnorm(1, 0, 0.2), constrain it to be <= 1 using previous technique
# and make it positive using absolute value.
# take that proportion from power of each person in first group, apply it to second

update.power <- function(iniparams) {
  choices <- sample(0:iniparams$numgroups, 2, replace=FALSE)  
  proportion <- min(abs(rnorm(1, 0, iniparams$shockvar)), 1)
  power <- iniparams$working.power 
  gimme <- sum(power[iniparams$groupassgs == choices[1]] * proportion)
  power[iniparams$groupassgs == choices[1]] <- power[iniparams$groupassgs == choices[1]] - power[iniparams$groupassgs == choices[1]] * proportion
  power[iniparams$groupassgs == choices[2]] <- power[iniparams$groupassgs == choices[2]] + gimme / length(power[iniparams$groupassgs == choices[2]])
  return(power)
}

decay.power <- function(iniparams) {
  power <- iniparams$working.power 
  mean.grpgd <- mean(iniparams$groupwise.goods.means)
  sd.grpgd <- sd(iniparams$groupwise.goods.means)
  testcond <- mean.grpgd - (1.5 * sd.grpgd)
  for (i in 1:iniparams$numgroups) {
    if(iniparams$groupwise.goods.means[i] < testcond) {
      power[iniparams$groupassgs == i] <- power[iniparams$groupassgs == i] - (power[iniparams$groupassgs == i] * iniparams$power.decay)
    }
  }
  return(power)
}

# written to be applied to each row of possible bribes matrix, calculate utility and 
# return that utility for a given possible bribe, described in said row
bribe.ufunction <- function(bribes, inipar, trust.est, commitment.est) {
  bigsum <- generalized.bribe.sigmasum(bribes, inipar)
  unbribed.powersum <- generalized.nobribe.powersum(bribes, inipar)
  utility <- 1 - (trust.est * unbribed.powersum)  - (trust.est * commitment.est * bigsum)
  return(utility)
}

# generalize a calculation that would otherwise show up in both mass and elite bribe util fxns
# takes a vector of bribes by group.
generalized.bribe.sigmasum <- function(bribes, inipar) {
  sigmasum <- rep(0, inipar$numgroups)
  for (i in 1:inipar$numgroups) {
    if(bribes[i] != 0) {
      sigmasum[i] <- inipar$groupwise.powersum[i] / bribes[i]
    }
  }
  bigsum <- sum(sigmasum)
  return(bigsum)
}

# ditto another mass/elite shared calc 
# weird ifelse is for paranoia abt behavior when all groups get nonzero bribe---I want 0 not NA or NULL or some shit 
generalized.nobribe.powersum <- function(bribes, inipar) ifelse(all(bribes != 0), 0, sum(inipar$groupwise.powersum[bribes == 0]))


# function to test the whole thing without spitting out a bunch of CSVs etc.
testruns <- function(runs) {
  library(ineq)  # for later
  results <- data.frame(matrix(NA, nrow=runs, ncol=26))
  colnames(results) <- c("goods.mean.elite", "goods.mean.mass", "goods.sd.mass", "goods.gini.mass", 
                         "goods.gini.all", "power.mean.elite", "power.mean.mass", "power.sd.mass", "power.gini.mass", 
                         "power.gini.all", "num.subgroups", "subgroups.max.members", "subgroups.min.members", 
                         "subgroup.mean.members", "groupwise.goods.gini", "groupwise.power.gini", 
                         "trust", "commitment", "penalty", "errorvar", "decay", "power.decay", "shockvar", "rounds", 
                         "attempts", "ending.trust")
  for (i in 1:runs) {
    results[i,] <- outer.wrapper()
  }
  return(results)
}

# quickie function to get all the CSVs back
itsAlive <- function(filenames) {
  biglist <- lapply(filenames, read.csv)   # should work with vector of character strings
  library(plyr)
  bigdf <- rbind.fill(biglist)
  return(bigdf)
}