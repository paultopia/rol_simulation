# Actual code used to generate simulation in chapter 8.
# Run on R version 3.1.2 (as I recall), in batch mode, on numerous independent
# parallel cpu instances (on high-performance cluster) to generate
# total 200k+ rounds.
# to initiate: add "simulate.ch8(NUMBER OF RUNS)" to bottom of file then run in batch.
# I ran with something like 100 parallel instances of 2k runs each.

simulate.ch8 <- function(runs) {
  library(ineq)
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
    noisename <- 100 * sample(1:999, 1)
    k <- j + noisename
    increment.name <- sprintf("run%s_%s.csv", k, timeRR())
    write.csv(results, increment.name, row.names=FALSE)  # does this handle column names right?
    # save.image()
    output.fnames[j] <- increment.name
  }
  write(output.fnames, file = "simul_filenames.txt", append = TRUE)  # does this handle column names right?
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
  errorvar.v <- runif(1, .01, .8)
  decay.v <- sample(c(0, .01), 1)
  power.decay.v <- runif(1, 0, .5)
  shockvar.v <- runif(1, .05, .9)  # larger than first set of runs
  # modification: shift probability mass toward more unequal groups.
  distortPower <- sample(1:4, 1)
  distortGoods <- sample(1:4, 1)
  unjustP <- sample(0:9, 1)
  unjustG <- sample(0:9, 1)
  if (distortPower == 1) {
    power.v <- warp(power.v, unjustP, subgroups.dist)
  }
  if (distortGoods == 1){
    goods.v <- warp(goods.v, unjustG, subgroups.dist)
  }
  iniparams <- list(goods = goods.v, power = power.v, numgroups = subgroups.num, groupassgs = subgroups.dist,
                    trust = trust.v, commitment = commitment.v, penalty = penalty.v, errorvar =  errorvar.v,
                    decay = decay.v, power.decay = power.decay.v, shockvar = shockvar.v)
  one.run <- inner.wrapper(iniparams)
  return(one.run)
}

# make things more inegalitarian.
# this might break a little with floating point imprecision, but not enough to change results.
warp <- function(distro, wFactor, groups) {
  massdistro <- distro[101:1100]
  massgroups <- groups[101:1100]
  budget <- sum(massdistro)
  num1s <- length(massgroups[massgroups == 1])
  giveto1s <- (budget * wFactor) / num1s
  for (i in 1:1000) {
    if(massgroups[i] == 1) {
      massdistro[i] <- massdistro[i] + giveto1s
    } else {
      massdistro[i] <- massdistro[i] - (massdistro[i] * wFactor)
    }
  }
  distro[101:1100] <- massdistro
  return(distro)
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
  budget <- 10000 - sum(iniparams$goods[1:100])
  bribe.fracmatrix <- bribe.fracmatrix.make(iniparams$numgroups, budget)


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
    massassgs <- iniparams$groupassgs[iniparams$groupassgs != 0]
    masspower <- iniparams$working.power[iniparams$groupassgs != 0]
    iniparams$groupwise.powersum <- tapply(masspower, massassgs, sum)
    elite.act <- evaluate.elite(iniparams, bribe.fracmatrix)
    # elite.act will be a vector with first item indicating first 1, 2, or 3 for status quo, overthrow w/o bribe,
    # or overthrow w/ bribe, then remainder of items is one slot for each groups indicating bribe or not.
    attempts <- ifelse(elite.act[1] == 1, attempts, attempts + 1)
    mass.acts <- evaluate.masses(iniparams, elite.act)
    # mass.acts will be two-item vector w/ number of resisters and total power of resisters.
    round.outcome <- evaluate.outcome(elite.act, mass.acts, iniparams$working.power)
    # 1 if maintain, 0 if overthrow

    if (round.outcome == 0) break

    # set new parameters... isolating from original parameters to avoid horrible bug dangers.

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
# (actually returns a data frame, but works fine)
bribe.fracmatrix.make <- function(numgroups, budget){
  to.combine <- seq(from = 0, to = .9, by = .1)
  args.topass <- rep("to.combine", numgroups)
  allbribes <- expand.grid(mget(args.topass))
  betterbribes <- allbribes[rowSums(allbribes) < 1, ]
  bestbribes <- betterbribes[rowSums(betterbribes) > 0,]
  bestbribes <- bestbribes * budget
  return(bestbribes)
  # this is truly ugly and could probably be optimized, but with parallel processing
  # I didn't bother.  For replication, worth considering.
}


# this function will multiple budget by fractions in the fracmatrix, then pass rowwise with applyto bribe.ufunction()
# the latter will return a utility for each row, then this function will pick and return the max.

maximize.bribe <- function(iniparams, trust.estimate, commitment.estimate, bribemat) {
  utilities <- apply(bribemat, 1, bribe.ufunction, inipar = iniparams, trust.est = trust.estimate, commitment.est = commitment.estimate)
  toputil <- max(utilities)
  topbribe <- bribemat[which.max(utilities),]
  bestbribe <- c(toputil, topbribe)
  return(bestbribe)
}




# expected utility for throwing a coup with no bribery
calc.topple.util <- function(iniparams, trust.estimate) {
  # maximum pay is 100, i.e., one elite share of total goods.
  # future versions/replications should generalize this to different maxes.
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
    masspeeps <- rbind(goodstemp, powertemp, groupstemp)
    indivcalc <- apply(masspeeps, 2, resistYN, inip = iniparams, eliteac = elite.act)    # resistyn will be 0 if no resist 1 if resist
    numresisters <- sum(indivcalc)
    totalpower <- sum(powertemp[indivcalc == 1])
    mass.act <- c(numresisters, totalpower)
    return(mass.act)
  }
}

# applies to a vector of size 3 "citizen" where first is that citizen's goods, second is that citizen's power, third is group ID
# returns 1 if resists, 0 if no resist.
resistYN <- function(citizen, inip, eliteac) {
  fair.feeling <- citizen[1] - (sum(inip$goods[101:1100]) / 1000)
  fair.feeling <- max(fair.feeling, 0)
  personal.trust.est <- min(max(inip$working.trust + rnorm(1, 0, inip$errorvar), 0) ,1)
  personal.commitment.est <- min(max(inip$commitment + rnorm(1, 0, inip$errorvar), 0) ,1)
  my.commitment <- (inip$commitment + (personal.trust.est * fair.feeling)) / 2
  my.commitment <- min(my.commitment, 1)
  my.treach <- 1 - my.commitment
  bribes <- eliteac[-1]
  mygroup <- as.numeric(citizen[3])
  numinGr <- as.numeric(length(inip$groupassgs[inip$groupassgs == mygroup]))
  mybribe <- as.numeric(bribes[mygroup]) / numinGr
  takebribe.util <- my.treach * mybribe
  bigsum <- vectorized.bribe.sigmasum(bribes, inip)
  unbribed.powersum <- generalized.nobribe.powersum(bribes, inip)
  problose <- 1 - (personal.trust.est * unbribed.powersum) - (personal.commitment.est * personal.trust.est * bigsum)
  fight.util <- citizen[1] - (problose * inip$penalty)
  decision <- ifelse(takebribe.util > fight.util, 0, 1)
  return(decision)
}



# evaluate.outcome
# 1 if maintain, 0 if overthrow
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
    trustme <- min((trustme + trustme * iniparams$decay) ,1)
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
  bigsum <- vectorized.bribe.sigmasum(bribes, inipar)
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

vectorized.bribe.sigmasum <- function(bribes, inipar){
  powers <- inipar$groupwise.powersum
  powers <- as.numeric(powers[bribes != 0])
  bribes <- as.numeric(bribes[bribes != 0])
  bigsum <- sum(powers/bribes)
  return(bigsum)
}

# ditto another mass/elite shared calc
# weird ifelse is for paranoia abt behavior when all groups get nonzero bribe---I want 0 not NA or NULL etc.
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


# test function to do smaller number of runs for timing/functionality test.

testbatch <- function(runs) {
  library(ineq)  # for later
  results <- data.frame(matrix(NA, nrow=runs, ncol=27))
  colnames(results) <- c("run.id", "goods.mean.elite", "goods.mean.mass", "goods.sd.mass", "goods.gini.mass",
                         "goods.gini.all", "power.mean.elite", "power.mean.mass", "power.sd.mass", "power.gini.mass",
                         "power.gini.all", "num.subgroups", "subgroups.max.members", "subgroups.min.members",
                         "subgroup.mean.members", "groupwise.goods.gini", "groupwise.power.gini",
                         "trust", "commitment", "penalty", "errorvar", "decay", "power.decay", "shockvar", "rounds",
                         "attempts", "ending.trust")
  for (i in 1:runs) {
    run.res <- c(i,outer.wrapper())
    results[i,] <- run.res
  }
  increment.name <- sprintf("testrun_%s.csv", timeRR())
  write.csv(results, increment.name, row.names=FALSE)  # does this handle column names right?
  # save.image()
  return(results)
}

##########
# WHAT FOLLOWS
# IS CODE FOR
# SUBSEQUENT DATA analysis
#
###########

# read data from a bunch of CSVs and concatenate
file_list <- list.files()

for (file in file_list){

  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file)
  }

  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <- read.csv(file)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }

}

write.csv(dataset, "newbigds.csv", row.names=FALSE)
str(dataset)

mainds <- dataset

# get rid of row ID field

mainds <- mainds[,-1]

# make separate DS for failures
failds <- mainds[which(mainds$rounds!=1000),]

# scale and center all except rounds
scaleds <- scale(mainds[,-24])
scaleds <- cbind(scaleds, rounds = mainds$rounds)


scalefailds <- scale(failds[,-24])
scalefailds <- cbind(scalefailds, rounds = failds$rounds)

scaleds <- as.data.frame(scaleds)
scalefailds<- as.data.frame(scalefailds)

install.packages("stargazer")
library(stargazer)

cleanerW <- lm(rounds ~ shockvar + goods.mean.mass + goods.gini.mass + power.mean.mass + power.gini.mass + num.subgroups + groupwise.goods.gini + groupwise.power.gini + trust + commitment + penalty + errorvar + decay + power.decay, data = scaleds)

cleanerX <- lm(rounds ~ shockvar*goods.mean.mass + shockvar*goods.gini.mass + shockvar*power.mean.mass + shockvar*power.gini.mass + shockvar*num.subgroups + shockvar*groupwise.goods.gini + shockvar*groupwise.power.gini + shockvar*trust + shockvar*commitment + shockvar*penalty + shockvar*errorvar + shockvar*decay + shockvar*power.decay, data = scaleds)

cleanerY <- lm(rounds ~ shockvar*goods.mean.mass + shockvar*goods.gini.mass + shockvar*power.mean.mass + shockvar*power.gini.mass + shockvar*num.subgroups + shockvar*trust + shockvar*commitment + shockvar*penalty + shockvar*errorvar + shockvar*decay + shockvar*power.decay, data = scaleds)

cleanerZ <- lm(rounds ~ shockvar*goods.mean.mass + shockvar*power.mean.mass + shockvar*groupwise.goods.gini + shockvar*groupwise.power.gini + shockvar*trust + shockvar*commitment + shockvar*penalty + shockvar*errorvar + shockvar*decay + shockvar*power.decay, data = scaleds)

cleanerBB <- lm(rounds ~ shockvar*goods.mean.mass + shockvar*goods.gini.mass + shockvar*num.subgroups + shockvar*groupwise.goods.gini + shockvar*trust + shockvar*commitment + shockvar*penalty + shockvar*errorvar + shockvar*decay, data = scaleds)

# produce html file with output for easy reading

# then repeat analysis on smaller, failures-only dataset to confirm no weird anomalies
forword01 <- stargazer(cleanerW, cleanerX, cleanerY, cleanerZ, cleanerBB, type="html", dep.var.labels = "Rounds (Stability)", out="finalD.html", df=FALSE)

cleanerW2 <- lm(rounds ~ shockvar + goods.mean.mass + goods.gini.mass + power.mean.mass + power.gini.mass + num.subgroups + groupwise.goods.gini + groupwise.power.gini + trust + commitment + penalty + errorvar + decay + power.decay, data = scalefailds)

cleanerX2 <- lm(rounds ~ shockvar*goods.mean.mass + shockvar*goods.gini.mass + shockvar*power.mean.mass + shockvar*power.gini.mass + shockvar*num.subgroups + shockvar*groupwise.goods.gini + shockvar*groupwise.power.gini + shockvar*trust + shockvar*commitment + shockvar*penalty + shockvar*errorvar + shockvar*decay + shockvar*power.decay, data = scalefailds)

cleanerY2 <- lm(rounds ~ shockvar*goods.mean.mass + shockvar*goods.gini.mass + shockvar*power.mean.mass + shockvar*power.gini.mass + shockvar*num.subgroups + shockvar*trust + shockvar*commitment + shockvar*penalty + shockvar*errorvar + shockvar*decay + shockvar*power.decay, data = scalefailds)

cleanerZ2 <- lm(rounds ~ shockvar*goods.mean.mass + shockvar*power.mean.mass + shockvar*groupwise.goods.gini + shockvar*groupwise.power.gini + shockvar*trust + shockvar*commitment + shockvar*penalty + shockvar*errorvar + shockvar*decay + shockvar*power.decay, data = scalefailds)

cleanerBB2 <- lm(rounds ~ shockvar*goods.mean.mass + shockvar*goods.gini.mass + shockvar*num.subgroups + shockvar*groupwise.goods.gini + shockvar*trust + shockvar*commitment + shockvar*penalty + shockvar*errorvar + shockvar*decay, data = scalefailds)


forword02 <- stargazer(cleanerW2, cleanerX2, cleanerY2, cleanerZ2, cleanerBB2, type="html", dep.var.labels = "Rounds (Stability)", out="finalD2.html", df=FALSE)
