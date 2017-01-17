

# Control + , , s to run current selection

v <- 1:3
df <- data.frame(v, c('a', 'b', 'c'))
df

mtcars
mtcars$mpg


# BAYES THEOREM =====================================================
# rainier when it's colder
temp <- c('hot', 'hot', 'hot', 'cold', 'cold', 'cold')
precip <- c('sun', 'rain', 'sun', 'rain', 'sun', 'rain')

# want probability that it's cold, given that it's rainy
# p(c | r) = p(r | c) * p(c) / p(r)
p_c <- length(temp[temp == 'hot']) / length(temp)
p_r <- length(precip[precip == 'rain']) / length(precip)

# on any given day, it's equally likely to be hot or cold
# on any given day, it's equally likely to be sunny or rainy

# temp and precip, given temp is cold
temp_cold <- temp[temp == 'cold']
precip_cold <- precip[temp == 'cold'] # precip on cold days
p_r_c <- length(precip_cold[precip_cold == 'rain']) / length(precip_cold)
# how many days are rain, if it's cold / how many cold days # 2/3 # 67%

p_c_r  <- p_r_c * p_c / p_r
# posterior = (likelihood * prior) / average likelihood
# estimate = (2nd fork - rain from cold * first fork - cold) / all forks to rain

# "garden of forking data"
# if it's cold, two paths to rain / 3 total paths to rain
# number of paths given cold / total number of paths
hot
	sun
	rain
	sun
cold
	rain
	sun
	rain

# BAYES THEOREM =====================================================


# BINOMIAL DISTRIBUTION ============================================
# binomial distribution: every coin toss independent of each other and
#  probability of heads is same on every toss
dbinom
# in globe-tossing example:
# 		if you have 6 waters and 9 total tosses (3 lands), assuming 50% 
# 		chance of land or water on each toss, what's the likelihood of
# 		getting that result?
dbinom(6, size = 9, prob = 0.5) # 0.16
# So this says that the total 'paths through the garden' to get to 6 waters
# 		out of 9 tosses divided by the total 'paths through the garden' is
# 		.16, assuming 50-50 chance of water on each toss.

# If you toss a coin twice and get two heads, whats the likelihood?
# Heads
# 		Heads
# 		Tails
# Tails
# 		Heads
# 		Tails
# There's one path thru the garden leading to that outcome, and 4 total
# 		paths through the garden. So the likelihood of getting that 
# 		outcome is 1 / 4
dbinom(2, size = 2, prob = 0.5)

# If you toss a coin 3 times and get 2 heads, what is the likelihood of that happening:
#   Three paths thru the garden that have 2 heads
#   Eight paths thru the garden total
#   So likelihood of that happening is 3 / 8 or 37.5%
# Heads
# 		Heads
#       Heads
#       Tails
# 		Tails
#       Heads
#       Tails
# Tails
# 		Heads
#       Heads
#       Tails
# 		Tails
#       Heads
#       Tails
dbinom(2, size = 3, prob = 0.5) # correct!
# or worded differently, what's the probability of tossing a coin 3 times and getting two heads?
# BINOMIAL DISTRIBUTION =============================================


# Grid approximation ================================================
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)
# 20 values between 0 and 1

# define prior
prior <- rep(1, 20)
# 20 1's?

# compute likelihood at each value in grid
likelihood <- dbinom(6, size = 9, prob = p_grid)
# What are the chances of getting 6 Ws on 9 draws, given each % chance of getting a W?
#   1) What are the odds of getting 6 Ws if there's a zero % chance of getting W on a draw?
#     zero
#   2) What are the odds of getting 6 Ws if there's a 5% change of getting W on any toss? e.g. if the earch is 25% covered in water
#     .01
#   3) What are the odds of getting 6 Ws if there's a 50% chance of getting a W on any toss? e.g. if the earth is 50% water
#     .14% chance
#   4) What are the odds of getting 6 Ws if there's a 75% chance of getting a W on any toss? e.g. if earth is 75% water
#     .25% chance
#   (if there's a zero percent chance of getting a W, there's a zero % chance of getting 6 Ws on 9 tosses)
#   (if there's a 100% chance of getting a W, there's a zero % chance of getting less than 9 Ws on 9 tosses)
#   so it'll be low on each end and balloon out in the middle, but be skewed upwards because we're 2/3rds water.
# So it's like you're testing the output for every possible probability, to infer what the true value probably is.
# You know that there's an either-or outcome - a binomial outcome, you know the number of Ws and the number of tosses,
#   but you don't know what the probability of getting a W is generally. So you assume every probability from 0 to 100%
#   and see what the likelihood of getting 6 Ws on 9 outcomes would be at each assumed value, then plot the results
#   to see which one most likely explains getting 6 Ws on 9 outcomes.
# So you only have the results from your tosses, you don't know what proportion of the earth is covered in water, 
#   and you're using the results to make an estimate of how much of the earth is covered in water. So you 
#   plug in values for the earth being covered in no water, 50% water, and 100% water, test the likelihood of 
#   having 6 Ws in 9 total tosses for each one, and see which one has the highest likelihood / best explains the
#   results you got.

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# I think prior might be for weighting this trial against previous estimates using different tosses. Maybe if you
#   have 3 Ws in 5 tosses, or 10 Ws in 12 tosses, you'd weight this experiment aginst the prior values.
# But maybe here, for simplicity, the author just kept it 1 for simplicity.

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# plot
plot(p_grid, posterior, type = 'b', xlab = 'probability of water', ylab = 'posterior probability')
mtext('20 points')
# "if the probability of getting W on a toss is 67%, or if the world is 2/3rds covered in water, then there's
#   a 14% chance of getting the result that we observed of 6 Ws in 9 tosses"
# Posterior probability is the probability of getting the result you observed

# So this is trying to figure out how much of globe is covered in water, INSTEAD of how many blue 
#   marbles are in the bag.

# Replicate the different priors in Figure 2.5 
p_grid <- seq(from = 0, to = 1, length.out = 20)
# prior <- rep(1, 20)
# prior <- ifelse(p_grid < 0.5, 0, 1) # Figure 2.6 - say all values less than .5 are impossible for some reason
# prior <- exp(-5 * abs(p_grid - 0.5)) # Figure 2.6
likelihood <- dbinom(6, size = 9, prob = p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)
plot(p_grid, posterior, type = 'b', xlab = 'probability of water', ylab = 'posterior probability')

outcomes <- c('w', 'l', 'w', 'w', 'w', 'l', 'w', 'l', 'w')
p_grid <- seq(from = 0, to = 1, length.out = 200) # take 20 samples of assumed frequency of water
prior <- rep(1, length(p_grid)) # no prior to start with; blank slate
ws <- 0
for (i in seq_along(outcomes)) {
  message('n = ', i) # print number of trial this is
  outcome <- outcomes[i]
  message('Just drew a ', outcome) # print outcome of current trial
  if (outcome == 'w') ws <- ws + 1
  likelihood <- dbinom(ws, size = i, prob = p_grid)
  unstd.posterior <- likelihood * prior # if you throw out the prior, you're omitting info that you've gained in each stage. It's not the just last stage that matters; it's the info you gain from each in aggregate
  posterior <- unstd.posterior / sum(unstd.posterior)
  plot(p_grid, posterior, type = 'b', xlab = 'probability of water', ylab = 'posterior probability')
  Sys.sleep(1) # stop for 3 seconds to look at the plot
  prior <- posterior # update the prior after each run
}
# "if the earth is 70% water, there's a 30% chance of tossing sequence outcomes; 
#   if the earth is 60% water, there's a 15% chance of tossing the sequence of outcomes"
# Grid approximation ================================================


# Grid approximation - marbles ============================================
# For practice, to see if I can do it using another example and hopefully
#   understand this stuff a little better...
# Let's say you have a bag of 5 marbles and you draw 3, with replacement
#   You draw black, white, black. How many of the coins in the bag are black?
# Using grid expansion:
p_grid <- c(0, 1/5, 2/5, 3/5, 4/5, 1) # 0, 25%, 50% or 2, 75% or three, 100%.
prior <- rep(1, length(p_grid))
likelihood <- dbinom(3, size = 5, prob = p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)
plot(p_grid, posterior, type = 'b', xlab = 'proportion of black', ylab = 'posterior probability')
as.matrix(posterior)
# So this says that the posterior probability of 1 black marble is 0%, of 2 black marbles
#   is 13%, 3 black marbles is 47%, 4 black marbles is 49%, and 5 black marbles is 0%.
# e.g. there's a 47% chance that you have 3 black marbles and 2 white marbles, given that you
#   drew 3 blacks on 5 draws.

p_grid <- c(0, .25, .5, .75, 1) # 0, 25%, 50% or 2, 75% or three, 100%.
prior <- rep(1, length(p_grid))
likelihood <- dbinom(2, size = 3, prob = p_grid) # So the 50% chance of either black or white is baked into the
#   dbinom() function call - you're assuming an binomial distribution. p_grid is the frequency of black marbles
#   in the bag.
unstd.posterior <- likelihood * prior
# posterior is the likelihood of a certain frequency of marbles given what you drew
posterior <- unstd.posterior / sum(unstd.posterior)
plot(p_grid, posterior, type = 'b', xlab = 'proportion of black', ylab = 'posterior probability')
as.matrix(posterior)
# Grid approximation - marbles ============================================


# quadratic approximation ======================================================
library(rethinking)
globe.qa <- map(
  alist(
    w ~ binom(9, p), # binomial likelihood
    p ~ dunif(0, 1) # uniform prior
  ),
  data = list(w = 6)
)
# Will need to play around with this when i have a console
# But basically this finds that maximum posterior likelihood -- which proportion
#  of water is the globe most likely to be, given 6 out of 9 tosses are water, 
#   along with the standard deviation of the curve.
# For quadratic approximation, you only need the posterior mode (max of curve)
#   and the curvature of the peak.
# So this will find the posterior mode, look at the curvature around the peak,
#   and it'll then fit a quadratic model to approximate the curve. This works
#   because you can assume the normal / gaussian distribution with a quadratic
#   funtion, because the log of a normal distribution forms a parabola.
# display summary of quadratic approximation
precis(globe.qa)

# analytical calculation
w <- 6
n <- 9
curve(dbeta(x, w + 1, n - w - 1), from = 0, to = 1)
curve(dnorm(x, .67, .16), lty = 2, add = TRUE)
# the details of this don't matter, the point is that this is 100% correct
#  and can be used to compare with the quadratic approximation.
# The takeaway here is that the quadratic approximation fails miserably when
#  the number of observations is low, but gets pretty close when it's high.
# A lot of statistical procedures use quadratic approximations, or similar, 
#   that don't work well on small data sets.
# If you have a small number of obs., grid approximation is a good option
#   because its bad performance isn't a problem and it's accurate.
# quadratic approximation ======================================================



# summary ====================================================================
# the target of bayesian inference is a posterior probability distribution,
#  which states the relative number of ways the conjectured cause of the
#  data (the underlying population data) could have caused the data (the 
#  sample of outcomes that you have). The relative numbers indicate
#  plausibilities of different conjectures. these plausibilities are updated
#  as you get more observations of data, in a process known as Bayesian
#  updating (taking the prior and multiplying it by the likelihoods to get
#  to the posterior).
# A bayesian model is a composite of a likelihood, a choice of parameters,
#  and a prior. The likelihood is the plausibility of each possilbe value
#  of the parameters, before accounting for the data. computing plausibilities
#  happens with bayes theorem, and results in the posterior distribution.
# parameters are things like: is the event binomial, how many observations are
#  there, what are the options for getting to a certain outcomes, etc.
# Bayesian models are fit to data with numerical techniques, inlcuidng gird
#  approximation, quadratic approximation, and markov monte carlo.
# grid approx is great but doesn't scale and is computationally intense,
#  quadratic is computationally efficient but bad with small number of obs.,
#  and monte carlo is good for the complexity of multilevel models.
# summary ====================================================================


# practice ====================================================================
2E1: 2 - probability of rain, given that it's monday
2E2: PAGE 45
# practice ====================================================================

