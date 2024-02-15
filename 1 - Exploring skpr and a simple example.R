#------------------------------------------------------------------------------#
# Analysis of Vignettes                                                        #
# 1 - Exploration of experimental design                                       #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Simple example                                                            ####
#------------------------------------------------------------------------------#

# In this first example there are three levels:
# * A - Male or Female (2 entries)
# * B - Harassment or Bullying
# * C - Public or Private

# Contrasts are designed so that they sum to zero as this is a factorial design, for example for 
# A, Male is given -1, Female is given +1.
candidates_3way = expand.grid(
  ge = c('male', 'female'),
  be = c('bullying', 'harassment'),
  lo = c('private', 'public')
)
candidates_3way

# In skpr we can define a design using gen_design()
design1 <- skpr::gen_design(
  candidateset = candidates_3way,
  model = ~ ge + be + lo,
  trials = 8
)
design1

# We can get the alias and correlation matrices
skpr::get_attribute(design1, "alias.matrix")
skpr::get_attribute(design1, "correlation.matrix")
# These would be different if some combinations of A,B,C were missed (try
# changing 'trials' in design1 above!)

skpr::get_optimality(design1)
# I is the average prediction variance. 
# D, A, G are various efficiencies out of 100.

# Let's try having fewer trials
design1a <- skpr::gen_design(
  candidateset = candidates_3way,
  model = ~ ge + be + lo,
  trials = 4
)
design1a
# Note that this has generated random assignments from the grid. I assume this
# is without replacement until the full grid is depleted, then it regens and
# repeats.

skpr::get_attribute(design1a, "alias.matrix")
skpr::get_optimality(design1a)

# Now let's try a 5-way model with different responses per level
vu_5way <-  expand.grid(
  a = as.factor(c("male", "female", "nonbinary")),
  b = as.factor(c("student", "professor")),
  c = as.factor(c("single", "married", "divorced")),
  d = as.factor(c("young", "old")),
  e = as.factor(c("banter", "bullying", "harassment"))
)
vu_5way
dim(vu_5way)

design2 <- skpr::gen_design(
  candidateset = vu_5way,
  model = ~ (a + b + c + d + e)^2,
  trials = 108,
  blocksizes = 27
) # This is a big complex design - it will take time
# Note that this is assigning units to blocks of size 27, which does
# divide 108. If it didn't, the function would try its best to balance the
# design.

# This took time as it is trying to assign to blocks, and generate units,
# to minimise heterogeneity between units (vignettes).

skpr::get_attribute(design2, "alias.matrix")
# Fully balanced, as all things should be.

skpr::get_optimality(design2)
# I is the average prediction variance. 
# D, A, G are various efficiencies out of 100.

