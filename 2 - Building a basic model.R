#------------------------------------------------------------------------------#
# Vignette Analysis                                                            #
# Practical example                                                            #
#------------------------------------------------------------------------------#

# Now we actually try and analyse some data

#------------------------------------------------------------------------------#
# Load data                                                                 ####
#------------------------------------------------------------------------------#

df_vignette <- read.csv("vignette_data.csv")

# Explore the file
summary(df_vignette)
head(df_vignette)
tail(df_vignette)

#------------------------------------------------------------------------------#
# Tidy the data                                                             ####
#------------------------------------------------------------------------------#

# Ensure categorical variables are factors
df_vignette$female <- factor(df_vignette$female)
df_vignette$condition <- factor(df_vignette$condition)
df_vignette$medium <- factor(df_vignette$medium)

# The outcome rating is to be made an ordered factor, as this is an ordinal model
df_vignette$severity <- ordered(df_vignette$severity)

summary(df_vignette)

# We need to check what contrasts are used. R tends to default to 0/1 dummy
# coding (aka 'treatment' contrasts) but we want -1/+1 contrasts
# (aka 'sum' contrasts'). This gives ANOVA-like parameterisation and 
# helps estimation.
# If have tractional factorial design or many dimensions, make sure to 
# choose the contrasts intentionally to avoid it getting messy!

options('contrasts')
options(contrasts = c("contr.sum", "contr.poly"), digits = 2)
options('contrasts')

# Check that level labels are assigned
levels(df_vignette$female) #unassigned!
levels(df_vignette$female) <- c('male', 'female')
levels(df_vignette$condition) #assigned fine
levels(df_vignette$medium) #assigned fine

# Check again to make sure nothing has gone wonky
head(df_vignette)
tail(df_vignette)
summary(df_vignette)

#------------------------------------------------------------------------------#
# Build a basic mode, severity rating as outcome                            ####
#------------------------------------------------------------------------------#

model_0 <- ordinal::clmm( 
  data = df_vignette,
  formula = severity ~  1 + (1|participant) + (1|vignette))
model_0
summary(model_0)

# Analyse the variances:
# Variance due to participants:
2.4/(2.400 + 0.935)

# Variance due to vignettes:
0.935/(2.400 + 0.935)

# These thresholds can be turned into cumulative probabilities as this model
# is a logit link
exp(-5.10)/(1+exp(-5.10))
exp(-3.17)/(1+exp(-3.17))
exp(-1.06)/(1+exp(-1.06))
exp(1.24)/(1+exp(1.24))

#------------------------------------------------------------------------------#
# Fitting a series of models                                                ####
#------------------------------------------------------------------------------#

# This model is a one-way main effects model (just to test main effects)
model_1 <- ordinal::clmm( 
  data = df_vignette,
  formula = severity ~  1 + 
    (condition + medium + female) +
    (1|participant) +
    (1|vignette)
)
summary(model_1)

# This model has all two-way interactions:
model_2 <- ordinal::clmm( 
  data = df_vignette,
  formula = severity ~  1 + 
    (condition + medium + female)^2 +
    (1|participant) +
    (1|vignette)
)
summary(model_2)

# This model has three-way interactions, which is possible because we have a
# full model with no aliasing)
model_3 <- ordinal::clmm( 
  data = df_vignette,
  formula = severity ~  1 + 
    (condition + medium + female)^3 +
    (1|participant) +
    (1|vignette)
)
summary(model_2)

# Let's now do model comparisons
model_comparison <- anova(model_0, model_1, model_2, model_3)
# AIC and Likelihood Ratio Tests suggests model_2 as being the best fit

# Let's check for the main effects
drop1(model_1, test = "Chisq")
# Dropping condition has a significant effect on the model

# Let's check for two-way interaction effects
drop1(model_2, test = "Chisq")
# medium:female and condition:female both significant effects if dropped

# We can also check if a random effect is needed (removing 1|vignette)
anova(model_2, update(model_2, ~ . -(1|vignette)))
# Fit gets worse, AIC 4346 to 4533, also lowering log likelihood

#------------------------------------------------------------------------------#
# Plotting effects                                                          ####
#------------------------------------------------------------------------------#

result_0 <- model_parameters(model_0)
plot(result_0)

result_1 <- model_parameters(model_1)
plot(result_1)

result_2 <- model_parameters(model_2)
plot(result_2)

# add prettier labels
newlabs <- c('Medium x Gender', 'Behaviour x Gender', 'Behaviour x Medium', 'Gender', 'Medium', 'Behaviour', '4|5', '3|4', '2|3', '1|2')
plot(model_parameters(model_2, exponentiate=FALSE)) + 
  ggplot2::labs(y = 'Parameter', x = 'Estimate (log odds)') + 
  ggplot2::scale_y_discrete(labels = newlabs) +
  ggplot2::theme_bw() + 
  ggplot2::theme(legend.position = "none")

#------------------------------------------------------------------------------#
# Using emmeans to explore the model                                        ####
#------------------------------------------------------------------------------#

# average coefficients on log odds scales for female effect
emmeans(model_2, specs=c('female'))

# cumulative probability of each rating at each threshold (averaging over other 
# effects)
emmeans(model_2, spec=c('cut', 'female'), mode='cum.prob')

# probability of a rating (averaging over other effects)
emmeans(model_2, specs=c('female'), mode='prob')

# get the mean rating by condition (from the ordinal model)
# Note that the mean rating is from the model and the ratings are 1-5 (even if 
# you number the ratings differently)
emmeans(model_2, spec=c('female'), mode='mean.class')

# plotting the effects 
# estimated marginal effects on long odds scale (can be hard to interpret) with 
# 95% confidence intervals
emmip(model_2, female ~ condition, CIs=T)

# richer and more accurate to look at the predicted probabilities (reccomended for interpretation of effects)
# dot plot
emmip(model_2, 
      ~ severity | condition + female, 
      mode= "prob", 
      CIs = F, 
      linearg = list(linetype = 0)) + 
  ggplot2::labs(x = 'Severity rating', y = 'Probability') + 
  ggplot2::theme_bw()

# dot plot with CIs
emmip(model_2, ~ severity | condition + female, 
      mode = "prob", 
      CIs = T, 
      linearg = list(linetype = 0)) + 
  ggplot2::labs(x = 'Severity rating', y = 'Probability') + 
  ggplot2::theme_bw()

# plotting the other interaction
emmip(model_2, ~ severity | medium + female, 
      mode = "prob", 
      CIs = T, 
      linearg = list(linetype = 0)) + 
  ggplot2::labs(x = 'Severity rating', y = 'Probability') + 
  ggplot2::theme_bw()
