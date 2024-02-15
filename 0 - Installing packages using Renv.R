#------------------------------------------------------------------------------#
# Exploring Vignettes                                                          #
# 0 - Handling packages using Renv                                             #
#------------------------------------------------------------------------------#

# Initialise Renv
renv::init()

# The skpr function generates and evalues experimental designs
# This requires a manual installation of "Matrix" first
renv::install("Matrix")
renv::install("skpr")
library(skpr)

# Tidyverse is just generally advised for manipulation
renv::install("tidyverse")
library(tidyverse)

# There are a few packages required for the analysis
renv::install("ordinal")
library(ordinal)

renv::install("parameters")
library(parameters)

renv::install("emmeans")
library(emmeans)

renv::install("see")
library(see)


renv::snapshot()
