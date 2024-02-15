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




renv::snapshot()
