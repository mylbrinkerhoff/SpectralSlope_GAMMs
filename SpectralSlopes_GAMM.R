####################################################################
###
###   SpectralSlopes_GAMM.R
###   
###   This 
###
###   M. Brinkerhoff  * UCSC  * 2024-07-14 (Su)
###   
####################################################################

# Libraries that are required
library(mgcv)
library(itsadug)
library(tidyverse)
library(viridis)
library(reshape2)
library(readr)
library(sp) # for colors which also print well in grayscale
source("R/gamm_hacks.R")

# Loading Data
slz.clean <- read.csv("data/processed/slz_cleaned.csv", header = TRUE)

### convert certain columns into factors.
slz.clean$Phonation <- factor(slz.clean$Phonation, levels = c("modal", 
                                                              "breathy", 
                                                              "checked", 
                                                              "laryngealized"))
slz.clean$Speaker <- slz.clean$Speaker %>% factor()
slz.clean$Word <- slz.clean$Word %>% factor()
slz.clean$Vowel <- slz.clean$Vowel %>% factor()
slz.clean$Iter <- slz.clean$Iter %>% factor()
slz.clean$Tone <- slz.clean$Tone %>% factor()
slz.clean$Position <- slz.clean$Position %>% factor()

summary(slz.clean)
str(slz.clean)


# GAMM analysis for H1-H2
## Linear model
h1h2.gam.linear <- bam(h1h2c ~ Phonation, 
                      data = slz.clean, method = "ML")
summary(h1h2.gam.linear)

## Extend the linear model over time
h1h2.gam <- bam(h1h2c ~ Phonation +
                 s(measurement.no, by = Phonation, bs = "cr"),
               data = slz.clean, method = "ML")
summary(h1h2.gam)
gam.check(h1h2.gam)


## Separating out the intercept difference and non-linear difference
slz.clean$phon.ord <- as.ordered(slz.clean$Phonation)
contrasts(slz.clean$phon.ord) <- "contr.treatment"

h1h2.gam.diff <- bam(h1h2c ~ phon.ord +
                     s(measurement.no) +
                     s(measurement.no, by = phon.ord),
                   data = slz.clean, method = "ML")
summary(h1h2.gam.diff)

four.colors <- bpy.colors(n = 4, cutoff.tails = 0.7, alpha = 1.0)
par(mfrow=c(2,2))
plot_smooth(h1h2.gam.diff, view="measurement.no",
            plot_all="phon.ord", rug=F, col = four.colors)

plot_diff(h1h2.gam.diff, view="measurement.no", 
          comp=list(phon.ord=c("checked","modal")))

plot_diff(h1h2.gam.diff, view="measurement.no", 
          comp=list(phon.ord=c("breathy","modal")))

plot_diff(h1h2.gam.diff, view="measurement.no", 
          comp=list(phon.ord=c("laryngealized","modal")))

