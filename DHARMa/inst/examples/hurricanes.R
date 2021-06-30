# Loading hurricanes dataset

library(DHARMa)
data(hurricanes)

str(hurricanes)

# this is the model fit by Jung et al.
library(glmmTMB)
originalModelGAM = glmmTMB(alldeaths ~ scale(MasFem) *
                             (scale(Minpressure_Updated_2014) + scale(NDAM)),
                           data = hurricanes, family = nbinom2)

# no significant deviation in the general DHARMa plot
res <- simulateResiduals(originalModelGAM)
plot(res)

# but residuals ~ NDAM looks funny, which was pointed 
# out by Bob O'Hara in a blog post after publication of the paper
plotResiduals(res, hurricanes$NDAM)

# we also find temporal autocorrelation
res2 = recalculateResiduals(res, group = hurricanes$Year)
testTemporalAutocorrelation(res2, time = unique(hurricanes$Year))

# task: try to address these issues - in many instances, this will 
# make the MasFem predictor n.s.