# Loading hurricanes dataset

library(DHARMa)
data(hurricanes)

str(hurricanes)
library(glmmTMB)
originalModelGAM = glmmTMB(alldeaths ~ scale(MasFem) *
                             (scale(Minpressure_Updated_2014) + scale(NDAM)),
                           data = hurricanes, family = nbinom2)

# Residual checks with DHARMa
res <- simulateResiduals(originalModelGAM)
plot(res)

# no significant deviation in the general plot, but try this
# which was highlighted by https://www.theguardian.com/science/grrlscientist/2014/jun/04/hurricane-gender-name-bias-sexism-statistics
plotResiduals(res, hurricanes$NDAM)

# we also find temporal autocorrelation
res2 = recalculateResiduals(res, group = hurricanes$Year)
testTemporalAutocorrelation(res2, time = unique(hurricanes$Year))
