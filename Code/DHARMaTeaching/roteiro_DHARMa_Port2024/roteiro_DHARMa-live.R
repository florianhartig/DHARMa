# ISEC DHARMa skills showcase - script for live editing
# During the skills showcase, please use Slack to interact, otherwise twitter @florianhartig

library(DHARMa)
library(lme4)
library(glmmTMB)

set.seed(123)
testData = createData(sampleSize = 200, family = poisson())

fittedModel <- glmer(observedResponse ~ Environment1 + (1 | group), 
                     family = "poisson", data = testData)

res <- simulateResiduals(fittedModel)
plot(res)

testDispersion(res)
testUniformity(res)
testOutliers(res)
testQuantiles(res)

testZeroInflation(res)
testSpatialAutocorrelation(res)
testTemporalAutocorrelation(res)

testGeneric(res, summary=mean)

res2 <- recalculateResiduals(res, group = testData$group)
plot(res2)

residuals(res)

set.seed(123)
testData = createData(sampleSize = 200, family = poisson())
fittedModel <- glm(observedResponse ~ Environment1 , 
                     family = "poisson", data = testData)
res <- simulateResiduals(fittedModel)
plot(res)


set.seed(123)
testData = createData(sampleSize = 200, family = poisson())
fittedModel <- glmer(observedResponse ~ 1 + (1|group), 
                   family = "poisson", data = testData)
res <- simulateResiduals(fittedModel)
plot(res)

plotResiduals(res, form = testData$Environment1)


plot(SiblingNegotiation ~ FoodTreatment, data = Owls)

m1 <- glm(SiblingNegotiation ~ FoodTreatment*SexParent + 
            offset(log(BroodSize)), data = Owls, family = poisson)

res <- simulateResiduals(m1)
plot(res)

m2 <- glmer(SiblingNegotiation ~ FoodTreatment*SexParent + 
            offset(log(BroodSize)) + (1|Nest), data = Owls, family = poisson)

res <- simulateResiduals(m2)
plot(res)


m3 <- glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + 
              offset(log(BroodSize)) + (1|Nest), data = Owls, family = nbinom1)

res <- simulateResiduals(m3)
plot(res)

plotResiduals(res, Owls$FoodTreatment)

testDispersion(res)
testZeroInflation(res)

m4 <- glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + 
                offset(log(BroodSize)) + (1|Nest), 
              ziformula = ~ FoodTreatment*SexParent,
              data = Owls, family = nbinom1)

res <- simulateResiduals(m4)
plot(res)

testDispersion(res)
testZeroInflation(res)

data(Salamanders)
Salamanders$pres = Salamanders$count > 0

mb1 = glm(pres ~ 0 + spp * cover, data = Salamanders, family = binomial)

res<- simulateResiduals(mb1)
plot(res)

res2 <- recalculateResiduals(res, group = Salamanders$site)
plot(res2)

