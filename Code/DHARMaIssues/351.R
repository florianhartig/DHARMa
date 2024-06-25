m0 = glmmTMB(SiblingNegotiation ~ log(FoodTreatment) + 
               offset(log(BroodSize)),
             family = nbinom1, data = Owls)
newR = getSimulations(m0, 1)

newData = model.frame(m0)

update(m0, data = newData)

model.frame(m0)



m1 = glmmTMB(SiblingNegotiation ~ FoodTreatment + 
               offset(log(BroodSize)),
             dispformula = ~ FoodTreatment,
             family = nbinom1,
             data = Owls)

simulateLRT(m0,m1, n = 10)


