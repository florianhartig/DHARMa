library(DHARMa)
library(glmmTMB)


# read in PA dataset
PAdataset <- read.csv("non_overlapping_PAs.csv")

# calculate overlap controlling for area
PAdataset$PA_overlap_prop <-PAdataset$PA_overlap/PAdataset$AREA

# transform some continuous variables
PAdataset$remote_trans <- sqrt(PAdataset$mean_remoteness)
PAdataset$PA_overlap_trans <- sqrt(PAdataset$PA_overlap_prop)
PAdataset$elev_trans <- sqrt(PAdataset$mean_elev - min(PAdataset$mean_elev))
PAdataset$population_trans <- sqrt(PAdataset$mean_pop)
PAdataset$area_trans <- sqrt(PAdataset$AREA)
PAdataset$water_trans <- sqrt(PAdataset$prop_water)

# convert internatoinal variable to factor
PAdataset$international <- as.factor(PAdataset$international)

# scale and center covariates
str(PAdataset)
PAdataset[,c("HDI_scaled", "PS_scaled", "remoteness_scaled", "diversity_scaled", "elevation_scaled", "area_scaled", "age_scaled", "PA_overlap_scaled", "pop_scaled", "precipitation_scaled", "temp_scaled", "mamm_att_scaled", "bird_att_scaled", "species_att_scaled", "IUCN_scaled", "grassland_scaled", "forest_scaled", "water_scaled")] <- scale(PAdataset[,c("meanHDI", "meanPS", "remote_trans", "hab_diversity", "elev_trans", "area_trans", "age", "PA_overlap_trans", "population_trans", "mean_precipitation", "mean_temp", "pred.sum.mamm", "pred.sum.bird", "sum.species", "IUCN_ord", "prop_grassland", "prop_forest", "water_trans")], center = TRUE, scale = TRUE)

# separate global dataset into continents ####
africa <- PAdataset[PAdataset$CONT_FIN == "Africa",]
asia <- PAdataset[PAdataset$CONT_FIN == "Asia",] 
s.america <- PAdataset[PAdataset$CONT_FIN == "South America",]
n.america <- PAdataset[PAdataset$CONT_FIN == "North America",]
europe <- PAdataset[PAdataset$CONT_FIN == "Europe",]
oceania <- PAdataset[PAdataset$CONT_FIN == "Oceania",] 


# run hurdle model for South America
s.america.hurdle <- glmmTMB(final_count ~ species_att_scaled + grassland_scaled +
                              water_scaled + diversity_scaled + elevation_scaled +
                              temp_scaled + precipitation_scaled + age_scaled + area_scaled + 
                              IUCN_scaled + international + pop_scaled + PA_overlap_scaled + 
                              remoteness_scaled + PS_scaled,
                            zi = ~., data = s.america, family = "truncated_poisson")

summary(s.america.hurdle)


res = simulateResiduals(s.america.hurdle)
plot(res)
plot(res, smoothScatter = T, quantreg = F)
plot(res, smoothScatter = T, quantreg = T)


# explore different families
s.america.hurdle.1 <- update(s.america.hurdle, family = "truncated_nbinom1")
summary(s.america.hurdle.1)
res = simulateResiduals(s.america.hurdle.1)
predict(s.america.hurdle.1)


x = as.matrix(simulate(s.america.hurdle.1, nsim = 1000))
x[!complete.cases(x),]
min(apply(x, 1, max) - apply(x, 1, min), na.rm = T)


all(is.finite(res$simulatedResponse))


plot(res, smoothScatter = F, quantreg = F)

s.america.hurdle.2 <- update(s.america.hurdle, family = "truncated_nbinom2")
res = simulateResiduals(s.america.hurdle)
plot(res, smoothScatter = F, quantreg = F)







