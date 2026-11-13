library(gamlss)
library(dplyr)

#create dataset  
df <- data.frame(x = rep(0:9,1000), 
                 mean = rep(c(6900,6900,7000,7600,7200,7900,7900,8100,8500,8800), 1000), 
                 s = rep(c(43400,40200,36700,94200,31100,50600,45600,43600,53300,38400), 1000)) %>% 
  rowwise() %>% 
  mutate(y = rlnorm(1, log(mean^2 / sqrt(s^2 + mean^2)), sqrt(log(1 + (s^2 / mean^2))))) %>% 
  ungroup()
# fit model
df_gam <- gamlss(y ~ cs(x, df = 4), 
                 sigma.formula =~ x,
                 data = df,
                 family = LOGNO(),
                 #method = CG(),
                 trace = TRUE)

predict(df_gam)
simulate(df_gam) # not implemented
