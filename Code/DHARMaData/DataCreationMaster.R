# Data creation
  
hurricanes <- read_csv("../Code/DHARMaData/raw-data/hurricane.csv")

colnames(hurricanes) = c("Year","Name","MasFem","MinPressure_before",  "Minpressure_Updated_2014","Gender_MF","Category","alldeaths", "NDAM","Elapsed_Yrs","Source","ZMasFem",  "ZMinPressure_A","ZNDAM")
str(hurricanes)


usethis::use_data(hurricanes, pkg = "../DHARMa/.", overwrite = TRUE, compress = 'xz')

