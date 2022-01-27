
library(rhub)
# validate_email(email = "florian.hartig@ur.de")

rhub::platforms()
pltfms = c("ubuntu-rchk")
rhub::check(path = "./DHARMa", platform = pltfms)


# submitting to winbuilder

packageFile = "./DHARMa/"

devtools::check_win_devel(packageFile, quiet = T)
devtools::check_win_release(packageFile, quiet = T)
devtools::check_win_oldrelease(packageFile, quiet = T)


