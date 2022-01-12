
devtools::check_win_release()

library(rhub)
# validate_email(email = "florian.hartig@ur.de")

rhub::platforms()
pltfms = c("ubuntu-rchk")

rhub::check(path = "./DHARMa", platform = pltfms)


# submitting to 

packageFile = "DHARMa_0.4.5.tar.gz"

devtools::check_win_devel(packageFile)
devtools::check_win_release(packageFile)
devtools::check_win_oldrelease(packageFile)


