


res <- devtools::revdep_check("DHARMa")
devtools::revdep_check_summary(res)
devtools::revdep_check_save_logs(res)
