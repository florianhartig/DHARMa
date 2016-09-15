

# http://win-builder.r-project.org/upload.aspx


devtools::revdep()

revdep_maintainers

res <- devtools::revdep_check()
devtools::revdep_check_summary(res)
devtools::revdep_check_save_logs(res)
