


# http://win-builder.r-project.org/upload.aspx



# Reverse dependencies

# https://github.com/yihui/crandalf

devtools::revdep()

revdep_maintainers

res <- devtools::revdep_check()
devtools::revdep_check_summary(res)
devtools::revdep_check_save_logs(res)


# check when a function was introduced to base
newsDB <- news()
print(news(grepl("startsWith",Text), db=newsDB), doBrowse=FALSE)

