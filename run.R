source("_targets.R")
tar_invalidate(data_2025)
tar_invalidate(covid_data)
try({tar_prune()}, silent=TRUE)
tar_make()

tar_load_everything(strict=FALSE)

system("quarto render")

system("cp CNAME docs/CNAME")
system("open docs/index.html")

system("git add .")
system("git commit -m 'auto-update'")
system("git push")

