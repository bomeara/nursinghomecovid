source("_targets.R")
#tar_invalidate(everything())
try({tar_prune()}, silent=TRUE)
tar_make()

tar_load_everything(strict=FALSE)

system("quarto render")

system("cp CNAME docs/CNAME")
system("open docs/index.html")

system("git add .")
system("git commit -m 'auto-update'")
system("git push")

