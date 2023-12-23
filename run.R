source("_targets.R")
try({tar_prune()}, silent=TRUE)
tar_make()

tar_load_everything(strict=FALSE)

system("quarto render")

system("cp CNAME docs/CNAME")
system("open docs/index.html")
