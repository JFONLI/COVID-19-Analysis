# Check if you have these packages ----
# libs <- c("maps", "mapproj")
# 
# x <- sapply(libs, function(x)
#   if (!require(
#     x,
#     character.only = T,
#     warn.conflicts = F,
#     quietly = T
#   ))
#     install.packages(x))
# rm(x, libs)

# Convert backslashes to slashes ----
#path <- readClipboard()


path.fun <- "E:\\Data Analytics Engineering\\IE6600_ZL\\Materials\\RShiny\\exercise-1\\www\\functions\\percentMap.R"
adjPath.fun <- gsub("\\\\", "/", path.fun)

path.data <- "E:\\Data Analytics Engineering\\IE6600_ZL\\Materials\\RShiny\\exercise-1\\www\\data\\counties.RDS"
adjPath.data <- gsub("\\\\", "/", path.data)


library(maps)
library(mapproj)
source(adjPath.fun)
counties <- readRDS(adjPath.data)
percent_map(counties$white, "darkgreen", "% White")


