print("Check dependencies...")
list.of.packages <- c("ggplot2", "tidyverse", "lubridate", "rgl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)