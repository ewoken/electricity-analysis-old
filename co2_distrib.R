library(tidyverse)
library(lubridate)

source("./utils.r")

data = getAllData()

ggplot(data, aes(x = co2_kg_kwh)) +
  geom_histogram(binwidth = 0.001) +
  facet_wrap(~season)
ggsave("co2_distrbution.png")
