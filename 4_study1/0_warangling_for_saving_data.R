library(tidyverse)


names(tudibase_item_pilot)[names(tudibase_item_pilot) == "prolific_pid"] <- "ID"

tudibase_item_pilot <- tudibase_item_pilot %>%
    mutate(ID = 1:nrow(.))

save(tudibase_item_pilot, file = "data/pilot_item_testing.Rdata")
write.csv(x=tudibase_item_pilot, file="data/pilot_item_testing.csv",
          row.names = F)
