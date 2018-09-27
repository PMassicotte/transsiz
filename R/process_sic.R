# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Process SIC data given by Benjamin.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

sic <- read_csv("data/raw/SIC_TRANSSIZ/SIC_subset.csv") %>% 
  janitor::clean_names() %>% 
  separate(station_id, into = c("station", "cast"), sep = "-", convert = TRUE) %>% 
  mutate_at(vars(starts_with("sic")), .funs = funs(. / 100))

write_csv(sic, "data/clean/sic.csv")
