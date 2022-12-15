####Exploratory analysis####

# Install packages
library(tidyverse)
library(ggplot2)
library(visdat)
library(tigris)
library(naniar)
library(maps)

# Read in data
forest <- read_csv("Data/forest.csv")
forest_area <- read_csv("Data/forest_area.csv")
brazil_loss <- read_csv("Data/brazil_loss.csv")
soybean_use <- read_csv("Data/soybean_use.csv")
veg_oil <- read_csv("Data/vegetable_oil.csv")

# Combining data frames
all_forests <- full_join(forest, forest_area, by = c("code", "year", "entity"))
all_forests_oil <- full_join(all_forests, veg_oil, 
                             by = c("year", "entity", "code"))
all_forests_oil_soy <- full_join(all_forests_oil, soybean_use, 
                                 by = c("entity", "code", "year"))

all_forests_oil_soy <- all_forests_oil_soy %>% 
  pivot_wider(names_from = "crop_oil", values_from = "production")

vis_miss(all_forests)
gg_miss_var(all_forests_oil)
gg_miss_var(all_forests_oil_soy)
#missing almost all of our conversion data

unique(all_forests$entity)
unique(forest$entity)

#dygraph of deforestation in brazil
library(dygraphs)
brazil_loss %>% 
  mutate(agriculture = (cumsum(commercial_crops + pasture + tree_plantations_including_palm)/1000000), 
         infrastructure = (cumsum(flooding_due_to_dams + other_infrastructure + roads)/1000000), 
         lumber = (cumsum(selective_logging + small_scale_clearing)/1000000), 
         mining = (cumsum(mining)/1000000), 
         fire = (cumsum(fire)/1000000), 
         natural_distubrances = (cumsum(natural_disturbances)/1000000)) %>% 
  select(year, agriculture, infrastructure, lumber, mining, fire, natural_disturbances) %>% 
  dygraph(main = "Cumulative Deforestation in Brazil",x = "Year", y = "Forest Area Lost (Hectares)") %>% 
  dyRangeSelector() %>% 
  dyOptions(stackedGraph = TRUE) %>% 
  dyLegend(show = "follow")
#Can just change the column names when ready to put into the dashboard




