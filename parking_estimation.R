
# 1. Load Packages --------------------------------------------------------

# in order to use PUMS data
# remotes::install_github("walkerke/tidycensus") 

library(janitor)
library(tidycensus)
library(leaflet)
library(mapview)
library(tidyverse)
library(magrittr)
# library(flextable)
library(here)
# library(sf)
library(survey)
library(srvyr)
# library(brms)
# library(tidybayes)
# library(patchwork)
# library(waffle)
# library(testthat)
# library(devtools)
# library(hrbrthemes)


# 2. Load functions -------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))

# 3. Load data ------------------------------------------------

# Here I enter summary data from the Schlossberg and Amos 2015 paper.
# You may need to do additional data entry and cleaning to get to this point.
df <- tibble(
  Neighborhood = c("South University", 
                   "North Eugene", 
                   "Northwest Eugene"),
  Houses = c(298, 398, 323),
  Driveways = c(298, 398, 323),
  `Spaces in driveways` = c(361, 772, 887),
  `Spaces on the street` = c(965, 1041, 330),
  `Cars parked in driveways` = c(226, 369, 286),
  `Cars parked on the street` = c(204, 66, 38)
  ) 

# Set the appropriate reference year for the Census data
yr = 2013

# Set the appropriate grouping for the Census data
survey_grouping = "acs5"

# Set the relevant county
county = "Lane"

# Set the relevant state
state = "OR"

# Set the geography to block group - don't change
geog = "block group"


# The following can be useful to view the available variables in the ACS data
vars_census <- load_variables(2013, "acs5", cache = TRUE)
# View(vars_census)


# 4. Identify Census Block Groups -----------------------------------------

# To determine which block groups to enter below, go to:
# https://tigerweb.geo.census.gov/tigerweb/

# 4.1. Download 2013 5-yr ACS data on car access --------------------------
cars <- get_acs(state = state, 
                county = county, 
                geography = geog, 
                variables = c("B25046_001"), # This is the appropriate table for car access in the ACS
                year = yr,
                geometry = TRUE,
                survey = survey_grouping) %>% 
  filter(NAME %in% c("Block Group 1, Census Tract 48, Lane County, Oregon",
                     "Block Group 2, Census Tract 49, Lane County, Oregon",
                     "Block Group 2, Census Tract 29.02, Lane County, Oregon",
                     "Block Group 1, Census Tract 25.03, Lane County, Oregon")) %>%
  mutate(
    # This step groups multiple block groups into a single neighborhood label.
    # It may not be necessary in your study.
    # The labels should be the same as what you use in the field observation data frame.
    Neighborhood = case_when(
      NAME %in% c("Block Group 1, Census Tract 48, Lane County, Oregon",
                  "Block Group 2, Census Tract 49, Lane County, Oregon") ~ "South University",
      NAME %in% c("Block Group 2, Census Tract 29.02, Lane County, Oregon") ~ "North Eugene",
      NAME %in% c("Block Group 1, Census Tract 25.03, Lane County, Oregon") ~ "Northwest Eugene"
    )
  ) %>%
  # These steps help take care of standard errors when combining multiple groups.
  mutate(se = moe / 1.645,
         se_2 = se ^ 2) %>%
  group_by(Neighborhood) %>%
  summarize(`Total cars (census)` = sum(estimate),
            moe = 1.645 * sqrt(sum(se_2)))

# Check you've selected the right neighborhoods
mapview(cars, 
        zcol= "Neighborhood", 
        legend = TRUE)

# 4.2. Download 2013 5-yr ACS data on number of households ----------------

# This follows the same steps as above, but for houses
houses <- get_acs(state = state, 
                  county = county, 
                  geography = geog, 
                  variables = c("B25003_001"),
                  survey = survey_grouping,
                  year = yr) %>% 
  filter(NAME %in% c("Block Group 1, Census Tract 48, Lane County, Oregon",
                     "Block Group 2, Census Tract 49, Lane County, Oregon",
                     "Block Group 2, Census Tract 29.02, Lane County, Oregon",
                     "Block Group 1, Census Tract 25.03, Lane County, Oregon")) %>%
  mutate(Neighborhood = case_when(
    NAME %in% c("Block Group 1, Census Tract 48, Lane County, Oregon",
                "Block Group 2, Census Tract 49, Lane County, Oregon") ~ "South University",
    NAME %in% c("Block Group 2, Census Tract 29.02, Lane County, Oregon") ~ "North Eugene",
    NAME %in% c("Block Group 1, Census Tract 25.03, Lane County, Oregon") ~ "Northwest Eugene"
  )) %>%
  mutate(se = moe / 1.645,
         se_2 = se ^ 2) %>%
  group_by(Neighborhood) %>%
  summarize(`Total houses (census)` = sum(estimate),
            moe = 1.645 * sqrt(sum(se_2)))

# 5. Process data ---------------------------------------------------------

# Join the cars and houses dataframes into a single "census" dataframe
census <- cars %>%
  left_join(houses,
            by = "Neighborhood") 

# This step comes up with a cars per house ratio. 
# This is only necessary if your study area doesn't match the block group geography.
census %<>%
  mutate(`Total cars per house (census)` = `Total cars (census)` / `Total houses (census)`)

# Join the field observations with the census data
df %<>%
  left_join(census %>%
              select(Neighborhood, `Total cars per house (census)`),
            by = "Neighborhood")

# Calculate summary statistics from field observations and census data
df %<>%
  mutate(`Spaces in garages` = case_when(
    `Neighborhood` %in% c("South University") ~ 1 * Houses,
    TRUE ~ 2 * Houses
  ),
  Cars = round(Houses * `Total cars per house (census)`),
  `Cars parked in garages` = Cars - (`Cars parked in driveways` + `Cars parked on the street`),
  `Vacancy rate - on street` = (`Spaces on the street` - `Cars parked on the street`) / `Spaces on the street`,
  `Vacancy rate - driveways` = (`Spaces in driveways` - `Cars parked in driveways`) / `Spaces in driveways`,
  `Vacancy rate - garages` = (`Spaces in garages` - `Cars parked in garages`) / `Spaces in garages`,
  `Car location - on street` = `Cars parked on the street` / Cars,
  `Car location - driveways` = `Cars parked in driveways` / Cars,
  `Car location - garages` = `Cars parked in garages` / Cars,
  `Spaces per vehicle` = (`Spaces on the street` + `Spaces in driveways` + `Spaces in garages`) / Cars)

# Create a summary table that's organized by summary statistic on rows and study areas/neighborhoods on the columns
summary_table <- df %>%
  select(Neighborhood,
         `Spaces on the street`,
         `Spaces in driveways`, 
         `Spaces in garages`,
         `Cars parked on the street`,
         `Cars parked in driveways`,
         Cars,
         `Cars parked in garages`,
         `Vacancy rate - on street`,
         `Vacancy rate - driveways`,
         `Vacancy rate - garages`,
         `Car location - on street`,
         `Car location - driveways`,
         `Car location - garages`,
         `Spaces per vehicle`) %>%
  pivot_longer(cols = `Spaces on the street`:`Spaces per vehicle`,
               names_to = "Characteristic",
               values_to = "value") %>%
  pivot_wider(id_cols = Characteristic, names_from = Neighborhood)
