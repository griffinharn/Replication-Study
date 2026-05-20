# Author: Griffin Harn
# Date: May 6, 2026
# Purpose: Replicate a demographic study on urban areas in Richmond, VA
# ------------------------------------------------------------ #
# install packages and call libraries and census api key
install.packages("tidyverse")
install.packages("tidycensus")
install.packages("ggplot2")
install.packages("tigris")
install.packages("scales")
library(tidyverse)
library(tidycensus)
library(ggplot2)
library(sf)
library(RColorBrewer)
library(scales)
library(tigris)
options(tigris_use_cache = TRUE)

census_api_key("01dbfbd3461fb4f7a8edc0ef5bc20f0df57f7469", install = TRUE)

# get list of all counties in VA to confirm correct names for independent cities
va_counties <- get_decennial(
  geography = "county",
  variables = "P1_001N",
  state = "VA",
  year = 2020
)

# get table for population of all Census Tracts in the Richmond MSA
rich_tracts <- get_decennial(
  geography = "tract",
  variables = "P1_001N",
  state = "VA",
  county = c(
    "Goochland", "Powhatan", "Chesterfield", 
  "Henrico", "Charles City", "New Kent",
  "Hanover", "Richmond city", "Amelia",
  "Dinwiddie", "King and Queen", "King William", "Prince George",
  "Sussex", "Petersburg city", "Hopewell city", "Colonial Heights city"),
  year = 2020,
  geometry = T
)
print(rich_tracts, n = 332)


# map all census tracts in the MSA, symbolized by population
ggplot() +
  geom_sf(data = rich_tracts, aes(fill = value))

# calculate area of tracts and divide population by area to get pop. density
density_2020 <- rich_tracts %>%
  mutate(
    area_m2 = as.numeric(st_area(geometry)),
    area_km2 = area_m2 / 1000000,
    pop_density = value / area_km2
  )

# attach labels to tracts based on urban/suburban and high/low density
density_labels <- density_2020 %>%
  mutate(
    density_class = case_when(
    pop_density >= 4500 ~ "High-Density Urban",
    between(pop_density, 1900, 4499.9) ~ "Low-Density Urban",
    between(pop_density, 1000, 1899.9) ~ "High-Density Suburban",
    between(pop_density, 800, 999.9) ~ "Mid-Density Suburban",
    between(pop_density, 550, 799.9) ~ "Low-Density Suburban",
    between(pop_density, 0.1, 549.9) ~ "Exurban",
    pop_density == 0 ~ "No Population"
  ))

# map tracts, symbolizing by population density
ggplot() +
  geom_sf(data = density_labels, aes(fill = density_class)) +
  scale_fill_manual(
    values = setNames(
      brewer.pal(6, "Blues"),
      c("Exurban", "Low-Density Suburban", 
        "Mid-Density Suburban", "High-Density Suburban",
        "Low-Density Urban", "High-Density Urban")
    ),
    breaks = c("High-Density Urban", "Low-Density Urban",
               "High-Density Suburban", "Mid-Density Suburban",
               "Low-Density Suburban", "Exurban"),
    name = "Density Classification"
    ) +
  labs(
    title = "Population Density in the Richmond Metropolitan Area"
    ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

ggsave("richmond_density.png")

# see all available palettes
display.brewer.all()

# ------------------------------------------------------------------------ #

# look at income across census tracts
rich_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "VA",
  county = c(
    "Goochland", "Powhatan", "Chesterfield", 
    "Henrico", "Charles City", "New Kent",
    "Hanover", "Richmond city", "Amelia",
    "Dinwiddie", "King and Queen", "King William", "Prince George",
    "Sussex", "Petersburg city", "Hopewell city", "Colonial Heights city"),
  year = 2020,
  geometry = T
)
# quickly map that
ggplot() +
  geom_sf(data = rich_income, aes(fill = estimate)) +
  scale_fill_gradient(low = "white", high = "darkgreen")

# add title, labels, etc.
ggplot() +
  geom_sf(data = rich_income, aes(fill = estimate)) +
  scale_fill_gradient(
    low = "white", high = "darkgreen",
    labels = scales::dollar_format(),   # formats legend as $X,XXX
    name = "Median Household\nIncome",  # \n adds a line break in the title
    na.value = "grey"               
  ) +
  labs(
    title    = "Median Household Income by Census Tract",
    subtitle = "Greater Richmond Region — 2016-2020 ACS 5-Year Estimates",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text     = element_text(size = 7) 
  )
ggsave("richmond_tracts_income.png")

# look at income again but at the county level to calculate mean across the MSA
rich_counties_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "VA",
  county = c(
    "Goochland", "Powhatan", "Chesterfield", 
    "Henrico", "Charles City", "New Kent",
    "Hanover", "Richmond city", "Amelia",
    "Dinwiddie", "King and Queen", "King William", "Prince George",
    "Sussex", "Petersburg city", "Hopewell city", "Colonial Heights city"),
  year = 2020,
  geometry = T
)
# calculate mean
mean(rich_counties_income$estimate, na.rm = TRUE)

# -------------------------------------------------------------------------- #
# making bar graph of the racial breakdown of the MSA
richmond_msa_race <- get_acs(
  geography = "cbsa",
  variables = c(
    total             = "B02001_001",
    white             = "B02001_002",
    black             = "B02001_003",
    native            = "B02001_004",
    asian             = "B02001_005",
    pacific_islander  = "B02001_006",
    other             = "B02001_007",
    two_or_more       = "B02001_008"
  ),
  year = 2020
) |>
  filter(str_detect(NAME, "Richmond, VA"))

# calculate percentages and exclude total
richmond_msa_race_pct <- richmond_msa_race |>
  filter(variable != "total") |>
  mutate(
    total = richmond_msa_race$estimate[richmond_msa_race$variable == "total"],
    pct = estimate / total * 100
  )

# plot racial composition of the region in a bar graph
ggplot(richmond_msa_race_pct, aes(x = reorder(variable, pct), y = pct)) +
  geom_col(fill = "lightgreen") +
  scale_x_discrete(
    labels = c(
      "white"            = "White",
      "black"            = "Black",
      "native"           = "Native American",
      "asian"            = "Asian",
      "pacific_islander" = "Pacific Islander",
      "other"            = "Other",
      "two_or_more"      = "Two or More"
    )
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(
    title    = "Racial Composition of the Greater Richmond Region",
    subtitle = "2016-2020 ACS 5-Year Estimates",
    x        = "Race",
    y        = "Share of Population",
    caption  = "Source: U.S. Census Bureau, ACS 5-Year Estimates 2020"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text.x   = element_text(angle = 30, hjust = 1)
  )
ggsave("race_graph.png")

# ------------------------------------------------------------------------ #

# population pyramid - getting the data
pyramid_data <- get_estimates(
  geography = "cbsa",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  year = 2020
) |>
  filter(str_detect(NAME, "Richmond, VA"))

# make male values negative
rich_filtered <- pyramid_data |>
  filter(str_detect(AGEGROUP, "^Age")) |>
  mutate(value = ifelse(SEX == "Male", -value, value))

# make the pyramid 
ggplot(rich_filtered, aes(x = value, y = AGEGROUP, fill = SEX)) +
  geom_col()

# clean it up and add title, legend, etc.
rich_pyramid <- ggplot(rich_filtered,
                       aes(x = value,
                           y = AGEGROUP,
                           fill = SEX)) +
  geom_col(width = 0.95, alpha = 0.75) +
  theme_minimal(base_family = "Verdana",
                base_size = 12) +
  scale_x_continuous(
    labels = ~ number_format(scale = .001, suffix = "k")(abs(.x)),
    limits = 50000 * c(-1,1)
  ) +
  scale_y_discrete(labels = ~ str_remove_all(.x, "Age\\s|\\syears")) +
  scale_fill_manual(values = c("darkred", "navy")) +
  labs(x = "2020 Census Bureau population estimate",
       y = "Age group",
       title = "Population structure in the Greater Richmond Region",
       fill = "")
rich_pyramid
ggsave("population_pyramid.png")

# ---------------------------------------------------------------------- #
# not included in assignment, just out of curiosity
# percentage of each census tract with a HS diploma

rich_HS <- get_acs(
  geography = "tract",
  variables = "S1501_C02_015",
  state = "VA",
  county = c(
    "Goochland", "Powhatan", "Chesterfield", 
    "Henrico", "Charles City", "New Kent",
    "Hanover", "Richmond city", "Amelia",
    "Dinwiddie", "King and Queen", "King William", "Prince George",
    "Sussex", "Petersburg city", "Hopewell city", "Colonial Heights city"),
  year = 2020,
  geometry = T
)
# quickly map that
ggplot() +
  geom_sf(data = rich_HS, aes(fill = estimate)) +
  scale_fill_gradient(low = "white", high = "darkblue")
