library(tidyverse)
library(tidycensus)
library(janitor)

source(here::here("R/00-utils-data-cleaning.R"))

#Create a by-station data frame
LESO_by_station <- LESO %>% 
  dplyr::group_by(station_name_lea_clean, state) %>% 
  dplyr::summarise(
    station_id=first(station_id),
    is_county=first(county_loc),
    station_type=first(station_type),
    lea_type = first(LEA_type),
    n_transfers=n(),
    n_mil_vehicle = sum(is_mil_vehicle),
    total_value_agg=sum(total_value),
    weapons_value=sum(total_value[is_weapon]),
    mil_value=sum(total_value[is_militarized]),
    protest_value=sum(total_value[protest_use]),
    transfer_date_first=min(ship_date), 
    transfer_date_last=max(ship_date),
    loc_name=first(location_name)) %>% 
  mutate(weapons_portion=weapons_value/total_value_agg,
         participation_length=as.numeric(transfer_date_last - transfer_date_first)) %>% 
  mutate(participation_length2=participation_length %/% as.numeric(days(1)))

#Read in census data
census_api_key("3e3a120df9ef71ef6e8c6818f0de9203608393a3", install=TRUE, overwrite=TRUE)

us <- unique(fips_codes$state)[1:51]

census_data <- map_df(us, function(x) {
  get_acs(geography = "county", 
          variables = c(total_pop="B01003_001",
                        white_pop="B02001_002", 
                        median_income="B06011_001"), 
          state = x, 
          output = "wide")
})

census_data <- census_data %>% 
  dplyr::rename("total_pop"="total_popE",
                "white_pop"="white_popE",
                "median_income"="median_incomeE",
                "county_state"="NAME")

#Create a variable for white percent of population, make the county_state variable uppercase
census_data <- census_data %>%
  mutate(white_pct=white_pop/total_pop,
         county_state=str_to_upper(county_state))

#Split the county_state variable into county and state so that it can be merged with the crosswalk data
split_location <- stringr::str_split(census_data$county_state, ",", simplify=TRUE) %>% 
  data.frame() %>% 
  dplyr::rename(
    county = X1,
    state = X2) %>% 
  mutate(county=str_squish(county),
         state=str_squish(state))

census_data <- clean_names(bind_cols(split_location, census_data))

census_data <- census_data %>% 
  mutate(state_abbrev=state_abbrev(census_data$state),
         county_nocounty=str_squish(str_replace(county, "COUNTY", "")))

#Read in FBI crosswalk data to match LEAs to the counties in which they are located
load(here::here("data/01-raw/35158-0001-Data.rda"))
FBI_crosswalk <- da35158.0001 %>% 
  clean_names() %>% 
  select(name, statename, countyname, agcytype, lg_name, address_name, address_str1, address_city, address_state, address_zip, u_tpop) 

FBI_crosswalk <- FBI_crosswalk %>% 
  mutate(clean_state=state_abbrev(statename),
         clean_county=str_squish(countyname),
         name=str_squish(name))

#Apply the same standardization rules to the FBI crosswalk data's station names as were applied to the LESO data, for a better match
source(here::here("R/00-utils-data-cleaning.R"))
FBI_crosswalk <- clean_station_names(FBI_crosswalk, FBI_crosswalk$name)

#Removing duplicate stations from the FBI crosswalk data
FBI_crosswalk <- FBI_crosswalk %>% 
  unite("station_id_cw", c(clean_state, station_name_lea_clean), na.rm = TRUE, remove = FALSE) %>% 
  mutate(duplicate=duplicated(station_id_cw))
FBI_crosswalk <- subset(FBI_crosswalk, duplicate==FALSE, select = -duplicate)


#Limit LESO_by_station dataframe to stations that serve counties
LESO_by_station_county <- subset(LESO_by_station, is_county)

#Merge with LESO_by_station_county
LESO_by_station_cw <- inner_join(LESO_by_station_county, FBI_crosswalk, by=c("state"="clean_state", "station_name_lea_clean"="station_name_lea_clean"))

#Merge with ACS data
LESO_by_station_census_county <- inner_join(LESO_by_station_cw, census_data, by=c("clean_county"="county_nocounty", "state"="state_abbrev"))

