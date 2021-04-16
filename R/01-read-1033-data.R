
library(readxl)
library(plyr)
library(tidyverse)
library(janitor)
library(tabulizer)
library(here)
library(lubridate)
library(knitr)
library(lazyeval)
library(magrittr)

#This script is for reading in, combining, and processing the quarterly updates of 1033 equipment currently in circulation ("inventory" files).

#Create a folder to store the files generated below
if(!dir.exists(here::here("data/02-interim"))) {
  dir.create(here::here("data/02-interim"))
}

#A function that cleans and standardizes the data and adds new useful variables
process_LESO_data <- function(LESO){

  LESO %<>%  
    mutate(total_value = acquisition_value * quantity,
           nsn_clean = str_replace_all(nsn, "-", ""),
           ship_date = as.Date(ship_date),
           ship_date_week = lubridate::floor_date(as.Date(ship_date), unit = "weeks"),
           ship_date_quarter = lubridate::floor_date(as.Date(ship_date), unit = "quarter"),
           ship_date_year = lubridate::floor_date(as.Date(ship_date), unit = "years")
           ) %>% 
    
  #Make a variable to capture policy change, but the start date of the Obama reforms varies by type of equipment
    mutate(policy_change_ban = case_when(
      ship_date <= "1996-09-30" ~ "pre1033",
      ship_date <= "2015-05-18" ~ "1033a",
      ship_date <= "2017-08-28" ~ "obama_reforms",
      TRUE ~ "1033b"),
      policy_change_controlled = case_when(
      ship_date <= "1996-09-30" ~ "pre1033",
      ship_date <= "2015-10-01" ~ "1033a",
      ship_date <= "2017-08-28" ~ "obama_reforms",
      TRUE ~ "1033b")
    ) %>%
  
  #An indicator variable capturing the reform period for each type of equipment
    mutate(obama_reform_ban = ifelse(ship_date > "2015-05-18" & ship_date < "2017-08-28", TRUE, FALSE),
           obama_reform_controlled = ifelse(ship_date > "2015-10-01" & ship_date < "2017-08-28", TRUE, FALSE)) %>% 
  
  #' ### Categorizing items, especially weapons
  #' * The first two numerals of the NSN (national stock number) correspond to a group; the first four correspond to a sub-group/"class"
  #' * The table below, from a federal supply classification catalogue, matches group numbers to group labels
  #' * Four of the groups can be classified as weapons they are captured in an is_weapons indicator variable
    mutate(group = as.numeric(substr(nsn, 1, 2)),
           subgroup = as.numeric(substr(nsn_clean, 1, 4)))
  
  NSN_Group_Index <- read_excel(here::here("data/01-raw/NSN Group Index.xlsx"))
  LESO <- left_join(LESO, NSN_Group_Index, by = c("group" = "Group"))
  
  #Categories for weapons and militarized vehicles, using the groups and subgroups, plus some key words
  LESO %<>% 
      mutate(Label = str_squish(Label)) %>% 
      mutate(is_weapon = ifelse(str_detect(Label, "10 Weapons|13 Ammunition and Explosives|11  Nuclear Ordnance|14  Guided Missiles"), TRUE, FALSE),
             is_vehicle = group==15 | group==16 | group==19 | group==23 | group==38,
             is_mil_vehicle = ifelse(subgroup == 2355 | subgroup == 2350 | (str_detect(item_name, "ARMOR|COMBAT|ASSAULT") & (group == 23|group == 38)), TRUE, FALSE))
  #The logic here^ is "either one of these explicitly militarized vehicle subgroups" OR "a group indicating vehicle plus a keyword indicating militarized"
  #The is_mil_vehicle indicator only includes ground vehicles 
      
  #'Look for militarized equipment and bring together a "militarized" indicator variable
  #'Also make an indicator for equipment often called out as being used against protestors
  LESO %<>% 
    mutate(is_militarized = ifelse(str_detect(item_name, "ARMOR|WEAPON|RIFLE|BALLISTIC|AMMUNITION|ASSAULT|COMBAT")|
                                   str_detect(item_name, "BARREL,GRENADE LAUNCHER|BAYONET|LAUNCHER,GRENADE|GRENADE")|
                                   str_detect(item_name,"DESC=RIOT SHIELD|RIOT CONTROL SHIELD|RAM,BATTERING|ARMORED|AIRCRAFT, FIXED WING| ONLY COMPLETE COMBAT/ASSAULT/TACTICAL WHEELED VEHICLES|FRAME,TRAILER,120 MILLIMETER MORTAR|CARRIER,120 MILLIMETER MORTAR")|
                                   subgroup %in% c(2350, 2355, 1345, 1370, 1376)|
                                   group == 69|
                                   is_weapon|
                                   is_mil_vehicle, TRUE, FALSE),
           not_demilA = !demil_code=="A", # Demilitarization code A indicates "uncontrolled" per the DoD, all other demil codes are "controlled
           is_rifle = str_detect(item_name, "RIFLE,5.56 MILLIMETER|RIFLE,7.62 MILLIMETER|GUN,RIFLE,5.56MM"),
           is_aircraft = subgroup==1510 | subgroup==1520 | subgroup==1550, #rotary wing, fixed wing, unmanned
           is_riot_shield = ifelse(str_detect(item_name, "BODY SHIELD|SHIELD,PROTECTIVE|RIOT CONTROL SHIELD|FACESHIELD,MILITARY,RIOT CONTROL|FACE SHIELD|FACESHIELD,INDUSTRIAL|FACE SHIELD,GENERAL|FACESHIELD,PROTECTIVE|SHIELD,BALLISTIC|DESC=SHIELD BODY|FACE SHIELD,TACTICAL SIMULATION HELMET|NON-BALLISTIC BODY SHIELD|SHIELD WITH NOSEPIECE"), TRUE, FALSE)) %>% 
    mutate(protest_use = ifelse(is_mil_vehicle|
                                is_riot_shield|
                                str_detect(item_name, "GAS HEADPIECE|MASK,GAS|GAS MASK|PEPPER SPRAY|DESC=CPCV BATONS RUBBER|NON-LETHAL|NONLETHAL|LRAD|LONG RANGE ACOUSTIC DEVICE|BATON|RIOT|SHOTGUN,12 GAGE|CONVERSION KIT FOR 12 GA  SHO|TRAINING SUIT|BODY ARMOR"), TRUE, FALSE),
           protest_use_novehicle = ifelse(is_riot_shield|
                                          str_detect(item_name, "GAS HEADPIECE|MASK,GAS|GAS MASK|PEPPER SPRAY|DESC=CPCV BATONS RUBBER|NON-LETHAL|NONLETHAL|LRAD|LONG RANGE ACOUSTIC DEVICE|BATON|RIOT|SHOTGUN,12 GAGE|CONVERSION KIT FOR 12 GA  SHO|TRAINING SUIT|BODY ARMOR"), TRUE, FALSE)
    )

  #'Clean and standardize LEA station names
  source(here::here("R/00-utils-data-cleaning.R"))
  LESO <- clean_station_names(LESO, LESO$station_name_lea)
  
  #'Pull a location name from the clean LEA name and mark counties.
  LESO %<>% 
    mutate(location_name = station_name_lea_clean %>%
           str_replace_all(., "DEPARTMENT|OF PUBLIC SAFETY|OF CORRECTIONS|POLICE|OFFICE|SHERIFF|DIVISON OF|MARSHAL|CONSTABLE|DISTRICT ATTORNEY|LAW ENFORCEMENT|NARCOTICS|AUTHORITY|PUBLIC SAFETY|MUNICIPAL|CORRECTIONAL FAC|AIRPORT AUTHORITY|(LEA)|CA STATE PRISON|METROPARKS DIV OF LAW ENF|PROSECUTORS|INTL AIRPORT|DPS|AUXILIARY (EMA)|INTL AIRPORT|STATE PRISON| LEA$|CORRECTIONAL CENTER", "") %>% 
           str_squish(),
           county_loc = str_detect(station_name_lea_clean, "COUNTY|PARISH"))
  #' Didn't remove HI_ED tag since college names aren't usually cities anyways.
  #' In this data, all sheriffs serve counties 
  
  #' Indicator variables for type of LEA agency/station
  LESO %<>% 
    mutate(
      is_sheriff = str_detect(station_name_lea_clean, "SHERIFF"),
      is_police = str_detect(station_name_lea_clean, "POLICE|POLICIA"),
      is_constable = str_detect(station_name_lea_clean, "CONSTABLE"),
      is_k_12 = str_detect(station_name_lea_clean, "K_12|K-12"),
      is_higher_ed = str_detect(station_name_lea_clean, "HI_ED"),
      is_marshal = str_detect(station_name_lea_clean, "MARSHAL"),
      is_carceral = str_detect(station_name_lea_clean, "CORRECTIONS|CORRECTION|STATE PENITENTIARY|STATE PRISON|REGIONAL JAIL|DETENTION|PAROLE AND PROBATION|PROBATION|DEPARTMENT OF CORR|FEDERAL BUREAU OF PRISONS|BUR OF PRISONS|PRISON"),
      is_prosecutor = str_detect(station_name_lea_clean, "DISTRICT ATTORNEY|PROSECUTOR|ATTORNEY GEN|STATE ATTORNEY|ATTY GEN"),
      is_parks_lands_wildlife = str_detect(station_name_lea_clean, "PARK|FISH AND BOAT|MARINE RESOURCES|FISH & WILDLIFE|NATURAL & ENV|WILDLIFE|LIVESTOCK|FISHERIES|FORESTRY|WATERWAY|HARBORMASTER|CONSERVATION|AGRICULTURE|NATURAL RESOURCES|RIVER DAM AUTHORITY|MARINE PATROL|GAME AND FISH|FOREST SERVICE|FISH AND GAME|GAME FISH|DEPT AGRIC|DOI BUR OF LAND|LIVE STOCK|DEPARTMENT OF CONSERV|ANIMAL CONTROL|FOREST PRESERVE|FOREST PROTECTION|DEPARTMENT AGRIC, FS|NPS|NATL FOREST|NATURAL ENV RESOURCES|DNR"),
      is_public_safety = str_detect(station_name_lea_clean, "PUBLIC SAFETY|DPS|PUBLIC SAFEY|DEPARTMENT OF SAFETY"),
      is_vice = str_detect(station_name_lea_clean, "DTF|ALCOHOL AND TOBACCO|NARCOTICS|DRUG ENFORCEMENT|ORG CRIME DRUG|TOBACCO CONTROL|ALCOHOL ENF|GAMBLING CONTROL|ATF|DEA|ALCOHOLIC BEV CONTROL|NTF|NARC SWAT|NARC ENF|NARCOTIC|GAMING AGENCY|DRUG TASK|ALCOHOLIC BEVERAGE CONTROL|DIV OF ALCOHOL"),
      is_patrol = str_detect(station_name_lea_clean, "HIGHWAY PATROL|HWY PATROL|PATROL|STATE TROOPER"),
      is_fire = str_detect(station_name_lea_clean, "ARSON|FIRE "),
      is_fbi = str_detect(station_name_lea_clean, "FBI|FEDERAL BUREAU OF INVESTIGATION"),
      is_immigration = str_detect(station_name_lea_clean, "CBP| ICE | ICE$|^ICE |BORDER PATROL|IMMIGRATION AND CUSTOMS|CUSTOMS AND BORDER| INS |^INS | INS$"),
      is_transportation = str_detect(station_name_lea_clean, "AIRPORT|TRANSPORTATION|PORT AUTH|MOTOR VEHICLE ENFORCEMENT"),
      is_doj = str_detect(station_name_lea_clean, "DOJ"),
      is_dhs = str_detect(station_name_lea_clean, "DHS|HOMELAND")
    )
  
  LESO %<>% 
    mutate(LEA_type = case_when(
      is_higher_ed ~ "higher ed",
      is_k_12 ~ "k-12",
      is_parks_lands_wildlife ~ "parks lands wildlife",
      is_transportation ~ "transportation",
      is_fire ~ "fire",
      is_vice ~ "vice",
      is_carceral ~ "carceral",
      is_prosecutor ~ "prosecutor",
      is_public_safety ~ "public safety",
      is_patrol ~ "patrol",
      is_constable ~ "constable",
      is_marshal ~ "marshal",
      is_sheriff ~ "sheriff",
      is_police ~ "police",
      is_fbi ~ "fbi",
      is_immigration ~ "cbp/ice",
      is_doj ~ "doj",
      is_dhs ~ "dhs")
    )
  #The categories aren't perfectly mutually exclusive so the order above matters
  
  #' Many station names are repeated across states, so I make a new variable to uniquely identify each station, by combining the station name and state variables.
  LESO %<>% unite("station_id", c(state, station_name_lea_clean), na.rm = TRUE, remove = FALSE)
  
}


#READING IN AND COMBINING INVENTORY DATA FILES
#The Marshall Project has been downloading all available data since ~2014, so this code is to read in and combine those datasets
#Their folder is replicated in our repository as "MP Repo-agency level"

#Capture a list of the paths of most inventory files, which are named like this:
list_paths_inventory_files <- Sys.glob(here::here("data/01-raw/MP Repo-agency level/*/DISP_AllStatesAndTerritories_*.xlsx"))

#Function to read in all sheets from each inventory file
combine_sheets_filepath <- function(filepath){
  sheet_names <- c(excel_sheets(path = filepath))
  sheets_list <- lapply(sheet_names, function(x) read_excel(path = filepath, sheet = x))
  full_file <- clean_names(bind_rows(sheets_list))
}

#Read in inventory files
list_df_inventory <- lapply(list_paths_inventory_files, combine_sheets_filepath)

#A few of the older inventory files (2016-03-20 and earlier) are named differently and split into two: 
list_paths_AL_LA <- Sys.glob(here::here("data/01-raw/MP Repo-agency level/*/Alaska_Louisiana.xls"))
list_paths_MA_WY <- Sys.glob(here::here("data/01-raw/MP Repo-agency level/*/Massachussetts_Wyoming_Territories.xls"))
list_paths_AL_WY <- c(list_paths_AL_LA, list_paths_MA_WY)

#Read in all sheets from these files
list_df_AL_WY <- lapply(list_paths_AL_WY, combine_sheets_filepath)

#list_df_AL_WY contains the 5 AL:LA files, in date order ([1:5]), and the 5 MA:WY files, in date order ([6:10]),
#so [1] needs to be combined with [6] and so on to generate a complete inventory file for each release
list_df_AL_WY_joined <- c()
for (n in 1:5){
  list_df_AL_WY_joined[[n]] <- bind_rows(list_df_AL_WY[[n]], list_df_AL_WY[[n+5]])
}

#The oldest file of all is again named differently and split in two. Read in and combine those files
list_paths_first_agency_release <- Sys.glob(here::here("data/01-raw/MP Repo-agency level/2014-11-21/*"))
list_df_first_agency_release <- lapply(list_paths_first_agency_release, combine_sheets_filepath)
df_first_agency_release <- bind_rows(list_df_first_agency_release) %>% 
  dplyr::rename(station_name_lea = agency_name)

#Put the lists of dataframes for all inventory files together in one list
list_df_alldates <- c(list(df_first_agency_release), list_df_AL_WY_joined, list_df_inventory)

#Apply the data processing function to all inventory files
list_df_alldates_clean <- lapply(list_df_alldates, process_LESO_data)



#JOIN INVENTORY FILES TO MAKE A LIST OF OVERALL TRANSFERS

#fixing a couple of (likely data entry) errors that mess up this merge
list_df_alldates_clean[[3]]$ship_date[list_df_alldates_clean[[3]]$ship_date == "2015-12-08"] <- "2014-12-08"
list_df_alldates_clean[[4]]$ship_date[list_df_alldates_clean[[4]]$ship_date == "2015-12-08"] <- "2014-12-08"
list_df_alldates_clean[[20]]$ship_date[list_df_alldates_clean[[20]]$ship_date == "2020-12-30"] <- "2019-12-30"
list_df_alldates_clean[[14]]$ship_date[list_df_alldates_clean[[14]]$ship_date == "2018-11-05"] <- "2018-06-30"

#Atart with the earliest dataset and then just add all transfers with new ship dates from each subsequent dataset 
LESO_bind <- list_df_alldates_clean[[1]]
l <- length(list_df_alldates_clean)
for (n in 2:l){ 
  filtered <- list_df_alldates_clean[[n]] %>% filter(ship_date > max(LESO_bind$ship_date, na.rm = T))
  LESO_bind <- LESO_bind %>%  bind_rows(filtered)
  print(nrow(LESO_bind))
}
sum(LESO_bind$total_value)

#Add in federal agency files, which are separated out from the main inventory starting with 2018-09-30
#These files contain only net new shipments each quarter (in contrast to an inventory snapshot) so they can be simply appended
list_paths_fedagency_files <- here::here(Sys.glob("data/01-raw/MP Repo-agency level/*/*Federal*"))
list_df_fedagency <- lapply(list_paths_fedagency_files, read_excel)

#Combine all, clean names, label them as "federal", and apply the data processing function from above
fedagency_shipments <- bind_rows(list_df_fedagency) %>% 
  clean_names() %>% 
  mutate(station_type = "federal") %>% 
  process_LESO_data()

#combine federal agency dataset and main LESO inventory merge
LESO_bind_fed <- bind_rows(LESO_bind, fedagency_shipments)

#there's one more one-off file: ammunition transfers in 2017-2018
ammo_transfers_filepath <- here::here("data/01-raw/MP Repo-agency level/2018-12-31/FY17-18 AMMO Shipped-State-Fed-Cancellations 10022018.xlsx") 

ammo_sheets_list <- lapply(excel_sheets(ammo_transfers_filepath), function(x) read_excel(path = ammo_transfers_filepath, sheet = x, col_types = "text"))

ammo_transfers <- ammo_sheets_list[[1]] %>% 
  bind_rows(ammo_sheets_list[[2]]) %>% 
  clean_names() %>% 
  #Multiple dates are listed under "received" for some rows. the range was usually small so just take the first
  mutate(date_received_list = strsplit(date_received, ",")) %>% 
  mutate(date_received_raw = lapply(date_received_list, first) %>% 
           as.character()) %>%
  #The fact that some dates came as lists made read_excel convert some of them to numeric and leave others in d/m/y format
  mutate(date_received = ifelse(str_detect(date_received_raw, "\\d{5}"), as.Date(as.numeric(date_received_raw), origin = "1900-01-01"), as.Date(date_received_raw, "%m/%d/%Y")) %>% 
           as.Date(origin = "1970-01-01")) 
  #The ifelse produces a numeric value, so convert to date again

#Quirks with this ammunition file: 1) acquisition value is actually already total, not per unit; 2) some rows are named differently
ammo_transfers %<>% 
  dplyr::rename("station_name_lea" = "agency_name") %>% 
  mutate(ship_date = date_received,
         acquisition_value = as.numeric(acquisition_value),
         true_quantity = quantity,
         quantity = 1,
         item_name = paste(item_name, "AMMUNITION", sep = " ")) %>% 
  select(-c(date_received, date_received_list, date_received_raw))

ammo_transfers %<>% process_LESO_data()

LESO <- bind_rows(LESO_bind_fed, ammo_transfers)

#Some datasets include a station_type column, marking agencies as either state or federal; for others I added the label myself. Here I'm labeling federal agencies that aren't already labeled, because they come from older datasets that didn't distinguish
LESO$station_type[str_detect(LESO$station_name_lea, "(\\b|[._-])(US|FBI|DOI|DOJ|DHS|CBP|ICE|USDVA|USPIS|DEA)(\\b|[._-])")] <- "federal"
#this might not be comprehensive but works pretty well to capture most federal agencies, and it doesn't seem tofalsely identify any state/local agencies as federal

#double check that these federal agency rows aren't repeats // that federal agency data has indeed dropped out of the main inventory files
#elements 16:22 in the list_df_alldates_clean list are the inventory files with dates overlapping the federal agency shipment udpates
for (n in 16:22){
  list_df_alldates_clean[[n]] %>% filter(str_detect(list_df_alldates_clean[[n]]$station_name_lea, "(\\b|[._-])(US|FBI|DOI|DOJ|DHS|CBP|ICE|USDVA|USPIS|DEA)(\\b|[._-])")) %>% nrow() %>% print()
}
#the only fed agency rows that appear are these same 10 "MT DOJ" rows, which do not appear in the fedagency shipments files
fedagency_shipments %>% filter(str_detect(station_name_lea, "MT DOJ GAMBLING")) %>% nrow()


#After running this file, run: "R/02-identify-equipment-affected-by-EO13688.R"
#This file adds variables identifying equipment affected by Obama's EO13688 and also saves copies of the data for later use


#Uncomment below to make a combined inventory file for ~what's in circulation over time~ analysis

# update_date_list <- Sys.glob(here::here("data/01-raw/MP Repo-agency level/*")) %>% 
#   map(., ~ str_replace_all(., "/Users/clawrence/mil-police/data/01-raw/MP Repo-agency level/", ""))
# 
# stopifnot(length(update_date_list) == length(list_df_alldates_clean))
# 
# for (n in 1:length(list_df_alldates_clean)){
#   list_df_alldates_clean[[n]] %<>% mutate(id = update_date_list[[n]])
# }
# 
# LESO_superfile <- bind_rows(list_df_alldates_clean) %>% 
#   filter(!is.na(ship_date))
# 
# LESO_superfile %<>% 
#   left_join(EO_13688_banned_nsns, by=c("nsn"="banned_nsn")) %>% 
#   mutate(is_banned_nsn = !is.na(banned_type)) %>% 
#   mutate(is_controlled_milvehicle = !is_banned_nsn & is_mil_vehicle)
# 
# write_csv(LESO_superfile, here::here("data/02-interim/LESO_superfile.csv"))

