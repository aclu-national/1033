#Run after 01-read-1033-data.R
#Check the README for explanation

####-----------Add a marker for NSNs that are banned by EO13688

#Read in banned NSNs list
EO_13688_banned_nsns <- read_csv(here::here("data/01-raw/EO 13688 banned nsns.csv"), skip = 1) %>% 
  select(item, type) %>% 
  dplyr::rename(banned_nsn=item,
                banned_type=type)

add_EO13688_variables <- function(df){
  df %<>% 
    left_join(EO_13688_banned_nsns, by=c("nsn"="banned_nsn")) %>% 
    mutate(is_banned_nsn = !is.na(banned_type)) %>% 
    mutate(is_controlled_milvehicle = !is_banned_nsn & is_mil_vehicle)
}

LESO %<>% add_EO13688_variables()
list_df_alldates_clean <- lapply(list_df_alldates_clean, add_EO13688_variables)

####------------Save cleaned and processed data files

#Save to interim data
write_rds(LESO, here::here("data/02-interim/LESO.rds"))

#Save the latest iteration to interim data to see what's currently in circulation
n_sheets <- length(list_df_alldates_clean)
write_rds(list_df_alldates_clean[[n_sheets]], here::here("data/02-interim/LESO_now.rds"))

#And save some other ones that are used in the analysis Rmd
write_rds(list_df_alldates_clean[[3]], here::here("data/02-interim/LESO_04072015.rds"))
write_rds(list_df_alldates_clean[[5]], here::here("data/02-interim/LESO_07142015.rds"))
write_rds(list_df_alldates_clean[[7]], here::here("data/02-interim/LESO_03202016.rds"))
write_rds(list_df_alldates_clean[[11]], here::here("data/02-interim/LESO_06302017.rds"))
write_rds(list_df_alldates_clean[[22]], here::here("data/02-interim/LESO_06302020.rds"))


