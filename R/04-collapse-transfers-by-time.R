

# Making New Data Frames by Week, Quarter, and  Year

# Function to collapse the data by various units of time

collapse_by_time <- function(LESO, unit){
  LESO_by_unit <- LESO %>%
    group_by(.data[[sprintf("ship_date_%s", unit)]]) %>%
    dplyr::summarise(spend_per_unit=sum(total_value),
                     weapons_spend=sum(total_value[is_weapon]),
                     mil_spend=sum(total_value[is_militarized]),
                     protest_spend=sum(total_value[protest_use]),
                     ndemilA_spend=sum(total_value[not_demilA]),
                     milvehicle_spend = sum(total_value[is_mil_vehicle]),
                     n = n(),
                     n_milvehicle = sum(is_mil_vehicle),
                     n_weapons = sum(is_weapon),
                     n_mil = sum(is_militarized),
                     n_ndemilA = sum(not_demilA)) %>%
    dplyr::mutate(pct_weapons = round(n_weapons/n, digits = 1),
                  pct_mil = round(n_mil/n, digits = 1),
                  pct_ndemilA = round(n_ndemilA/n, digits = 1)) %>% 
    mutate(protest_portion=protest_spend/spend_per_unit,
           obama_reforms=ifelse(.data[[sprintf("ship_date_%s", unit)]] > "2015-05-18" & .data[[sprintf("ship_date_%s", unit)]] < "2017-08-28", TRUE, FALSE))
}

LESO_by_year <- collapse_by_time(LESO, "year")
LESO_by_quarter <- collapse_by_time(LESO, "quarter")
LESO_by_week <- collapse_by_time(LESO, "week")


# Function to collapse by time, highlighting specific types of equipment

collapse_by_time_controlled <- function(LESO, unit){
  LESO_by_unit <- LESO %>%
    group_by(.data[[sprintf("ship_date_%s", unit)]]) %>%
    dplyr::summarise(spend_per_unit=sum(total_value),
                     val_rifles=sum(total_value[is_rifle]),
                     val_controlledvehicle = sum(total_value[is_controlled_milvehicle]),
                     val_aircraft = sum(total_value[is_aircraft]),
                     n = n(),
                     n_rifles = sum(is_rifle),
                     n_controlledvehicle = sum(is_controlled_milvehicle),
                     n_aircraft = sum(is_aircraft))
}

LESO_controlled_quarter <- collapse_by_time_controlled(LESO, "quarter")
