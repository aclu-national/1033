
clean_station_names <- function(df, name){
  require("dplyr")
  df <- df %>% 
    mutate(station_name_lea_clean=name %>%
             str_to_upper() %>% 
             str_replace_all(., " CO$| CO | CCO ", " COUNTY ") %>%
             str_replace_all(., " CSO", " COUNTY SHERIFF DEPARTMENT") %>%
             str_replace_all(., " CSD", " COUNTY SHERIFF DEPARTMENT") %>%
             str_replace_all(., " SO$", " SHERIFF DEPARTMENT") %>%
             str_replace_all(., "PARISH SO", "PARISH SHERIFF DEPARTMENT") %>%
             str_replace_all(., " DEPT ", " DEPARTMENT ") %>% 
             str_replace_all(., "^DEPT ", "DEPARTMENT ") %>% 
             str_replace_all(., " DEPT$", " DEPARTMENT") %>% 
             str_replace_all(., "DEPTMENT|DEPT.", "DEPARTMENT") %>% 
             str_replace_all(., "PD", "POLICE DEPARTMENT") %>% 
             str_replace_all(., "SHEFIFFS|SHERRIF|SHERIFF'S|SHERIFFS|SHERIF|SHERIFFF", "SHERIFF") %>% 
             str_replace_all(., "SHERIFFF", "SHERIFF") %>% 
             str_replace_all(., "MARSHALS", "MARSHAL") %>% 
             str_replace_all(., " DA |^DA | DA$|DIST ATTORNEY", " DISTRICT ATTORNEY ") %>% 
             str_replace_all(., " CTY$| CTY |^CTY ", " COUNTY ") %>% 
             str_replace_all(., " CTY\\)", " COUNTY\\) ") %>% 
             #' CTY seems to indicate county. Where there is a X city, it is always located in a coterminous county
             str_replace_all(., "OFFICE", "DEPARTMENT") %>% 
             str_replace_all(., "POLICE$", "POLICE DEPARTMENT") %>% 
             str_replace_all(., "SHERIFF$", "SHERIFF DEPARTMENT") %>% 
             str_replace_all(., "DPS", "DEPARTMENT OF PUBLIC SAFETY") %>% 
             str_replace_all(., "TWP", "TOWNSHIP") %>% 
             str_squish()
    )
}


state_abbrev <- function(c) {
  c %>% 
    str_to_title() %>% 
    str_squish() %>% 
    dplyr::recode("Alaska" = "AK",
                  "Alabama" = "AL",
                  "Arkansas" = "AR",
                  "Arizona" = "AZ",
                  "California" = "CA",
                  "Colorado" = "CO",
                  "Connecticut" = "CT",
                  "District of Columbia" = "DC",
                  "District Of Columbia" = "DC",
                  "Delaware" = "DE",
                  "Florida" = "FL",
                  "Georgia" = "GA",
                  "Hawaii" = "HI",
                  "Iowa" = "IA",
                  "Idaho" = "ID",
                  "Illinois" = "IL",
                  "Indiana" = "IN",
                  "Kansas" = "KS",
                  "Kentucky" = "KY",
                  "Louisiana" = "LA",
                  "Massachusetts" = "MA",
                  "Maryland" = "MD",
                  "Maine" = "ME",
                  "Michigan" = "MI",
                  "Minnesota" = "MN",
                  "Missouri" = "MO",
                  "Mississippi" = "MS",
                  "Montana" = "MT",
                  "North Carolina" = "NC",
                  "North Dakota" = "ND",
                  "Nebraska" = "NE",
                  "New Hampshire" = "NH",
                  "New Jersey" = "NJ",
                  "New Mexico" = "NM",
                  "Nevada" = "NV",
                  "New York" = "NY",
                  "Ohio" = "OH",
                  "Oklahoma" = "OK",
                  "Oregon" = "OR",
                  "Pennsylvania" = "PA",
                  "Puerto Rico" = "PR",
                  "Rhode Island" = "RI",
                  "South Carolina" = "SC",
                  "South Dakota" = "SD",
                  "Tennessee" = "TN",
                  "Texas" = "TX",
                  "Utah" = "UT",
                  "Virginia" = "VA",
                  "Vermont" = "VT",
                  "Washington" = "WA",
                  "Wisconsin" = "WI",
                  "West Virginia" = "WV",
                  "Wyoming" = "WY",
                  "Virgin Islands" = "VI",
                  "Guam" = "GU",
                  "CNMI Islands" = "MP")
}


