# Work space setup -------------------------------------------
{ 
  rm(list = ls()) # clear console
  options(scipen = 999) # forces R to avoid exponential notation
  system.info <- Sys.info()
  
  # set location of dropbox folder 
  if(system.info[[8]] == "dylanturner"){
    db.loc <- "/Users/dylanturner/Dropbox" # desktop
  } else {
    db.loc <- "/home/dylan/Dropbox"
  }
  
  # set working directory within dropbox folder
  setwd(paste(db.loc,"/Research/Hurricane Damage Predictions", sep = ""))
}


# Load Libraries --------------------------------------------
    library(tidyverse)
    #library(drat)
    #addRepo("geanders")
    #install.packages("hurricaneexposuredata")
    library(hurricaneexposure) # for analyzing historical hurricane data
    library(hurricaneexposuredata) # historical hurricane data
    library(rgdal)
    library(geosphere)
    library(rgeos)
    library(rvest)
    library(stringr)
    library(quantmod)
    library(usmap) # contains  fips() for converting county name to fips code

# loading and cleaning historical hurricane data ------------------------------    
recalculate <- 0
if(recalculate == 1){
    #load historical hurricane exposure data
        data("hurr_tracks")

    # get date ranges of each hurricane
        hurricane_dates <- hurr_tracks[,c("storm_id","date")]
    
    # split hurricane date up into month, year, day
        hurricane_dates$year <- substr(hurricane_dates$date,1,4)
        hurricane_dates$month <- substr(hurricane_dates$date,5,6)
        hurricane_dates$day <- substr(hurricane_dates$date,7,8)
        
    # reconstruct and put into standard date format
        hurricane_dates$date_cleaned <- paste(hurricane_dates$year,hurricane_dates$month,hurricane_dates$day,sep = "-")
    
    # aggregate by storm name to get the start date 
        min_date <- aggregate(hurricane_dates, by = list(storm_id = hurricane_dates$storm_id), FUN = min)
        min_date <- min_date[,c("storm_id","date_cleaned")]
        colnames(min_date) <- c("storm_id","start_date")
    
    # aggregate by storm name to get the end date
        max_date <- aggregate(hurricane_dates, by = list(storm_id = hurricane_dates$storm_id), FUN = max)
        max_date <- max_date[,c("storm_id","date_cleaned")]
        colnames(max_date) <- c("storm_id","end_date")
        
    # inner join to get one df with storm name, start date, and end date
        hurricane_date_range <- inner_join(min_date, max_date, by = "storm_id")
    
    # add hurricane data range to hurr_tracks
         #hurr_tracks <- inner_join(hurr_tracks, hurricane_date_range, by = "storm_id")
    
    # compile a single data frame with all relevant hurricane data
      # for loop will likely take several minutes to run
      storms <- unique(hurricane_date_range$storm_id)
      for(k in 1:length(storms)){
      print(k)
      if(k == 1){
        rain <- filter_storm_data(storm = storms[k], include_rain = T , output_vars = c("fips","storm_dist", "tot_precip"), days_included = c(-2, -1, 0, 1,2))
        wind <- filter_wind_data(storm = storms[k], output_vars = c("fips","vmax_sust","vmax_gust","sust_dur"))
        #tracks <- hurr_tracks %>% filter(storm_id == storm[k])
        #names(tracks)[6] <- "wind_track"
        hurricanes <- inner_join(rain,wind,by = "fips")
        hurricanes$storm_id <- storms[k]
        #hurricanes <- inner_join(hurricanes, tracks, by = "storm_id")
      } else {
        rain <- filter_storm_data(storm = storms[k], include_rain = T , output_vars = c("fips","storm_dist", "tot_precip"), days_included = c(-2, -1, 0, 1,2))
        wind <- filter_wind_data(storm = storms[k], output_vars = c("fips","vmax_sust","vmax_gust","sust_dur"))
        #tracks <- hurr_tracks %>% filter(storm_id == storm[k])
        #names(tracks)[6] <- "wind_track"
        temp <- inner_join(rain,wind,by = "fips")
        temp$storm_id <- storms[k]
        
        hurricanes <- rbind.data.frame(hurricanes, temp)
      }
    }
    
    # add start and end date to hurricanes data frame
        hurricanes <- inner_join(hurricanes, hurricane_date_range, by = "storm_id")
        
    # check to see if any hurricane events spaned multiple years
        hurricanes$start_year <- substr(hurricanes$start_date,1,4)
        hurricanes$end_year <- substr(hurricanes$end_date,1,4)
        F %in% c(hurricanes$start_year == hurricanes$end_year)
        
    # add a year column
        hurricanes$year = as.numeric(hurricanes$start_year)
        
    # calculate max hurricane category
        storms <- unique(hurricanes$storm_id)
        hurricanes$max_hurr_cat <- NA
        for(k in 1:length(storms)){
          hurricanes %>% filter(storm_id == storms[k]) -> temp
          wind <- max(hurricanes$vmax_sust)
          hurr_cat <- NA
          hurr_cat <- replace(hurr_cat, wind <= 33, 0)
          hurr_cat <- replace(hurr_cat, wind > 33 & wind <= 42.46, 1)
          hurr_cat <- replace(hurr_cat, wind > 42.46 & wind <= 49.17, 2)
          hurr_cat <- replace(hurr_cat, wind > 49.17 & wind <= 58.11, 3)
          hurr_cat <- replace(hurr_cat, wind > 58.11 & wind <= 69.29, 4)
          hurr_cat <- replace(hurr_cat, wind > 69.29, 5)
          hurricanes$max_hurr_cat[which(hurricanes$storm_id == storms[k])] <- hurr_cat
        }    

    # save cleaned hurricanes data frame 
        write_csv(hurricanes,"./Data/hurricanes_cleaned.csv")
} else {
  hurricanes <- read_csv("./Data/hurricanes_cleaned.csv")
}




# loading  FEMA data -------------------------------

    # load historical nfip claims
      nfipClaims <- read_csv("./Data/FEMA/FimaNfipClaims.csv")
      #nfipClaims <- nfipClaims[sample(nrow(nfipClaims), 10000, replace = F),] # subsample to make data easier to work with during initial coding
      
    # load historical disaster declaration data
      disasterDeclarations <- read_csv("./Data/FEMA/DisasterDeclarationsSummaries.csv")
          # add fips code column
          disasterDeclarations$fips <- paste0(disasterDeclarations$fipsStateCode,disasterDeclarations$fipsCountyCode)
        
  
# clean nfip data -----------------------------------------------------------------
      
        # occupancy_type
        nfipClaims$sfh <- as.numeric(nfipClaims$occupancyType == 1)
      
        # condo  
        nfipClaims$condo <- as.numeric(nfipClaims$condominiumIndicator != "N")
        
        # fips code
        nfipClaims$fips <- nfipClaims$countyCode
        
        # date of loss
        nfipClaims$year <- as.numeric(substr(nfipClaims$yearofLoss,1,4))
        
        # keep only single family homes and non-condos
        nfipClaims %>% filter(condo == 0 & sfh == 1) %>% drop_na(year) -> nfipClaims      
      
        # combine hurricane exposure data with nfip claims 
        
            # Left join nfip claims and hurricanes data frames to identify which nfip claims 
            # can be attributed to one of the storms
            # left join nfip claims with the first exposure data frame 
            # (exposure data frame using first distance threshold specified)
               nfipClaims <- left_join(nfipClaims, hurricanes, by = c("fips","year"))
            
            # filter the data frame to remove observations where the date of loss
            # is not in the date range for the storm event. Also filtering based on
            # distance, by assuming that if the minimum distance between the 
            # hurricane track and city center is greater than 200km, then the claim
            # should not be attributed to that storm since.
                nfipClaims <- nfipClaims %>%
                  filter((dateOfLoss < end_date & dateOfLoss > start_date & storm_dist < 200) | 
                           is.na(end_date) )
      
        # data source
          nfipClaims$source <- "nfip"
        
        # indicator for flood insurance
          nfipClaims$flood_insurance <- 1
        
        # freeboard
          nfipClaims$freeboard_ft <- nfipClaims$elevationDifference

        # damage
          # flood damage to structure/contents
          nfipClaims$flood_structure_claim <- nfipClaims$amountPaidOnBuildingClaim
          nfipClaims$flood_contents_claim <- nfipClaims$amountPaidOnContentsClaim
          
          # coverage on structure/contents
          nfipClaims$flood_structure_coverage <- nfipClaims$totalBuildingInsuranceCoverage
          nfipClaims$flood_contents_coverage <- nfipClaims$totalContentsInsuranceCoverage
          
          # sum to get total flood damage
          nfipClaims$total_flood_damage <- rowSums(nfipClaims[,c("flood_structure_claim","flood_contents_claim")], na.rm = T)
          
          # the amount of damage may be censored from above (ex. true damage may be $100,000 but if the policy only had
          # $50,000 of coverage than the paid claim will not reflect actual damage)
          nfipClaims$total_flood_damage_censored <- as.numeric(nfipClaims$flood_structure_claim == nfipClaims$flood_structure_coverage |
                                                                 nfipClaims$flood_contents_claim == nfipClaims$flood_structure_coverage)
          
          # base flood elevataion
          nfipClaims$base_flood_elv_ft <- nfipClaims$baseFloodElevation
          # replace NA values with 0
          nfipClaims$base_flood_elv_ft <- replace(nfipClaims$base_flood_elv_ft, is.na(nfipClaims$base_flood_elv_ft),0)
          
          # basement
          nfipClaims$basement <- as.numeric(nfipClaims$basementEnclosureCrawlspace %in% c(1,2))
          nfipClaims$crawlspace <- as.numeric(nfipClaims$basementEnclosureCrawlspace %in% c(3,4))
          
          # elevated building
          nfipClaims$elevated_building <- nfipClaims$elevatedBuildingIndicator
          
          # assumed_home_valu 
          nfipClaims$assumed_home_value <- nfipClaims$totalBuildingInsuranceCoverage
          nfipClaims$assumed_home_value_censored <- as.numeric(nfipClaims$totalBuildingInsuranceCoverage == 250000)
          
          
          # flood zone 
          unique(nfipClaims$floodZone)
          nfipClaims$a_zone <- as.numeric( grepl("A",nfipClaims$floodZone))
          nfipClaims$v_zone <- as.numeric(grepl("V",nfipClaims$floodZone))
          nfipClaims$x_zone <- as.numeric(grepl("X",nfipClaims$floodZone))
          nfipClaims$sfha <- nfipClaims$a_zone + nfipClaims$v_zone
          
          # number of floors 
          nfipClaims$floors <- nfipClaims$numberOfFloorsInTheInsuredBuilding
          
          # FIRM
          nfipClaims$post_firm = nfipClaims$postFIRMConstructionIndicator
          
          # primary residence
          nfipClaims$primary_res = nfipClaims$primaryResidence
          
          # add hurricane categories
          nfipClaims$hurr_cat <- NA
            nfipClaims$hurr_cat <- replace(nfipClaims$hurr_cat, nfipClaims$vmax_sust <= 33, 0)
            nfipClaims$hurr_cat <- replace(nfipClaims$hurr_cat, nfipClaims$vmax_sust > 33 & nfipClaims$vmax_sust <= 42.46, 1)
            nfipClaims$hurr_cat <- replace(nfipClaims$hurr_cat, nfipClaims$vmax_sust > 42.46 & nfipClaims$vmax_sust <= 49.17, 2)
            nfipClaims$hurr_cat <- replace(nfipClaims$hurr_cat, nfipClaims$vmax_sust > 49.17 & nfipClaims$vmax_sust <= 58.11, 3)
            nfipClaims$hurr_cat <- replace(nfipClaims$hurr_cat, nfipClaims$vmax_sust > 58.11 & nfipClaims$vmax_sust <= 69.29, 4)
            nfipClaims$hurr_cat <- replace(nfipClaims$hurr_cat, nfipClaims$vmax_sust > 69.29, 5)
         
          #Category 1 hurricanes have sustained winds of 33 to 42.46 m/s. These very dangerous winds will produce some damage.
          #Category 2 hurricanes have sustained winds of 42.46 to 49.17 m/s. These extremely dangerous winds will cause extensive damage.
          #Category 3 hurricanes have sustained winds of 49.17 to 58.11 m/s. Devastating damage will occur.
          #Category 4 hurricanes have sustained winds of 58.115 to 69.29 mph. Catastrophic damage will occur.
          #Category 5 hurricanes have sustained winds greater than 69.29 mph. Catastrophic damage will occur. 
        
         
            
        # subset into a data frame with common variables
        nfip <- nfipClaims[,c("source","state","fips","storm_dist",
                              "tot_precip","vmax_sust","vmax_gust",
                              "sust_dur","storm_id","start_date",
                              "end_date","flood_insurance","year",
                              "flood_structure_claim","flood_structure_coverage",
                              "flood_contents_claim","flood_contents_coverage",
                              "total_flood_damage","total_flood_damage_censored",
                              "condo","sfh","freeboard_ft","base_flood_elv_ft",
                              "basement","crawlspace","elevated_building",
                              "floodZone","a_zone","v_zone","x_zone",
                              "sfha","floors","post_firm", "primary_res",
                              "hurr_cat","max_hurr_cat","assumed_home_value",
                              "assumed_home_value_censored","latitude","longitude")]   
        
        
# add inflation index ---------------------------------------------------------    
        
        # get an inflation index to adjust nfip to 2020 dollars
        getSymbols("CPIAUCSL", src='FRED') #All Urban consumers CPI from FRED
        avg.cpi <- apply.yearly(CPIAUCSL, mean) # get CPI average by year
        base_year <- "2020" #using 2020 as the base year
        inflation_index <- data.frame(avg.cpi/as.numeric(avg.cpi[base_year])) 
        inflation_index$date <- as.Date(rownames(inflation_index))
        inflation_index$year <- as.numeric(substr(inflation_index$date,1,4))
        inflation_index <- inflation_index[,c("CPIAUCSL","year")]
        colnames(inflation_index) <- c(paste0("inflation_factor",base_year),"year")
        
        # add inflation index to nfip data 
        nfip <- left_join(nfip, inflation_index, by = "year")   
        
        
# Add in zillow home value data ---------------------------------------
          # load and clean zillow home value index
          zhvi <- read.csv("./Data/Zillow Data/zhvi_sfh.csv")
          for(j in 1996:2020){
            zhvi[,ncol(zhvi)+1] <- rowMeans(zhvi[,which(substr(colnames(zhvi),2,5) == j)], na.rm = T)
            colnames(zhvi)[ncol(zhvi)] <- paste0("avg_county_home_value_",j)
          } 
          
          # create fips code column
          zhvi$state_fips <- as.character(zhvi$StateCodeFIPS)
          zhvi$state_fips[which(nchar(zhvi$state_fips) == 1)] <- paste0("0", zhvi$state_fips[which(nchar(zhvi$state_fips) == 1)])
          zhvi$county_fips <- as.character(zhvi$MunicipalCodeFIPS)
          zhvi$county_fips[which(nchar(zhvi$county_fips) == 2)] <- paste0("0", zhvi$county_fips[which(nchar(zhvi$county_fips) == 2)])
          zhvi$county_fips[which(nchar(zhvi$county_fips) == 1)] <- paste0("00", zhvi$county_fips[which(nchar(zhvi$county_fips) == 1)])
          zhvi$fips <- paste0(zhvi$state_fips, zhvi$county_fips)  
          
          # keep only needed columns
          zhvi %>% select(fips, which(substr(colnames(zhvi),1,22) == "avg_county_home_value_")) -> zhvi

          # convert NaN to NA
          zhvi <- data.frame(sapply(zhvi, function(x) ifelse(is.nan(x), NA, x)))
          
          # convert factors to numeric
              for(k in 2:ncol(zhvi)){
                zhvi[,k] <- as.numeric(as.character(zhvi[,k]))
              }
          
          # add columns for years before the zhvi starts
          for( k in 1950:1995){
            zhvi[,ncol(zhvi)+1] <- NA
            colnames(zhvi)[ncol(zhvi)] <- paste0("avg_county_home_value_",k)
          }
          zhvi <- zhvi[,order(colnames(zhvi))]

    
          # impute missing average county home values by imputing from nearest non-missing year using inflation index
              # this is note a very clean solution (suggestions on how to eliminate this loop are welcome)
              for(k in 1:nrow(zhvi)){
                  print(k)
                  temp <- zhvi[k,]
                  if(T %in% is.na(temp)){
                    for(j in 1:length(temp)){
                      if(is.na(temp[,j])){
                        non_missing <- temp[,which(is.na(temp)==F & colnames(temp) != "fips")]
                        imputation_point <- non_missing[1]
                        imputation_year <- as.numeric(gsub("avg_county_home_value_","",colnames(non_missing)[1]))
                        imputation_inflation_value <- inflation_index$inflation_factor2020[which(inflation_index$year == imputation_year)]
                        missing_year <- as.numeric(gsub("avg_county_home_value_","",colnames(temp)[j]))
                        missing_inflation_value <- inflation_index$inflation_factor2020[which(inflation_index$year == missing_year)]
                        pct_change <- (imputation_inflation_value - missing_inflation_value) / missing_inflation_value
                        zhvi[k,j] <- imputation_point / (1+pct_change)
                      }
                    }
                  }
              }
          
          # convert from wide to long format
          zhvi <- gather(zhvi, condition, measurement, avg_county_home_value_1950:avg_county_home_value_2020)
          colnames(zhvi) <- c("fips","year","avg_county_home_value")
          zhvi$year <- as.numeric(substr(zhvi$year,nchar(zhvi$year)-3,nchar(zhvi$year)))
          
          
          # join zillow average county home values 
              nfip <-  left_join(nfip, zhvi, by = c("fips","year"))
          
          # redefine damages as a percentage of home structure value (using county average as home value)
            nfip$flood_structure_claim_posv <- nfip$flood_structure_claim / nfip$avg_county_home_value
        
# Create indicators for if observation if from a coastal state and coastal county --------------------------
          
    ## Coastal State --------------------------------------------------------------      
          # scrape table from wikipedia of coastal states 
          url <- "https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_coastline"
          coastal_states <- url %>%
            html() %>%
            html_nodes(xpath='/html/body/div[3]/div[3]/div[5]/div[1]/table') %>%
            html_table(fill = T)
          coastal_states <- coastal_states[[1]]
          
          states_on_coast <- data_frame(coastal_states[-1,1])
          colnames(states_on_coast) <- "state"
          
          # scrape another table of state abbreviations 
          url <- "https://en.wikipedia.org/wiki/List_of_U.S._state_and_territory_abbreviations"
          state_abb <- url %>%
            html() %>%
            html_nodes(xpath='/html/body/div[3]/div[3]/div[5]/div[1]/table[1]') %>%
            html_table(fill = T)
          state_abb <- state_abb[[1]]
          state_abb <- state_abb[-c(1:12),c(1,4)]
          colnames(state_abb) <- c("state","abb")
          
          # join state abbreviations to states on the coast
          states_on_coast <- left_join(states_on_coast,state_abb, by = "state")
          
          # add an indicator in nfip nfip for if state is coastal
          nfip$coastal_state <- as.numeric(nfip$state %in% states_on_coast$abb)
    
    ## Coastal County ----------------------------------------------------------      
        # load csv file of coastal counties 
          coastal_counties <- read_csv("./Data/coastal_counties.csv")
          colnames(coastal_counties) <- c("fips","state_fips","county_fips",
                                          "county","state","coastal_region",
                                          "2016_pop")
          # select fips and coastal_region 
          coastal_counties %>% select("fips", "coastal_region") -> coastal_counties
          
          # join with NFIP nfip to add coastal_region
          nfip <- left_join(nfip, coastal_counties, by = c("fips"))
          
          # add indicator: = 1 if coastal county
          nfip$coastal_county <- as.numeric(nfip$fips %in% coastal_counties$fips)   
          
# create inflation adjusted damage values ---------------------------------
nfip$flood_structure_claim_adj2020 <- nfip$flood_structure_claim/nfip$inflation_factor2020       
nfip$flood_contents_claim_adj2020 <- nfip$flood_structure_claim/nfip$inflation_factor2020    
nfip$total_flood_damage_adj2020 <- nfip$flood_structure_claim_adj2020 + nfip$flood_contents_claim_adj2020

# save cleaned data as a csv file --------------------------------------------
          write_csv(nfip, "./Data/nfip_data_cleaned.csv")
          

          
          
          

  