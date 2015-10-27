if (!exists("stormdata")) {
        if (!file.exists("repdata_data_StormData.csv.bz2")) {
                library(downloader)
                download("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "repdata_data_StormData.csv.bz2")
        }

        library(readr)
        # Look at the columns in the dataset, together with documentation
        read_csv("repdata_data_StormData.csv.bz2", n_max = 1, col_names = FALSE)
        cols <- paste(rep("c", 37), collapse = "")

        stormdata <- read_csv("repdata_data_StormData.csv.bz2",
                              col_types = cols,
                              col_names = TRUE)

        library(lubridate)
        library(stringr)
        library(plyr)
        library(dplyr)
        
        # TODO trim whitespace, add regexps
        EVTYPES <- c(
                "Astronomical Low Tide",
                "Avalanche",
                "Blizzard",
                "Coastal Flood",
                "Cold/Wind Chill",
                "Debris Flow",
                "Dense Fog",
                "Dense Smoke",
                "Drought",
                "Dust Devil",
                "Dust Storm",
                "Excessive Heat",
                "Extreme Cold/Wind Chill",
                "Flash Flood",
                "Flood",
                "Frost/Freeze",
                "Funnel Cloud",
                "Freezing Fog",
                "Hail",
                "Heat",
                "Heavy Rain",
                "Heavy Snow",
                "High Surf",
                "High Wind",
                "Hurricane (Typhoon)",
                "Ice Storm",
                "Lake-Effect Snow",
                "Lakeshore Flood",
                "Lightning",
                "Marine Hail",
                "Marine High Wind",
                "Marine Strong Wind",
                "Marine Thunderstorm Wind",
                "Rip Current",
                "Seiche",
                "Sleet",
                "Storm Surge/Tide",
                "Strong Wind",
                "Thunderstorm Wind",
                "Tornado",
                "Tropical Depression",
                "Tropical Storm",
                "Tsunami",
                "Volcanic Ash",
                "Waterspout",
                "Wildfire",
                "Winter Storm",
                "Winter Weather"
        )
        EVTYPES <- tolower(EVTYPES)
        
        # Start of better logging of 48 event types, see documentation
        begin_log <- ymd("1996/01/01")
        recent_stormdata <-
                stormdata %>% 
                mutate(DATE = mdy_hms(BGN_DATE),
                        EVTYPE = tolower(str_trim(EVTYPE))) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "hurricane/typhoon", "hurricane (typhoon)")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "^hurricane$", "hurricane (typhoon)")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "hurricane \\(typhoon\\) \\(typhoon\\)", "hurricane (typhoon)")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "tstm wind.*$", "thunderstorm wind")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "tstm wind$", "thunderstorm wind")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "thunderstorm high wind", "thunderstorm wind")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "fog", "dense fog")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "dense dense fog", "dense fog")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "rip currents", "rip current")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "wild/forest fire", "wildfire")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "^wind$", "high wind")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "high high wind", "high wind")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "strong high wind", "high wind")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "heavy surf/high surf", "high surf")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "heavy surf", "high surf")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "heat wave", "heat")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "winter weather mix", "winter weather")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "winter weather/mix", "winter weather")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "wintry mix", "winter weather")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "urban/sml stream fld", "flood")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "^extreme cold$", "extreme cold/wind chill")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "extreme cold/wind chill/high wind chill", "extreme cold/wind chill")) %>%
                mutate(EVTYPE = str_replace_all(EVTYPE, "cold/high wind chill", "extreme cold/wind chill")) %>%
                filter(DATE >= begin_log)
        
        # Analysis of effects on public health
        health_stormdata <- 
                recent_stormdata %>%
                mutate(FATALITIES = as.double(FATALITIES),
                       INJURIES = as.double(INJURIES)) %>%
                filter(FATALITIES > 0 | INJURIES > 0) %>%
                select(DATE, EVTYPE, FATALITIES, INJURIES)
        
        health_result <- health_stormdata %>%
                group_by(EVTYPE) %>%
                summarise(FATAL = sum(FATALITIES),
                          INJURED = sum(INJURIES)) %>%
                mutate(CORRECT_TYPE = EVTYPE %in% EVTYPES) %>%
                arrange(desc(FATAL), desc(INJURED))
        
        top10health <- head(health_result, 10)
        pie(top10health$FATAL, top10health$EVTYPE, col = rainbow(10))
        
        top10health <- head(arrange(health_result, desc(INJURED)), 10)
        pie(top10health$INJURED, top10health$EVTYPE, col = rainbow(10))
        
        # A look at values of the exponents
        unique(recent_stormdata$CROPDMGEXP)
        unique(recent_stormdata$PROPDMGEXP)
        damage_stormdata <- 
                recent_stormdata %>%
                mutate(PROPDMGEXP = mapvalues(PROPDMGEXP,
                                              from = c("", "0", "K", "M", "B"),
                                              to = c(1, 1, 1000, 1000000, 1000000000)),
                       CROPDMGEXP = mapvalues(CROPDMGEXP,
                                              from = c("", "K", "M", "B"),
                                              to = c(1, 1000, 1000000, 1000000000)),
                       TOTAL_DAMAGE = (as.numeric(PROPDMG) * as.numeric(PROPDMGEXP)) +
                                        (as.numeric(CROPDMG) * as.numeric(CROPDMGEXP))) %>%
                filter(TOTAL_DAMAGE > 0) %>%
                select(DATE, EVTYPE, TOTAL_DAMAGE)

        # Inflation adjust
        # Let's get the average yearly cpi since 1947. We'll use 1996 as our base year
        monthly_cpi <-
                read_csv("http://research.stlouisfed.org/fred2/data/CPIAUCSL.csv", col_names = TRUE)
        monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
        yearly_cpi <-
                monthly_cpi %>%
                group_by(cpi_year) %>%
                summarize(cpi = mean(VALUE))
        yearly_cpi$adj_factor <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == 1996]

        damage_stormdata <- 
                damage_stormdata %>%
                mutate(YEAR = year(DATE)) %>%
                left_join(yearly_cpi, by = c("YEAR" = "cpi_year")) %>%
                mutate(INFLATION_ADJUSTED_DAMAGE = TOTAL_DAMAGE * adj_factor) %>%
                select(DATE, EVTYPE, INFLATION_ADJUSTED_DAMAGE)
                
        damage_stormdata <-
                damage_stormdata
        
        damage_result <- damage_stormdata %>%
                group_by(EVTYPE) %>%
                summarize(DAMAGE = sum(INFLATION_ADJUSTED_DAMAGE)) %>%
                mutate(CORRECT_TYPE = EVTYPE %in% EVTYPES) %>%
                arrange(desc(DAMAGE))
}

str(stormdata)

