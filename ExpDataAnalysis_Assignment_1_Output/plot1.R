dir.create("assignment1")

##Read in the data
power_raw <- read.table("./assignment1/household_power_consumption.txt", col.names = 
                          c("all"))

## Select & Tidy Data for Dates of Interest
power_split[c("date", "time", "act_power", "reac_power", "volt",
              "intensity", "sub1", "sub2", "sub3")] <- str_split_fixed(
                power_raw$all, ";", 9)
power_split <- power_split[-1,]

power_dates <- filter(power_split, date %in% c("1/2/2007" , "2/2/2007"))

## Prepare Variables Required for Plots

power_dates <- power_dates %>%
  unite(col="fulldate", c("date", "time"), sep = " ")

power_dates <- transform(power_dates, fulldate = dmy_hms(fulldate))

power_dates <- power_dates %>% 
  mutate(wday = wday(power_dates$fulldate, label = T))

power_dates[,2:8] <- lapply(power_dates[,2:8], gsub, pattern = "\\.", replacement = "")

power_dates[,2:8] <- sapply(power_dates[,2:8], as.numeric)

power_dates <- power_dates %>% 
  mutate(act_power_kw = act_power/1000) %>% 
  mutate(reac_power_kw = reac_power/1000) %>% 
  mutate(volt_kv = volt/1000) %>% 
  mutate(sub1_kwh = sub1/1000) %>%
  mutate(sub2_kwh = sub2/1000) %>% 
  mutate(sub3_kwh = sub3/1000) 

str(power_dates)

##Plot One
png("plot1.png", height = 6, width = 6, units = "in", res = 80)
hist(power_dates$act_power_kw, col = "red", xlab = 
       "Global Active Power (kilowatts)", 
     main = "Global Active Power")
dev.off()
