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

##Plot Four
png("plot4.png", height = 6, width = 6, units = "in", res = 80)
par(mfrow = c(2,2), mar = c(4,4,2,1), oma = c(0,0,1,0))
with(power_dates, plot(fulldate, act_power_kw, type = "n",
                       xlab = "", ylab = "Global Active Power",
                       xaxt = "n"))
with(power_dates, lines(fulldate, act_power_kw))
xax <- dmy_hms(c("01/02/2007 00:00:00", "02/02/2007 00:00:00", "03/02/2007 00:00:00"))
axis(1, at = xax, labels = c("Thu", "Fri", "Sat"))
with(power_dates, plot(fulldate, volt_kv, type = "n",
                       xlab = "datetime", ylab = "Voltage", xaxt = "n"))
with(power_dates, lines(fulldate, volt_kv))
xax <- dmy_hms(c("01/02/2007 00:00:00", "02/02/2007 00:00:00", "03/02/2007 00:00:00"))
axis(1, at = xax, labels = c("Thu", "Fri", "Sat"))
with(power_dates, plot(fulldate, sub1_kwh, type = "n",
                       xlab = "", ylab = "Energy Sub Metering", xaxt = "n"))
with(power_dates, lines(fulldate, sub1_kwh))
with(power_dates, lines(fulldate, sub2_kwh, col = "red"))
with(power_dates, lines(fulldate, sub3_kwh, col = "blue"))
legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
       col = c("black", "red", "blue"), lty = 1, cex = 0.5)
xax <- dmy_hms(c("01/02/2007 00:00:00", "02/02/2007 00:00:00", "03/02/2007 00:00:00"))
axis(1, at = xax, labels = c("Thu", "Fri", "Sat"))
with(power_dates, plot(fulldate, reac_power_kw, type = "n",
                       xlab = "datetime", ylab = "Global_reactive_power", xaxt = "n"))
with(power_dates, lines(fulldate, reac_power_kw))
xax <- dmy_hms(c("01/02/2007 00:00:00", "02/02/2007 00:00:00", "03/02/2007 00:00:00"))
axis(1, at = xax, labels = c("Thu", "Fri", "Sat"))
dev.off()
