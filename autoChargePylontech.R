###
### SOLARMAN AUTOCHARGE
### requires python installation with pysolarmanv5
###

###
### set parameters for autocharge
###
# set working folder where python and R scripts are located
wd <- "~/sandbox/solarman_battery_autocharge/"
# charge & production target
min.production.summer <- 8000 # min wh to get through the day avoiding day unit rates 
min.production.winter <- 10000 # min wh to get through the day avoiding day unit rates 
min.production <- 7000
over.production <- 14000 # threshold after which autocharge is turned off if battery is half full
# battery specs
battery.size <- 6400 # battery usable storage in wh
battery.voltage <- 48 # battery changing voltage
battery.max.current <- 74 # battery recommended maximum charge rate
retrieve.battery.soc <- FALSE # requires access to home assistant database--adjust path to home-assistant_v2.db and sensor name
record.solar.generation <- FALSE # requires access to home assistant database--adjust path to home-assistant_v2.db and sensor name 
# charging schedule
adjust.dst.winter <- TRUE # set to TRUE for auto adjustment based on daylight saving time
winter.schedule <- c(0, 5, 7, 55) # set charging schedule for the winter period--e.g., 12:05AM to 7:55AM
summer.schedule <- c(1, 5, 8, 55) # set charging schedule for the summer period--e.g., 1:05AM to 8:55AM
# solar farm details
latitude <- "" # location (lat) of solar installation 
longitude <- "" # location (lon) of solar installation 
kwp <- "" # kwp of solar installation 
slope <- "" # slope/angle of solar installation
orientation <- 180 # orientatio of solar installation, where 180 equal due south
# solcast account
solcast.resource.id <- ""
solcast.api.key <- ""
###
### end of parameters for autocharge
###

# load required packages
setwd(wd)
required.packages <- c("jsonlite","lubridate","dplyr","rvest","xml2")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
if(!all(unlist(lapply(required.packages, require, character.only=T)))) { stop ("Please install required packages") }

# identify projection day
if(lubridate::hour(Sys.time()) %in% c(0, 24, 1, 2)) { 
  target.forecast <- Sys.Date() } else { 
  target.forecast <- Sys.Date()+1 }

# build API call endpoint
forecast.solar.endpoint <- paste0("https://api.forecast.solar/estimate/watthours/day/",latitude,"/",longitude,"/",slope,"/",orientation,"/",kwp)
solcast.endpoint <- paste0("https://api.solcast.com.au/rooftop_sites/",solcast.resource.id,"/forecasts?period=PT30M&hours=24&format=json&api_key=",solcast.api.key)
met.eireann.endpoint <- paste0("http://metwdb-openaccess.ichec.ie/metno-wdb2ts/locationforecast?lat=",latitude,";long=",longitude)

# define minimum.production based on summer/winter requirements
dateseq <- seq(as.Date(paste0(format(Sys.Date(), "%Y"),"-01-01")), as.Date(paste0(format(Sys.Date(), "%Y"),"-12-31")), by="days")
if(Sys.Date() %in% dateseq[which(lubridate::dst(as.character(dateseq))==F)]) { 
  min.production <- min.production.summer } else { min.production <- min.production.winter } 

# retrieve battery state of charge
battery.soc <- battery.power <- 0
if(retrieve.battery.soc==T) {
  # retieve battery soc from home assistant database
  battery.soc.call <- "sqlite3 -header -csv /var/snap/home-assistant-snap/current/home-assistant_v2.db \"select * from states where entity_id=\'sensor.solis_local_battery_soc\'\";"
  battery.soc <- read.table(textConnection(tail(system(paste0(battery.soc.call), intern = T), 1)), sep = ",")[[4]]
  battery.power <- ((battery.soc-20)/100)*battery.size # take 20% off for the depth of discharge
  if(battery.power<0) { battery.power <- 0 }
}

# record actual solar generation
if(record.solar.generation==T) {
  solis.generation <- "sqlite3 -header -csv /var/snap/home-assistant-snap/current/home-assistant_v2.db \"select * from states where entity_id=\'sensor.solis_local_daily_generation\'\";"
  solis.generation.db <- read.table(textConnection((system(paste0(solis.generation), intern = T))), sep = ",")
  solis.generation.db$time <- as.POSIXct(solis.generation.db$V8, format="%Y-%m-%d %H:%M:%S")
  solis.generation.db$date <- as.Date(solis.generation.db$time)
  suppressWarnings(solis.generation.db$production <- as.numeric(solis.generation.db$V4))
  solis.generation.db$production[is.na(solis.generation.db$production)] <- 0
  solis.generation.week <- aggregate(solis.generation.db$production, by = list(solis.generation.db$date), max)
  solis.generation.week$x <- solis.generation.week$x*1000
  if(file.exists("daily.estimates.csv")) { 
    solis.daily.estimates <- read.csv("daily.estimates.csv", sep = ",") 
    solis.daily.estimates$date <- as.Date(solis.daily.estimates$date)
    solis.daily.estimates$actual[solis.daily.estimates$date==(target.forecast-1)] <- solis.generation.week$x[solis.generation.week$Group.1==(target.forecast-1)]
    write.table(solis.daily.estimates, "daily.estimates.csv", sep = ",", row.names = F, append = F)
  }
}

# read from inverter
solis_battery_status.py <- paste0("/usr/bin/python ", wd, "solis_battery_status.py")
solis_battery_status.py.response <- character()
while(length(solis_battery_status.py.response) < 1) { Sys.sleep(3); solis_battery_status.py.response <- system(paste0(solis_battery_status.py), intern = T) }
current.schedule <- as.numeric(unlist(regmatches(solis_battery_status.py.response[3], gregexpr("[[:digit:]]+", solis_battery_status.py.response[3]))))
charging.time <- as.numeric(as.POSIXct(paste0(Sys.Date(), " ", current.schedule[3], ":", current.schedule[4]))-as.POSIXct(paste0(Sys.Date(), " ", current.schedule[1], ":", current.schedule[2])))
charging.total <- charging.time*battery.voltage

# adjust for DN meter & daylight saving time
if(adjust.dst.winter==T) {
  solis_battery_time.py.response <- character()
  # set charging time to summer schedule
  if(Sys.Date() %in% dateseq[which(lubridate::dst(as.character(dateseq))==F)]) {
    if(all(current.schedule==summer.schedule)) { print("Time schedule correct") } else {
      while(length(solis_battery_time.py.response) < 1) {
        Sys.sleep(3)
        solis_battery_time.py.response <- system(paste0("/usr/bin/python ", wd, "solis_battery_charging_time_par.py ", paste(summer.schedule, collapse = " ")), intern = T)
      }
    }
  }
  # set charging time to winter schedule
  if(Sys.Date() %in% dateseq[which(lubridate::dst(as.character(dateseq))==T)]) {
    if(all(current.schedule==winter.schedule)) { print("Time schedule correct") } else {
      while(length(solis_battery_time.py.response) < 1) {
        Sys.sleep(3)
        solis_battery_time.py.response <- system(paste0("/usr/bin/python ", wd, "solis_battery_charging_time_par.py ", paste(winter.schedule, collapse = " ")), intern = T)
      }
    }
  }
}

# build response vectors
response.met.eireann <- response.forecast.solar <- response.solcast <- best.guestimate <- 0
guess.met.eireann <- guess.forecast.solar <- guess.sol.cast <- 0

# query met.eireann
response.met.eireann <- character()
try(met.eireann.solar <- xml2::read_xml(met.eireann.endpoint), silent = T)
if(length(met.eireann.solar) > 0) {
  time <- dplyr::bind_rows(lapply(xml_attrs(rvest::html_nodes(met.eireann.solar, "time")), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
  time <- time[which(time$from==time$to),]
  cloudiness <- dplyr::bind_rows(lapply(xml_attrs(rvest::html_nodes(met.eireann.solar, "cloudiness")), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
  temperature <- dplyr::bind_rows(lapply(xml_attrs(rvest::html_nodes(met.eireann.solar, "temperature")), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
  radiation <- dplyr::bind_rows(lapply(xml_attrs(rvest::html_nodes(met.eireann.solar, "globalRadiation")), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
  response.met.eireann <- cbind(time, cloudiness, temperature, radiation)
  response.met.eireann$from <- as.POSIXct(response.met.eireann$from, format="%Y-%m-%dT%H:%M:%S")
  response.met.eireann$to <- as.POSIXct(response.met.eireann$to, format="%Y-%m-%dT%H:%M:%S")
  response.met.eireann$date <- as.Date(response.met.eireann$from)
  eireann1 <- response.met.eireann[response.met.eireann$date %in% target.forecast,]
  guess.met.eireann <- ceiling((sum(as.numeric(eireann1[[9]]))*kwp)*0.85) # take down .15 for accuracy
  # ceiling(aggregate(as.numeric(response.met.eireann[[9]]), by=list(response.met.eireann$date), sum)$x*kwp)
}

# query forecast.solar
response.forecast.solar <- character()
try(response.forecast.solar <- jsonlite::fromJSON(forecast.solar.endpoint), silent = T)
if(length(response.forecast.solar) > 0) {
  target.response <- which((names(unlist(response.forecast.solar$result)))==target.forecast)
  if(as.Date(names(response.forecast.solar$result[target.response]))==target.forecast) {
    guess.forecast.solar <- response.forecast.solar$result[[target.response]] } else {
      stop("forecast.solar dates mismatch")
    }
}

# query sol.cast
response.solcast <- character()
try(response.solcast <- jsonlite::fromJSON(solcast.endpoint), silent = T)
if(length(response.solcast) > 0) {
  response.solcast <- response.solcast$forecasts
  response.solcast$period_end <- as.POSIXct(response.solcast$period_end, format="%Y-%m-%dT%H:%M:%S")
  response.solcast$date <- as.Date(response.solcast$period_end)
  solcast1 <- response.solcast[response.solcast$date %in% target.forecast,]
  solcast1$datehour <-substr(as.character(solcast1$period_end), 1, 13)
  guess.sol.cast <- sum(aggregate(solcast1$pv_estimate, by=list(solcast1$datehour), mean)$x)
  guess.sol.cast <- round(guess.sol.cast*1000, 0)
}

# take mean of forecast.solar, solcast, and met.eireann to charge battery
best.guestimate.tot <- (guess.met.eireann+guess.forecast.solar+guess.sol.cast)
if(guess.met.eireann==0) { guess.met.eireann <- character() }
if(guess.forecast.solar==0) { guess.forecast.solar <- character() }
if(guess.sol.cast==0) { guess.sol.cast <- character() }
valid.guesses <- length(guess.met.eireann)+length(guess.sol.cast)+length(guess.forecast.solar)
best.guestimate <-ceiling(best.guestimate.tot/valid.guesses)
print(paste0("Estimate solar production is ",best.guestimate/1000, " kWh"))
if(best.guestimate==0) { stop("error retrieving solar production estimates")}

# write forecasts to file
daily.estimates <- data.frame(date=Sys.Date(), actual=NA, mean=best.guestimate, forecast.solar=guess.forecast.solar, solcast=guess.sol.cast, met.eireann=guess.met.eireann)
write.table(daily.estimates, "daily.estimates.csv", sep = ",", row.names = F, col.names = !file.exists("daily.estimates.csv"), append = T)

# build response vector
solis_battery_charging_par.py.response <- solis_battery_charging_on.py.response <- solis_battery_charging_off.py.response <- character()

# build python calls
solis_battery_status.py <- paste0("/usr/bin/python ", wd, "solis_battery_status.py")
solis_battery_charging_off.py <- paste0("/usr/bin/python ", wd, "solis_battery_charging_off.py")
solis_battery_charging_on.py <- paste0("/usr/bin/python ", wd, "solis_battery_charging_on.py")
solis_battery_charging_par.py <- paste0("/usr/bin/python ", wd, "solis_battery_charging_par.py")

# set auto charge on/off
if(best.guestimate > over.production) {
  if(retrieve.battery.soc==T) { if(battery.soc>60 & battery.power>(battery.size/1.7)) {
  if(35==unlist(regmatches(solis_battery_status.py.response[1], gregexpr("[[:digit:]]+", solis_battery_status.py.response[1])))[3]) {
    while(length(solis_battery_charging_off.py.response) < 1) {
      Sys.sleep(3)
      solis_battery_charging_off.py.response <- system(paste0(solis_battery_charging_off.py), intern = T)
      print("High solar production. Turning off battery autocharge") }
  } } }
}
if(best.guestimate <= min.production) { 
  if(33==unlist(regmatches(solis_battery_status.py.response[1], gregexpr("[[:digit:]]+", solis_battery_status.py.response[1])))[3]) {
    while(length(solis_battery_charging_on.py.response) < 1) {
      Sys.sleep(3)
      solis_battery_charging_on.py.response <- system(paste0(solis_battery_charging_on.py), intern = T)
      print("Low solar production. Turning on battery autocharge") }
  }
}

# set battery charge rate
charge.required <- min.production-best.guestimate
if(charge.required<=0) { charge.required <- 0 }
if(charge.required > battery.size) { charge.required <- battery.size }
if(retrieve.battery.soc==T) { if(battery.soc>50 & battery.power>(battery.size/2)) { charge.required <- charge.required-battery.power } }
if(charge.required<=0) { charge.required <- 0 }
charging.current <- ceiling((charge.required/charging.total))*10
if(charging.current > battery.max.current*10) { charging.current <- battery.max.current*10 }
print(paste0("Charging ", charging.current/10, "A at ", battery.voltage,"V over ", round(charging.time), " hours to charge ",charge.required, "Wh"))
Sys.sleep(3)
solis_battery_charging_par.py.response <- character()
while(length(solis_battery_charging_par.py.response) < 1) { 
  solis_battery_charging_par.py.response <- system(paste0(solis_battery_charging_par.py, " ", charging.current), intern = T) 
} 

# record battery status
solis_battery_status.py.response <- character()
while(length(solis_battery_status.py.response) < 1) { Sys.sleep(3); solis_battery_status.py.response <- system(paste0(solis_battery_status.py), intern = T) }
if(unlist(regmatches(solis_battery_status.py.response[1], gregexpr("[[:digit:]]+", solis_battery_status.py.response[1])))[3]==35) { solis_battery_status.py.response[1] <- "on"} else { solis_battery_status.py.response[1] <- "off" }
solis_battery_status.py.response[2] <- unlist(regmatches(solis_battery_status.py.response[2], gregexpr("[[:digit:]]+", solis_battery_status.py.response[2])))
solis_battery_status.py.response[3] <- gsub("Charge Time | |\\[|\\]", "", solis_battery_status.py.response[3])

# export state of charge
writeLines(text = solis_battery_status.py.response, con = "solis_battery_status.py.response.txt")
names(solis_battery_status.py.response) <- c("status", "current", "time")
solis_battery_status.py.response.df <- data.frame(status=solis_battery_status.py.response[1], current=solis_battery_status.py.response[2], time=solis_battery_status.py.response[3])
rownames(solis_battery_status.py.response.df) <- NULL

## export to home assistant sensors
#writeLines(text = solis_battery_status.py.response[1], con = "/media/solis_battery_status.txt")
#writeLines(text = solis_battery_status.py.response[2], con = "/media/solis_battery_current.txt")
#writeLines(text = solis_battery_status.py.response[3], con = "/media/solis_battery_time.txt")
#writeLines(text = as.character(charge.required), con = "/media/charge.required.txt")
#writeLines(text = as.character(best.guestimate), con = "/media/best.guestimate.txt")

