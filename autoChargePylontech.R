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
min.production <- 8000 # min wh to get through the day avoiding day unit rates 
winter.extra <- 2000 # extra wh to get through the winter days avoiding day unit rates 
over.production <- 12000 # threshold after which autocharge is turned off if battery is half full
# battery specs
battery.size <- (3552*2) # battery storage in wh
battery.voltage <- 48 # battery changing voltage
battery.max.current <- 37 # battery recommended maximum charge rate
retrieve.battery.soc <- FALSE # requires access to home assistant database 
record.solar.generation <- TRUE # retrieves solar production from inverter (default) or home assistant
solar.generation.source <- "inverter" # set "HA" to retrieve production from HA instead of the inventer
battery.reserve <- 55 # battery dod plus minimum charge (percentage)
battery.reserve.force <- TRUE # charge overnight to battery.reserve  
winter.buffer <- 740 # minimum morning charge in wh for winter days when the sun comes later in the day
# charging schedule
adjust.dst.winter <- TRUE # set TRUE for auto adjustment based on daylight saving time
winter.schedule <- c(0, 15, 7, 59) # set charging schedule for the winter period
summer.schedule <- c(0, 15, 8, 59) # set charging schedule for the summer period
# solar farm details
latitude <- "" # location (lat) of solar installation 
longitude <- "" # location (lon) of solar installation 
kwp <- "" # kwp of solar installation 
slope <- "" # slope/angle of solar installation 
orientation.forecast.solar <- 180 # orientation of solar installation, where 0 (not 180!) equals due south
# solcast account
solcast.resource.id <- ""
solcast.api.key <- ""
# parameters for met eireann, solcast, and forecast.solar
calibrate.best.guestimate <- FALSE # calibrate forecast data on moving averages of solar production
calibrate.days <- 5 # number of preceding days to use in the calibration model (requires available data in daily.estimates.csv)
query.solcast <- TRUE # set TRUE to query solcast for weather forecasts 
query.forecast.solar <- TRUE # set TRUE to query forecast.solar for weather forecasts
query.met.eireann <- TRUE # set TRUE to query met.eireann for weather forecasts. NOTE: service only available in Ireland 
use.solcast <- FALSE # set FALSE to remove prediction from the model
use.forecast.solar <- FALSE # set FALSE to remove prediction from the model
use.met.eireann <- TRUE # set FALSE to remove prediction from the model 
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

# define minimum.production based on summer/winter requirements
dateseq <- seq(as.Date(paste0(format(Sys.Date(), "%Y"),"-01-01")), as.Date(paste0(format(Sys.Date(), "%Y"),"-12-31")), by="days")
if(Sys.Date() %in% dateseq[which(lubridate::dst(as.character(dateseq)))]) { 
  min.production <- min.production+winter.extra
  over.production <- over.production+winter.extra
}

# identify projection day
if(lubridate::hour(Sys.time()) %in% c(0, 24, 1, 2)) { 
  target.forecast <- Sys.Date() } else {
    target.forecast <- Sys.Date()+1 }

# build API call endpoint
if(query.forecast.solar==TRUE) { forecast.solar.endpoint <- paste0("https://api.forecast.solar/estimate/watthours/day/",latitude,"/",longitude,"/",slope,"/",orientation.forecast.solar,"/",kwp) }
if(query.solcast==TRUE) { solcast.endpoint <- paste0("https://api.solcast.com.au/rooftop_sites/",solcast.resource.id,"/forecasts?period=PT30M&hours=24&format=json&api_key=",solcast.api.key) }
#if(query.met.eireann==TRUE) { met.eireann.endpoint <- paste0("http://metwdb-openaccess.ichec.ie/metno-wdb2ts/locationforecast?lat=",latitude,";long=",longitude) } ## old and deprecated
if(query.met.eireann==TRUE) { met.eireann.endpoint <- paste0("http://openaccess.pf.api.met.ie/metno-wdb2ts/locationforecast?lat=",latitude,";long=",longitude) }

# retrieve battery state of charge
battery.soc <- battery.power <- 0
if(retrieve.battery.soc==T) {
  # retieve battery soc from home assistant database
  battery.soc.call <- "sqlite3 -header -csv /var/snap/home-assistant-snap/current/home-assistant_v2.db \"select * from states where entity_id=\'sensor.solis_local_battery_soc\'\";"
  battery.soc <- read.table(textConnection(tail(system(paste0(battery.soc.call), intern = T), 1)), sep = ",")[[4]]
  battery.power <- ((battery.soc)/100)*battery.size # take 20% off for the depth of discharge
  if(battery.power<0) { battery.power <- 0 }
}

# record actual solar generation
if(record.solar.generation==T) {
  if(solar.generation.source=="HA") {
    solis.generation <- "sqlite3 -header -csv /var/snap/home-assistant-snap/current/home-assistant_v2.db \"select * from states where entity_id=\'sensor.solis_local_daily_generation\'\";"
    solis.generation.db <- read.table(textConnection((system(paste0(solis.generation), intern = T))), sep = ",")
    solis.generation.db$time <- as.POSIXct(solis.generation.db$V8, format="%Y-%m-%d %H:%M:%S")
    solis.generation.db$date <- as.Date(solis.generation.db$time)
    suppressWarnings(solis.generation.db$production <- as.numeric(solis.generation.db$V4))
    solis.generation.db$production[is.na(solis.generation.db$production)] <- 0
    solis.generation.week <- aggregate(solis.generation.db$production, by = list(solis.generation.db$date), max)
    solis.generation.week$x <- solis.generation.week$x*1000 
    }
  if(solar.generation.source!="HA") {
    solis_battery_generated_yesterday.py <- paste0("/usr/bin/python ", wd, "solis_battery_generated_yesterday.py")
    solis_battery_generated_yesterday.py.response <- character()
    while(length(solis_battery_generated_yesterday.py.response)!=1) { Sys.sleep(5); solis_battery_generated_yesterday.py.response <- system(paste0(solis_battery_generated_yesterday.py), intern=T) }
    print(solis_battery_generated_yesterday.py.response)
    solis_battery_generated_yesterday.py.response <- as.integer(gsub("\\D+", "", solis_battery_generated_yesterday.py.response))*100
  }
  if(file.exists("daily.estimates.csv")) { 
    solis.daily.estimates <- read.csv("daily.estimates.csv", sep = ",")
    write.table(solis.daily.estimates, "daily.estimates.csv.bck", sep = ",", row.names = F, append = F)
    solis.daily.estimates$date <- as.Date(solis.daily.estimates$date)
    if(solar.generation.source=="HA") { solis.daily.estimates$actual[solis.daily.estimates$date==(target.forecast-1)] <- solis.generation.week$x[solis.generation.week$Group.1==(target.forecast-1)] }
    if(solar.generation.source!="HA") { solis.daily.estimates$actual[solis.daily.estimates$date==(target.forecast-1)] <- solis_battery_generated_yesterday.py.response }
    write.table(solis.daily.estimates, "daily.estimates.csv", sep = ",", row.names = F, append = F)
  }
}

# read from inverter
charging.time <- 8
solis_battery_status.py <- paste0("/usr/bin/python ", wd, "solis_battery_status.py")
solis_battery_status.py.response <- character()
while(length(solis_battery_status.py.response)!=3) { Sys.sleep(5); solis_battery_status.py.response <- system(paste0(solis_battery_status.py), intern = T) }
current.schedule <- as.numeric(unlist(regmatches(solis_battery_status.py.response[3], gregexpr("[[:digit:]]+", solis_battery_status.py.response[3]))))
charging.time <- as.numeric(as.POSIXct(paste0(Sys.Date(), " ", current.schedule[3], ":", current.schedule[4]))-as.POSIXct(paste0(Sys.Date(), " ", current.schedule[1], ":", current.schedule[2])))
charging.total <- charging.time*battery.voltage

# adjust for DN meter & daylight saving time
if(adjust.dst.winter==T) {
  solis_battery_time.py.response <- character()
  # set charging time to summer schedule
  if(Sys.Date() %in% dateseq[which(lubridate::dst(as.character(dateseq))==F)]) {
    if(all(current.schedule==summer.schedule)) { print("Time schedule correct") } else {
      while(length(solis_battery_time.py.response)!=12) {
        Sys.sleep(5)
        try( solis_battery_time.py.response <- system(paste0("/usr/bin/python ", wd, "solis_battery_charging_time_par.py ", paste(summer.schedule, collapse = " ")), intern = T), silent = T)
      }
    }
  }
  # set charging time to winter schedule
  if(Sys.Date() %in% dateseq[which(lubridate::dst(as.character(dateseq))==T)]) {
    if(all(current.schedule==winter.schedule)) { print("Time schedule correct") } else {
      while(length(solis_battery_time.py.response)!=12) {
        Sys.sleep(5)
        try( solis_battery_time.py.response <- system(paste0("/usr/bin/python ", wd, "solis_battery_charging_time_par.py ", paste(winter.schedule, collapse = " ")), intern = T), silent = T)
      }
    }
  }
}

# build response vectors
response.met.eireann <- response.forecast.solar <- response.solcast <- best.guestimate <- 0
guess.met.eireann <- guess.forecast.solar <- guess.sol.cast <- 0

# query met.eireann
response.met.eireann <- character()
if(query.met.eireann==TRUE) {
  try(met.eireann.solar <- xml2::read_xml(met.eireann.endpoint), silent = T)
  if(length(met.eireann.solar) > 0) {
    time <- dplyr::bind_rows(lapply(xml_attrs(rvest::html_nodes(met.eireann.solar, "time")), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
    time <- time[which(time$from==time$to),]
    cloudiness <- dplyr::bind_rows(lapply(xml2::xml_attrs(rvest::html_nodes(met.eireann.solar, "cloudiness")), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
    temperature <- dplyr::bind_rows(lapply(xml2::xml_attrs(rvest::html_nodes(met.eireann.solar, "temperature")), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
    radiation <- dplyr::bind_rows(lapply(xml2::xml_attrs(rvest::html_nodes(met.eireann.solar, "globalRadiation")), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
    response.met.eireann <- cbind(time, cloudiness, temperature, radiation)
    response.met.eireann$from <- as.POSIXct(response.met.eireann$from, format="%Y-%m-%dT%H:%M:%S")
    response.met.eireann$to <- as.POSIXct(response.met.eireann$to, format="%Y-%m-%dT%H:%M:%S")
    response.met.eireann$date <- as.Date(response.met.eireann$from)
    eireann1 <- response.met.eireann[response.met.eireann$date %in% target.forecast,]
    guess.met.eireann <- ceiling((sum(as.numeric(eireann1[[9]]))*kwp)*0.85)
    # aggregate(as.numeric(response.met.eireann[[9]]), by=list(response.met.eireann$date), sum)
    # ceiling(aggregate(as.numeric(response.met.eireann[[9]]), by=list(response.met.eireann$date), sum)$x*2.92**0.85)
    # export 10-day forecast
    met.eireann.forecast <- aggregate((as.numeric(response.met.eireann[[9]])*(kwp*0.85)), by=list(response.met.eireann$date), sum)
    names(met.eireann.forecast) <- c("forecast", "kwh")
    met.eireann.forecast$kwh <- round(met.eireann.forecast$kwh)
    met.eireann.forecast.db <- met.eireann.forecast
    met.eireann.forecast.db$date <- Sys.Date()
    write.table(met.eireann.forecast.db, "met.eireann.forecast.db.csv", sep = ",", row.names = F, col.names = !file.exists("met.eireann.forecast.db.csv"), append = T)
    write.csv(met.eireann.forecast, file="met.eireann.forecast.csv")
  }
}

# query forecast.solar
response.forecast.solar <- character()
if(query.forecast.solar==TRUE) {
  try(response.forecast.solar <- jsonlite::fromJSON(forecast.solar.endpoint), silent = T)
  if(length(response.forecast.solar) > 0) {
    target.response <- which((names(unlist(response.forecast.solar$result)))==target.forecast)
    if(as.Date(names(response.forecast.solar$result[target.response]))==target.forecast) {
      guess.forecast.solar <- response.forecast.solar$result[[target.response]] } else {
        stop("forecast.solar dates mismatch")
      }
  }
}

# query sol.cast
response.solcast <- character()
if(query.solcast==TRUE) {
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
}
  
# calculate weekly moving averages based on production
if(calibrate.best.guestimate==TRUE & record.solar.generation==TRUE) {
  # calibrate met.eireann irradiation on moving averages of solar production
  mest.est <- solis.daily.estimates$met.eireann[c((nrow(solis.daily.estimates)-calibrate.days):nrow(solis.daily.estimates))]
  solis.actual <- solis.daily.estimates$actual[c((nrow(solis.daily.estimates)-calibrate.days):nrow(solis.daily.estimates))]
  guess.met.eireann.cal <- ceiling(guess.met.eireann*(mean(solis.actual[!is.na(mest.est)]/mest.est[!is.na(mest.est)])))
  # calibrate solcast on moving averages of solar production
  solcast.est <- solis.daily.estimates$solcast[c((nrow(solis.daily.estimates)-calibrate.days):nrow(solis.daily.estimates))]
  solis.actual <- solis.daily.estimates$actual[c((nrow(solis.daily.estimates)-calibrate.days):nrow(solis.daily.estimates))]
  guess.solcast.cal <- ceiling(guess.sol.cast*(mean(solis.actual[!is.na(solcast.est)]/solcast.est[!is.na(solcast.est)])))
  # calibrate forecast.solar on moving averages of solar production
  forecast.solar.est <- solis.daily.estimates$forecast.solar[c((nrow(solis.daily.estimates)-calibrate.days):nrow(solis.daily.estimates))]
  solis.actual <- solis.daily.estimates$actual[c((nrow(solis.daily.estimates)-calibrate.days):nrow(solis.daily.estimates))]
  guess.forecast.solar.cal <- ceiling(guess.forecast.solar*(mean(solis.actual[!is.na(forecast.solar.est)]/forecast.solar.est[!is.na(forecast.solar.est)])))
}

# backup recorded predictions for historical data
guess.met.eireann.ori <- guess.met.eireann
guess.forecast.solar.ori <- guess.forecast.solar
guess.sol.cast.ori <- guess.sol.cast

# select prediction services for model
if(use.solcast==FALSE) { guess.sol.cast <- 0}
if(use.forecast.solar==FALSE) { guess.forecast.solar <- 0}
if(use.met.eireann==FALSE) { guess.met.eireann <- 0 }

# take mean of forecast.solar, solcast, and met.eireann to charge battery
if(calibrate.best.guestimate==TRUE) { 
  best.guestimate.tot <- ceiling((guess.met.eireann.cal+guess.met.eireann)/2 +(guess.forecast.solar.cal+guess.forecast.solar)/2 +(guess.solcast.cal+guess.sol.cast)/2) 
  } else { best.guestimate.tot <- ceiling(guess.met.eireann+guess.forecast.solar+guess.sol.cast) }
if(guess.met.eireann==0) { guess.met.eireann <- character() }
if(guess.forecast.solar==0) { guess.forecast.solar <- character() }
if(guess.sol.cast==0) { guess.sol.cast <- character() }
valid.guesses <- length(guess.met.eireann)+length(guess.sol.cast)+length(guess.forecast.solar)
best.guestimate <-ceiling(best.guestimate.tot/valid.guesses)
print(paste0("Estimate solar production is ",best.guestimate/1000, " kWh (weighted) or ", round((guess.met.eireann.ori+guess.forecast.solar.ori+guess.sol.cast.ori)/3000, 2), " (uncalibrated)"))
if(best.guestimate==0) { stop("error retrieving solar production estimates")}

# write forecasts to file
daily.estimates <- data.frame(date=target.forecast, actual=NA, expected=best.guestimate, forecast.solar=guess.forecast.solar.ori, solcast=guess.sol.cast.ori, met.eireann=guess.met.eireann.ori)
write.table(daily.estimates, "daily.estimates.csv", sep = ",", row.names = F, col.names = !file.exists("daily.estimates.csv"), append = T)

# build response vector
solis_battery_charging_par.py.response <- solis_battery_charging_current_kwh_0000.py.response <- solis_battery_charging_current_kwh_0500.py.response <- solis_battery_charging_current_kwh_1000.py.response <- solis_battery_charging_current_kwh_1300.py.response <- solis_battery_charging_current_kwh_1500.py.response <- solis_battery_charging_current_kwh_2500.py.response <- solis_battery_charging_current_kwh_3300.py.response <- solis_battery_charging_current_kwh_5000.py.response <- solis_battery_charging_current_kwh_6000.py.response <- solis_battery_charging_on.py.response <- solis_battery_charging_off.py.response <- character()

# build python calls
solis_battery_status.py <- paste0("/usr/bin/python ", wd, "solis_battery_status.py")
solis_battery_charging_off.py <- paste0("/usr/bin/python ", wd, "solis_battery_charging_off.py")
solis_battery_charging_on.py <- paste0("/usr/bin/python ", wd, "solis_battery_charging_on.py")
solis_battery_charging_par.py <- paste0("/usr/bin/python ", wd, "solis_battery_charging_par.py")

# set auto charge on/off
if(best.guestimate > over.production) {
  if(retrieve.battery.soc==T & battery.soc>49 & battery.power>(battery.size/1.7)) {
  if(35==unlist(regmatches(solis_battery_status.py.response[1], gregexpr("[[:digit:]]+", solis_battery_status.py.response[1])))[3]) {
    while(length(solis_battery_charging_off.py.response) < 1) {
      Sys.sleep(5)
      solis_battery_charging_off.py.response <- system(paste0(solis_battery_charging_off.py), intern = T)
      print("High solar production. Turning off battery autocharge") }
  } }
}
if(best.guestimate <= min.production) { 
  if(33==unlist(regmatches(solis_battery_status.py.response[1], gregexpr("[[:digit:]]+", solis_battery_status.py.response[1])))[3]) {
    while(length(solis_battery_charging_on.py.response) < 1) {
      Sys.sleep(5)
      solis_battery_charging_on.py.response <- system(paste0(solis_battery_charging_on.py), intern = T)
      print("Low solar production. Turning on battery autocharge") }
  }
}

# calculate charge required
charge.required <- min.production-best.guestimate
if(charge.required<=0) { charge.required <- 0 }
# calculate battery charge by current and time
battery.charge <- 0
if(retrieve.battery.soc==T & battery.reserve.force==T) {
  # bring battery to battery.reserve (defaults to 55%)
  if(best.guestimate<=(min.production*1.125) & best.guestimate>(min.production*0.75)) { battery.charge <- battery.charge+(ceiling(battery.size*(battery.reserve-battery.soc)/100)) }
  # add extra 10% to battery when low production is forecast
  if(best.guestimate<=(min.production*0.75)) { battery.charge <- battery.charge+(ceiling(battery.size*((battery.reserve+10)-battery.soc)/100)) }
}
if(battery.charge<=0) { battery.charge <- 0 }
# combine charges
charge.required <- charge.required+battery.charge
# if(retrieve.battery.soc==T) {
# charge.required <- min.production-((ceiling(battery.power*((battery.reserve)/100)))+best.guestimate)
# if(charge.required<=0) { charge.required <- 0 }
if(charge.required<=0) { charge.required <- 0 }
# add buffer outside summer months
if(!month(Sys.time()) %in% 4:8) { if(charge.required<winter.buffer) { charge.required <- winter.buffer } }
if(charge.required > battery.size) { charge.required <- battery.size }
#if(retrieve.battery.soc==T) { if(battery.soc>45 & best.guestimate>min.production) { charge.required <- charge.required-battery.power } }
# do not over charge battery
if((charge.required + (battery.size*(battery.soc/100))) > battery.size) { charge.required <- ceiling(battery.size-(battery.size*(battery.soc/100))) }
if(charge.required<=0) { charge.required <- 0 }
charging.current <- ceiling((charge.required/charging.total))*10
# do not exceed battery max current
if(charging.current > (battery.max.current*10)) { charging.current <- battery.max.current*10 }
# set 1A minimum charge
if(charging.current > 0 & charging.current < 11) { charging.current <- 10 }
print(paste0("Charging ", charging.current/10, "A @", battery.voltage,"V for ", round(charging.time), " hours to charge ",(charging.current/10)*battery.voltage*charging.time, "Wh"))

# write changes to inverter
solis_battery_status.py.response <- character()
while(length(solis_battery_status.py.response)!=3) { Sys.sleep(5); solis_battery_status.py.response <- system(paste0(solis_battery_status.py), intern = T) }
if(unlist(regmatches(solis_battery_status.py.response[1], gregexpr("[[:digit:]]+", solis_battery_status.py.response[1])))[3]==35) { solis_battery_status.py.response[1] <- "on"} else { solis_battery_status.py.response[1] <- "off" }
solis_battery_charging_par.py.response <- character()
while(length(solis_battery_charging_par.py.response)!=2) { 
  Sys.sleep(5)
  solis_battery_charging_par.py.response <- system(paste0(solis_battery_charging_par.py, " ", charging.current), intern = T) 
}

# record battery status
solis_battery_status.py.response <- character()
while(length(solis_battery_status.py.response)!=3) { Sys.sleep(5); solis_battery_status.py.response <- system(paste0(solis_battery_status.py), intern = T) }
if(unlist(regmatches(solis_battery_status.py.response[1], gregexpr("[[:digit:]]+", solis_battery_status.py.response[1])))[3]==35) { solis_battery_status.py.response[1] <- "on"} else { solis_battery_status.py.response[1] <- "off" }
solis_battery_status.py.response[2] <- unlist(regmatches(solis_battery_status.py.response[2], gregexpr("[[:digit:]]+", solis_battery_status.py.response[2])))
solis_battery_status.py.response[3] <- gsub("Charge Time | |\\[|\\]", "", solis_battery_status.py.response[3])

# export state of charge
writeLines(text = solis_battery_status.py.response, con = "solis_battery_status.py.response.txt")
names(solis_battery_status.py.response) <- c("status", "current", "time")
solis_battery_status.py.response.df <- data.frame(status=solis_battery_status.py.response[1], current=solis_battery_status.py.response[2], time=solis_battery_status.py.response[3])
rownames(solis_battery_status.py.response.df) <- NULL

# export to home assistant sensors
#writeLines(text = solis_battery_status.py.response[1], con = "/media/solis_battery_status.txt")
#writeLines(text = solis_battery_status.py.response[2], con = "/media/solis_battery_current.txt")
#writeLines(text = solis_battery_status.py.response[3], con = "/media/solis_battery_time.txt")
#writeLines(text = as.character(charge.required), con = "/media/charge.required.txt")
#writeLines(text = as.character(best.guestimate), con = "/media/best.guestimate.txt")
