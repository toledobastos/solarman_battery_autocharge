# solarman_battery_autocharge - solar prediction and battery autocharge for pysolarmanv5

The code uses R and Python. Python handles the calls to the Solis modbus, made possible via TCP through an open port, which was developed and documented in the pysolarmanv5 library. 

The R script autoChargePylontech.R queries the forecast.solar, solcast, and met.eirean (only available in Ireland) APIs for your location (you need to sign up for a Solcast API—free of charge much like forecast.solar), takes an average for the day, and triggers the necessary charge of the battery based on charging time (e.g., 01-09AM) and current, which will vary depending on how much charge is required to get through the day and of course the size and max discharge rate of your battery. 

Once the paramenters are set and the script is running correctly, you may want to run it as a cronjob everyday 1 min past midnight, or earlier if you prefer (the script can detect if you want forecasts for tomorrow or today) with charging starting at 1AM. Obviously you could use Task Scheduler if you’re running this on a Windows machine or Automator on MacOS. 

Dependencies for Python are pysolarmanv5, sys, and time. Make sure you can import these libraries in Python or install them using pip. You will also have to edit the py files to include your inverter IP address and data logger serial number. If the registers for battery read and write are different these will have to be amended too. Check the documentation of RS485_MODBUS RTU for details.  

Dependencies in R are jsonlite, lubridate, dplyr, rvest, and xml2. The R script will install dependencies if these are not found in your system. 

-- mtb
