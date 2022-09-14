from pysolarmanv5.pysolarmanv5 import PySolarmanV5

def main():
   solis = PySolarmanV5('INVERTER_IP', DATA_LOGGER_SN)
   solis.write_holding_register(register_addr=43110, value=33) 
   energy_storage_status = solis.read_holding_register_formatted(register_addr=43110, quantity=1)
   print("Battery Charging from Grid Set to Off / Energy Storage Status", energy_storage_status)

if __name__ == "__main__":
   main()

