from pysolarmanv5.pysolarmanv5 import PySolarmanV5

def main():
   solis = PySolarmanV5('INVERTER_IP', DATA_LOGGER_SN)
   energy_storage_status = solis.read_holding_registers(register_addr=43110, quantity=1)
   print("Energy Storage Status (35=on 33=off)", energy_storage_status)

   timed_charge_current = solis.read_holding_registers(register_addr=43141, quantity=1)
   print("Charge Limit", timed_charge_current)

   timed_charge_starthour = solis.read_holding_registers(register_addr=43143, quantity=1)
   timed_charge_startmin = solis.read_holding_registers(register_addr=43144, quantity=1)
   timed_charge_endhour = solis.read_holding_registers(register_addr=43145, quantity=1)
   timed_charge_endmin = solis.read_holding_registers(register_addr=43146, quantity=1)
   print("Charge Time", timed_charge_starthour, ":", timed_charge_startmin, "-", timed_charge_endhour,":", timed_charge_endmin)

if __name__ == "__main__":
   main()

