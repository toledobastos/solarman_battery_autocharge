import sys
import time
import pysolarmanv5
 
# total arguments
n = len(sys.argv)
print("Total arguments passed:", n)
 
# Arguments passed
print("\nArgument 1:", sys.argv[1])
print("\nArgument 2:", sys.argv[2])
print("\nArgument 3:", sys.argv[3])
print("\nArgument 4:", sys.argv[4])

from pysolarmanv5.pysolarmanv5 import PySolarmanV5

def main():
   solis = PySolarmanV5('192.168.1.32', 4056057302)
   print("Adjusting start of charging schedule")
   solis.write_holding_register(register_addr=43143, value=int(sys.argv[1]))
   time.sleep(5)
   solis.write_holding_register(register_addr=43144, value=int(sys.argv[2]))
   time.sleep(5)
   print("Adjusting end of charging schedule")
   solis.write_holding_register(register_addr=43145, value=int(sys.argv[3]))
   time.sleep(5)
   solis.write_holding_register(register_addr=43146, value=int(sys.argv[4]))
   time.sleep(5)
   energy_storage_status = solis.read_holding_register_formatted(register_addr=43110, quantity=1)
   print("Battery Charging from Grid Set to On / Energy Storage Status", energy_storage_status)

if __name__ == "__main__":
   main()

