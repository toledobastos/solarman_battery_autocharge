import sys
import time
import pysolarmanv5

# total arguments
n = len(sys.argv)
print("Total arguments passed:", n)

from pysolarmanv5.pysolarmanv5 import PySolarmanV5

def main():
   solis = PySolarmanV5('192.168.1.32', 4056057302)
   solis.write_holding_register(register_addr=43141, value=int(sys.argv[1])) 
   energy_storage_status = solis.read_holding_register_formatted(register_addr=43141, quantity=1)
   print("Battery charging current changed / Energy Storage Status", energy_storage_status)
if __name__ == "__main__":
   main()

