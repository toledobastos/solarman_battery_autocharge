from pysolarmanv5.pysolarmanv5 import PySolarmanV5

def main():
   solis = PySolarmanV5('192.168.1.32', 4056057302)
   energy_generated_yesterday = solis.read_input_registers(register_addr=33036, quantity=1)
   print(energy_generated_yesterday)

if __name__ == "__main__":
   main()
