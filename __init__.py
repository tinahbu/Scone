import time, os
from multiprocessing import Process
from state_modifier import StateModifier
from state_inquirer import StateInquirer
from scone import Scone


def run_scone():
    Scone().run()   # run scone service


def run_state_modifier():
    StateModifier().run()


def run_state_inquirer():
    StateInquirer().run()


def bootstrap_ns():
    os.system('pyro4-ns')   # start name server


def main():
    Process(target=bootstrap_ns).start()
    Process(target=run_scone).start()
    time.sleep(5)  # wait enough time to make sure scone service is registered
    Process(target=run_state_modifier).start()
    # Process(target=run_state_inquirer).start()


if __name__ == "__main__":
    main()
