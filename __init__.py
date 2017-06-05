# import os
# os.system('./sbcl/run-sbcl.sh')

import multiprocessing
from time import sleep

semaphore = multiprocessing.Semaphore(1)

def scone(my_id):
    with semaphore:
        sleep(1)
    print("Hello, I am Scone\n")


def state_modifier(my_id):
    with semaphore:
        sleep(1)
    print("Hello, I am state_modifier\n")


def state_inquirer(my_id):
    with semaphore:
        sleep(1)
    print("Hello, I am state_inquirer\n")


def main():
    pool = multiprocessing.Pool(3)

    pool.apply_async(scone, [0])
    pool.apply_async(state_modifier, [1])
    pool.apply_async(state_inquirer, [2])

    pool.close()
    pool.join()


if __name__ == "__main__":
    main()
