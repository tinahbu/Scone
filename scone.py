from fcntl import fcntl, F_GETFL, F_SETFL
from os import O_NONBLOCK

import Pyro4 as Pyro4
from threading import RLock
from subprocess import Popen, PIPE


@Pyro4.expose
class Scone(object):
    def __init__(self):
        self.lock = RLock()
        # redirect sbcl's output and input to PIPE, so we can send string to stdin and stdout, just like what we do in
        # cmd.
        # to enter input and outpt
        self.sbcl_process = Popen(['./sbcl/run-sbcl.sh'], stdout=PIPE, stdin=PIPE, stderr=PIPE)
        # get get current stdout flags
        flags = fcntl(self.sbcl_process.stdout, F_GETFL)
        # set stdout to non-blocking
        fcntl(self.sbcl_process.stdout, F_SETFL, flags | O_NONBLOCK)
        # skip all the output at the ver beginning
        while True:
            try:
                line = self.sbcl_process.stdout.readline()
            # IOError means we reach the end of file until now
            except IOError:
                break
            print(line)
        # set flag back to blocking
        fcntl(self.sbcl_process.stdout, F_SETFL, flags)
        print("sbcl completes init!")

    def write_input(self, my_input):
        # send command to sbcl
        # the reason for adding one more '\n' is that cmd need to determine whether a cmd has ended
        self.sbcl_process.stdin.write(my_input + "\n")

    def read_output(self):
        # read output from sbcl
        # the out put format for sbcl is:
        # \n
        # (result string)
        # so we need readline twice to get the second line(result string)
        self.sbcl_process.stdout.readline()
        return self.sbcl_process.stdout.readline().strip(" \n")

    def interface1(self):
        self.lock.acquire()
        self.write_input("123")
        str0 = self.read_output()
        print(str0)
        self.lock.release()

    def interface2(self):
        self.lock.acquire()
        print 456
        self.lock.release()

    def run(self):
        daemon = Pyro4.Daemon()
        ns = Pyro4.locateNS()
        uri = daemon.register(self)
        ns.register('scone', uri)
        daemon.requestLoop()
