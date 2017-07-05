from fcntl import fcntl, F_GETFL, F_SETFL
from os import O_NONBLOCK
from time import sleep
import Pyro4 as Pyro4
from threading import RLock
from subprocess import Popen, PIPE

ERROR_MESSAGE = "\nERROR"   # we disable debug mode and hook it to a special sting

@Pyro4.expose
class Scone(object):
    def __init__(self):
        self.lock = RLock()
        # redirect sbcl's output and input to PIPE, so we can send string to stdin and stdout, just like what we do in
        # cmd.
        # to enter input and outpt
        self.sbcl_process = Popen(['./sbcl/run-sbcl.sh'], stdout=PIPE, stdin=PIPE, stderr=PIPE, shell=True)
        sleep(2)
        # get get current stdout flags
        flags = fcntl(self.sbcl_process.stdout, F_GETFL)
        # set stdout to non-blocking
        fcntl(self.sbcl_process.stdout, F_SETFL, flags | O_NONBLOCK)
        # skip all the output at the ver beginning
        print("**********SBCL init begins**********")
        self.write_input("(defun debug-ignore (c h) (declare (ignore h))(declare (ignore c)) (print (format t \"~CERROR\" #\\linefeed)) (abort))")
        self.write_input("(setf *debugger-hook* #'debug-ignore)")
        lines = self.read_output()
        for line in lines:
            print(line)
        print("**********SBCL init ends**********")
        print('')
        print("**********Scone init begins**********")
        self.write_input('(load "scone/scone-loader.lisp")')
        self.write_input('(scone "")')
        self.write_input('(load-kb "core")')
        # TODO Need to find a better way to determine whether we reach the end
        sleep(2)
        lines = self.read_output()
        for line in lines:
            print(line)
        print("**********Scone init ends**********")

    def write_input(self, my_input):
        # send command to sbcl
        # the reason for adding one more '\n' is that cmd need to determine whether a cmd has ended
        self.sbcl_process.stdin.write(my_input + "\n")

    def read_output(self):
        lines = []
        while True:
            try:
                line = self.sbcl_process.stdout.readline()
            # IOError means we reach the end of file until now
            except IOError:
                break
            lines.append(line)
        return lines

    def communicate(self, my_input):
        self.lock.acquire()
        self.write_input(my_input)
        self.lock.release()
        sleep(5)
        return self.read_output().startswith(ERROR_MESSAGE)

    def interface1(self):
        self.lock.acquire()
        self.write_input('(is-x-a-y? {operating system of Macbook_1} {Linux})')
        # TODO
        sleep(2)
        lines = self.read_output()
        for line in lines:
            print(line)
        self.lock.release()

    def interface2(self):
        self.lock.acquire()
        print 456
        self.lock.release()

    def create_user_group(self, new_group_name):
        return self.communicate("(new-type {" + new_group_name + "} {user})")

    def kill_sbcl(self, signum, frame):
        self.sbcl_process.kill()

    def run(self):
        daemon = Pyro4.Daemon()
        ns = Pyro4.locateNS()
        uri = daemon.register(self)
        ns.register('scone', uri)
        daemon.requestLoop()
