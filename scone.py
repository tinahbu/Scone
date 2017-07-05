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
        res = self.read_output()
        if res.startswith(ERROR_MESSAGE):
            return None
        else:
            return res

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

    def create_software(self, new_software_name):
        # (new-type {Apache} {software resources})
        scone_input = "(new-type {%s} {software resources})" % new_software_name
        res = self.communicate(scone_input)
        if res is None:
            return -1
        return 0

    def add_software_dependencies(self, software_name, dependencies):
        res = []
        for dep_item in dependencies:
            # (new-statement {BLAS} {depends on} {Fortran})
            scone_input = "(new-statement {%s} {depends on} {%s})" % (software_name, dep_item)
            res = self.communicate(scone_input)
            if res is None:
                res.append(dep_item)
        return res

    def add_software_version(self, software_name, new_version):
        # (x-is-the-y-of-z (new-string {"1.55"}) {version of software resources} {Boost 1.55})
        scone_input = ('(x-is-the-y-of-z (new-string {%s}) {version of software resources} {%s %s})'
                       % (new_version, software_name, new_version))
        res = self.communicate(scone_input)
        if res is None:
            return -1
        else:
            return 0

    def create_task(self, new_task_name):
        scone_input = "(new-type {%s} {task})" % new_task_name
        res = self.communicate(scone_input)
        if res is None:
            return -1
        else:
            return 0

    def task_requires_software(self, task_name, software_name):
        scone_intput = '(new-statement {%s} {requires} (new-indv NIL {%s}))' % (task_name, software_name)
        res = self.communicate(scone_intput)
        if res is None:
            return -1
        else:
            return 0

    # def task_performed_by(self, task_name, user_name):

    def user_group_is_authorized_to_exec(self, user_group, softwares):
        for software in softwares:
            scone_input = "(new-statement {%s} {is authorized to execute} {%s})" % user_group, software
            if self.communicate(scone_input) is None:   # TODO: rollback all authorization of not?
                return -1
        return 0

    def assign_user_to_groups(self, user_name, group_names):  # only add group for now
        for group_name in group_names:
            scone_input = "(new-indv {%s} {%s})" % user_name, group_name
            if self.communicate(scone_input) is None:   # TODO: rollback all assignment of not?
                return -1
        return 0

    def create_user(self, user_name, user_id, user_email):
        scone_inputs = ["(new-type {%s} {default user})" % user_name,
                        "(x-is-the-y-of-z (new-string {\"{%s}\"}) {username of user} {%s})" % (user_name, user_name),
                        "(x-is-the-y-of-z (new-string {\"{%s}\"}) {email of user} {%s})" % (user_email, user_name),
                        "(x-is-the-y-of-z (new-string {\"{%s}\"}) {userid of user} {%s})" % (user_id, user_name)]
        for i, scone_input in enumerate(scone_inputs):
            if self.communicate(scone_input) is None:
                if i is 0:
                    return -1
                else:
                    self.communicate("(remove-element {%s})" % user_name)
        return 0

    def create_user_group(self, new_group_name):
        scone_input = "(new-type {%s} {user})" % new_group_name
        res = self.communicate(scone_input)
        if res is None:
            return -1
        return 0

    def check_user_can_use_software(self, user_name, software_name, version):
        scone_input = "(statement-true? {%s} {is authorized to execute} {%s})" % user_name, software_name + version
        res = self.communicate(scone_input)
        if res is None or res == "NIL":
            return False
        return True

    def run(self):
        daemon = Pyro4.Daemon()
        ns = Pyro4.locateNS()
        uri = daemon.register(self)
        ns.register('scone', uri)
        daemon.requestLoop()
