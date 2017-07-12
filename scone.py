from fcntl import fcntl, F_GETFL, F_SETFL
from os import O_NONBLOCK
from time import sleep
import Pyro4
from threading import RLock
from subprocess import Popen, PIPE

ERROR_MESSAGE = '\nERROR'  # we disable debug mode and hook it to a special sting


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
        # we define raw_write_input and write_input because, write_input is used to enter query, but raw_write_input is
        # used to enter command lines to sbcl without adding any addition string
        # this is debug ignore function, when the query went wrong, sbcl will return error message instead of entering
        # debug mode
        self.raw_write_input(
            "(defun debug-ignore (c h) (declare (ignore h))(declare (ignore c)) (print (format t \"~CERROR\" #\\linefeed)) (abort))")
        self.raw_write_input("(setf *debugger-hook* #'debug-ignore)")
        print("**********SBCL init ends**********")

        # begin loading knowledgebase
        print("**********Scone init begins**********")
        self.raw_write_input('(load "scone/scone-loader.lisp")')
        self.raw_write_input('(scone "")')
        self.raw_write_input('(load-kb "core")')
        sleep(3)
        # flush out the stdout pipe
        lines = self.read_output()
        for line in lines:
            print(line)
        print("**********Scone init ends**********")
        # set stdout back to blocking
        fcntl(self.sbcl_process.stdout, F_SETFL, flags)

    def raw_write_input(self, raw_input):
        # add one more "\n" to indicate current command line has ended
        self.sbcl_process.stdin.write(raw_input + "\n")

    def write_input(self, my_input):
        # the reason for adding one more '\n' is that cmd need to determine whether a cmd has ended
        # scone-call is a lisp-defined funtion that will print one more [PROMPT] that indicates current query has
        # returned all the result
        self.sbcl_process.stdin.write("(scone-call %s)\n" % my_input)

    def read_output(self):
        lines = []
        while True:
            try:
                line = self.sbcl_process.stdout.readline()
            except IOError:
                break
            if line.startswith('"FINISH"'):
                break
            # print line
            # When we trigger debugger, there will be no "FINISH" end, so we need to handle it seperately
            if line.startswith('ERROR'):
                lines.append(ERROR_MESSAGE)
                self.sbcl_process.stdout.readline()
                break
            if not line.startswith('*') and not line.startswith('\n'):
                lines.append(line.strip())
        return lines

    def communicate(self, my_input):
        self.lock.acquire()
        self.write_input(my_input)
        self.lock.release()
        res = self.read_output()
        if res[0].startswith(ERROR_MESSAGE):
            return None
        else:
            return res

    def interface1(self):
        self.lock.acquire()
        lines = self.communicate('(is-x-a-y? {operating system of Macbook_1} {Linux})')
        for line in lines:
            print line
        self.lock.release()

    def interface2(self):
        self.lock.acquire()
        print 456
        self.lock.release()

    """
    Create new_software_name with provided versions in version_list
        Create new_software_name if not exist
        Create new_software_name_version
        Return list of  new_software_name_version
    """
    def create_software(self, new_software_name, version_list=[]):
        rv = []
        scone_input = "(type-node? {%s})" % new_software_name
        res = self.communicate(scone_input)
        if res is None:
            return -1
        if res[0] == "NIL":
            scone_input = "(new-type {%s} {software resources})" % new_software_name
            res = self.communicate(scone_input)
            if res is None or res[0] != "{%s}" % new_software_name:
                return -1
        for version in version_list:
            scone_input = "(new-type {%s_%s} {%s})" % (new_software_name, version, new_software_name)
            res = self.communicate(scone_input)
            if res is None or res[0] != "{%s_%s}" % (new_software_name, version):
                return -1
            rv += [res[0].strip('{}')]
        return rv


    """
    Add an existing software's dependencies, if this software_name does not exist, return -1
    Return list of softwares in dependencies that does not exist in KB
    """
    def add_software_dependencies(self, software_name, dependencies=[]):
        rv = []
        scone_input = "(type-node? {%s})" % software_name
        res = self.communicate(scone_input)
        if res is None or res[0] == "NIL":
            return -1
        for dep_item in dependencies:
            scone_input = "(type-node? {%s})" % dep_item
            res = self.communicate(scone_input)
            if res is None:
                return -1
            if res[0] == "NIL":
                rv.append(dep_item)
            else:
                # (new-statement {BLAS} {depends on} {Fortran})
                scone_input = "(new-statement {%s} {depends on} {%s})" % (software_name, dep_item)
                res = self.communicate(scone_input)
        return rv

    def add_software_version(self, software_name, new_version):
        # (x-is-the-y-of-z (new-string {"1.55"}) {version of software resources} {Boost 1.55})
        scone_input = ('(x-is-the-y-of-z (new-string {%s}) {version of software resources} {%s %s})'
                       % (new_version, software_name, new_version))
        res = self.communicate(scone_input)
        if res is None:
            return -1
        else:
            return 0

    """
    Create a individual task from task type
    return -1 if task already exists
            0 if task created successfully
    """
    def user_create_task(self, new_task_name):
        # (new-indv {user 1} {user})
        scone_input = "(new-indv {%s} {task})" % new_task_name
        res = self.communicate(scone_input)
        if res is None:
            return -1
        else:
            return 0

    """
    Assign softwares to specific task
    return a list of softwares that does not exist now
           -1 if task does not exist
    """
    def user_task_requires_software(self, task_name, software_list):
        scone_input = "(indv-node? {%s})" % task_name
        res = self.communicate(scone_input)
        if res[0] != 'T':
            return -1
        nonexisted_software_list = []
        for software in software_list:
            scone_input = "(type-node? {%s})" % software
            res = self.communicate(scone_input)
            if res[0] != 'T':
                nonexisted_software_list.append(software)
            else:
                scone_input = '(new-statement {%s} {requires} {%s} )' % task_name, software
                self.communicate(scone_input)
        return nonexisted_software_list

    """
    
    """
    def task_performed_by(self, task_name, user_name):
        #  access_check (user task)
        scone_input = '(access_check ({%s} {%s}))' % (task_name, user_name)
        res = self.communicate(scone_input)
        if res is None:
            return -1
        return res

    # User is authorized to exec, grant_auth()
    def user_group_is_authorized_to_exec(self, user_group, softwares):
        for software in softwares:
            scone_input = "(new-statement {%s} {is authorized to execute} {%s})" % user_group, software
            if self.communicate(scone_input) is None:  # TODO: rollback all authorization of not?
                return -1
        return 0

    def assign_user_to_groups(self, user_name, group_names):  # only add group for now
        for group_name in group_names:
            scone_input = "(new-indv {%s} {%s})" % user_name, group_name
            if self.communicate(scone_input) is None:  # TODO: rollback all assignment of not?
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
        # check user name, soft, version
        if res is None or res == "NIL":
            return False
        return True

    def check_vulnerability(self, target, software_name, version=None, compare=None):
        if target != 'user' and target != 'task' and target != 'software':
            return -1
        if version is None:
            scone_input = "(type-node? {%s})" % software_name
            res = self.communicate(scone_input)
            if res != 'T':
                return -1
            # (user_check_vulnerability {OpenSSL})
            scone_input = '(user_check_vulnerability {%s})' % software_name
            return scone_input
        else:
            if compare is None:
                return -1
            elif compare != 'equal' and compare != 'newer' and compare != 'older':
                return -1
            else:
                # (user_check_vulnerability_newer {python} "2.7")
                scone_input = "(type-node? {%s})" % software_name + "_" + version
                res = self.communicate(scone_input)
                if res != 'T':
                    return -1
                scone_input = '(%s_check_vulnerability_%s {%s} "%s")' % target, compare, software_name, version
                res = self.communicate(scone_input)
                return list(set(res))

    def check_access(self, user_name, task):
        scone_input = "(type-node? {%s})" % user_name
        res = self.communicate(scone_input)
        if res != 'T':
            return -1
        scone_input = "(indv-node? {%s})" % task
        res = self.communicate(scone_input)
        if res != 'T':
            return -1
        scone_input = '(access_check {%s} {%s})' % user_name, task
        res = self.communicate(scone_input)
        return list(set(res))

    def run(self):
        daemon = Pyro4.Daemon()
        ns = Pyro4.locateNS()
        uri = daemon.register(self)
        ns.register('scone', uri)
        daemon.requestLoop()

    def test(self):
        print self.communicate("")
