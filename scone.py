from fcntl import fcntl, F_GETFL, F_SETFL
from os import O_NONBLOCK
from time import sleep
import Pyro4
from threading import RLock
from subprocess import Popen, PIPE
import re

ERROR_MESSAGE = '\nERROR'  # we disable debug mode and hook it to a special sting

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

# print bcolors.WARNING + "Warning: No active frommets remain. Continue?" + bcolors.ENDC

@Pyro4.expose
class Scone(object):
    def __init__(self):
        self.vulnerabilities = []   # remember all inputted vulnerabilities
        self.cve = []

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
            print my_input
            return None
        else:
            return res

    """
    Create new_software_name with provided versions in version_list
        Create new_software_name if not exist
        Create new_software_name_version
        Return list of  new_software_name_version if success
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

            new_software_name_version = res[0].strip('{}')

            scone_input = "(new-indv NIL {%s_%s})" % (new_software_name, version)
            res = self.communicate(scone_input)
            if res is None:
                return -1

            # add version info
            scone_input = ('(x-is-the-y-of-z (new-string {"%s"}) {version of software resources} {%s_%s})'
                           % (version, new_software_name, version))
            res = self.communicate(scone_input)
            if res is None:
                return -1
            rv += [[new_software_name_version, self.check_all_vulnerability("software", new_software_name_version)]]
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

    """
    Add a new version to an existing software, if the software does not exist, return -1
    If success, return 0, if the new_version is already in KB, return 1
    """

    def add_software_version(self, software_name, new_version):
        # (x-is-the-y-of-z (new-string {"1.55"}) {version of software resources} {Boost 1.55})
        scone_input = "(type-node? {%s})" % software_name
        res = self.communicate(scone_input)
        if res is None or res[0] == "NIL":
            return -1

        if self.create_software(software_name, [new_version]) == -1:
            return 1
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
    Assign software to specific task
    return a list of software that does not exist now
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
                # (new-statement {CNN for product recommendation} {requires software} (new-indv NIL {Python_3.0}))
                scone_input = '(new-statement {%s} {requires software} (new-indv NIL {%s}) )' % (task_name, software)
                self.communicate(scone_input)
        v = set()
        if 1 in self.cve and task_name in self.cve_check_1():
            v.add(1)
        if 2 in self.cve and task_name in self.cve_check_2():
            v.add(2)
        return nonexisted_software_list, v

    def add_cve(self, f):
        self.cve += [f]

    # for CVE check when adding software to user task
    def cve_check_1(self):
        s1 = set(self.check_vulnerability('task', 'httplib'))
        s2 = set(self.check_vulnerability('task', 'urllib'))
        s3 = set(self.check_vulnerability('task', 'urllib2'))
        s4 = set(self.check_vulnerability('task', 'xmlrpclib'))
        s5 = set(self.check_vulnerability('task', 'python', '1.9', 'newer'))
        s6 = set(self.check_vulnerability('task', 'python', '2.8', 'older'))
        s7 = set(self.check_vulnerability('task', 'python', '2.9', 'newer'))
        s8 = set(self.check_vulnerability('task', 'python', '3.5', 'older'))
        return (s1 | s2 | s3 | s4) & ((s5 & s6) | (s7 & s8))

    def cve_check_2(self):
        s1 = set(self.check_vulnerability('task', 'Oracle Outside In Technology'))
        s2 = set(self.check_vulnerability('task', 'Oracle Fusion Middleware', '8.4.9', 'newer'))
        s3 = set(self.check_vulnerability('task', 'Oracle Fusion Middleware', '8.5.3', 'older'))
        return s1 & (s2 | s3)

    """
    Assign hardware to specific task
    return 0, if succeeds
           -1, if task does not exist
           -2, if processor does not exist
    """

    def user_task_requires_hardware(self, task_name, processor_full_name):
        scone_input = "(indv-node? {%s})" % task_name
        res = self.communicate(scone_input)
        if res[0] != 'T':
            return -1
        scone_input = "(type-node? {%s})" % processor_full_name
        res = self.communicate(scone_input)
        if res[0] != 'T':
            return -2
        scone_input = '(new-statement {%s} {requires processor} (new-indv NIL {%s}) )' % (task_name, processor_full_name)
        self.communicate(scone_input)
        return 0

    """
    Set task's performer to a user
    return the list of softwares that needs user first gain authorization of it
           -1, if task or user does not exist
    """

    def user_task_performed_by(self, task_name, user_name):
        scone_input = "(indv-node? {%s})" % task_name
        res = self.communicate(scone_input)
        if res[0] != 'T':
            return -1

        scone_input = "(indv-node? {%s})" % user_name
        res = self.communicate(scone_input)
        if res[0] != 'T':
            return -2

        tmp = 0
        scone_input = '(task_check_user_CPU {%s} {%s})' \
                      % (user_name, task_name)
        res = self.communicate(scone_input)
        if res is None or res[0] == "NIL":
            tmp = -3

        scone_input = '(task_check_user_GPU {%s} {%s})' \
                      % (user_name, task_name)
        res = self.communicate(scone_input)
        if res is None or res[0] == "NIL":
            tmp = -4

        # access_check (user task)
        scone_input = '(access_check {%s} {%s})' % (user_name, task_name)
        # Not debugged
        res = self.communicate(scone_input)
        if res is None:
            return -1
        if len(res) == 1:
            scone_input = "(new-statement {%s} {is performing} {%s})" % (user_name, task_name)
            self.communicate(scone_input)
            return tmp, 0
        else:
            return tmp, res

    """
    Create a {is authorized to execute} relation between user group and list of softwares
    return -1 if failed
            0 if success
    """
    # User is authorized to exec, grant_auth()
    def user_group_is_authorized_to_exec(self, user_group, softwares=[]):
        for software in softwares:
            scone_input = "(new-statement {%s} {is authorized to execute} {%s})" % (user_group, software)
            if self.communicate(scone_input) is None:  # TODO: rollback all authorization of not?
                return -1
        return 0

    """
    Create a {is authorized to execute} relation between user and list of softwares
    return -1 if failed
            0 if success
    """
    def user_is_authorized_to_exec(self, user, softwares=[]):
        for software in softwares:
            scone_input = "(new-statement {%s} {is authorized to execute} {%s})" % (user, software)
            if self.communicate(scone_input) is None:  # TODO: rollback all authorization of not?
                return -1
        return 0

    '''
    Assign a user to a list of groups
    return 0, if assign successfully to all of groups
           the list of groups that have not existed
    '''
    def assign_user_to_groups(self, user_name,
                              group_names=[]):  # only add group for nowres = self.communicate(scone_input)
        scone_input = "(indv-node? {%s})" % user_name
        res = self.communicate(scone_input)
        if res[0] != 'T':
            return -1
        nonexisted_group_list = []
        for group_name in group_names:
            scone_input = "(type-node? {%s})" % group_name
            res = self.communicate(scone_input)
            if res[0] != 'T':
                nonexisted_group_list.append(group_name)
                continue
            scone_input = "(x-is-a-y-of-z {%s} {member of user} {%s})" % (user_name, group_name)
            self.communicate(scone_input)

        return nonexisted_group_list

    """
    create new user, assign it to the user_group if provided (or to the default user),
    return -1, if the provided user_group does not exist
           -2, if operating system does not exist
           -3, if processor does not exist
           1,  if the user is already in the KB
           0,  if create successfully
    """
    def create_user(self, user_name, user_id, user_email,
                    user_processor_full_name,
                    group_name="default user",):
        scone_input = "(indv-node? {%s})" % user_name
        res = self.communicate(scone_input)
        if res is None:
            return -1
        if res[0] != "NIL":
            return 1

        # scone_input = "(type-node? {%s})" % user_os_full_name
        # res = self.communicate(scone_input)
        # if res is None or res[0] == "NIL":
        #     return -2

        scone_input = "(type-node? {%s})" % user_processor_full_name
        res = self.communicate(scone_input)
        if res is None or res[0] == "NIL":
            return -3
        # (x-is-the-y-of-z (new-indv NIL {MacOS_10.6}) {os of user} {user 3})
        # (x-is-a-y-of-z (new-indv NIL {Intel Core CPU_i5}) {processor of user} {user 6})
        scone_inputs = ["(new-indv {%s} {default user})" % user_name,
                        "(x-is-the-y-of-z (new-string {\"{%s}\"}) {username of user} {%s})" % (user_name, user_name),
                        "(x-is-the-y-of-z (new-string {\"{%s}\"}) {email of user} {%s})" % (user_email, user_name),
                        "(x-is-the-y-of-z (new-string {\"{%s}\"}) {userid of user} {%s})" % (user_id, user_name),
                        # "(x-is-the-y-of-z (new-indv NIL {%s}) {os of user} {%s})" % (user_os_full_name, user_name),
                        "(x-is-the-y-of-z (new-indv NIL {%s}) {processor of user} {%s})" % (user_processor_full_name, user_name),
                        "(x-is-a-y-of-z {%s} {member of user} {%s})" % (user_name, group_name)]
        for i, scone_input in enumerate(scone_inputs):
            if self.communicate(scone_input) is None:
                if i is 0:
                    return -1
                else:
                    self.communicate("(remove-element {%s})" % user_name)
        return 0

    """
    Create a new user group, failed return -1
    """
    def create_user_group(self, new_group_name):
        scone_input = "(new-type {%s} {user})" % new_group_name
        res = self.communicate(scone_input)
        if res is None:
            return -1
        return 0

    '''
    Check whether a user is authorized to execute that software maybe with specific version
    return True when user is authorized
           False when user is not authorized
           -1 when user does not exist or software does not exist
    '''
    def check_user_can_use_software(self, user_name, software_name, version=""):
        # (authorized_to_use? {user 2} {Apache})
        # Check whether user exists
        scone_input = "(indv-node? {%s})" % user_name
        res = self.communicate(scone_input)
        if res[0] != 'T':
            return -1
        # Check whether specific version of a software exists
        if len(version) == 0:
            scone_input = "(type-node? {%s})" % software_name
        else:
            scone_input = "(type-node? {%s})" % (software_name + "_" + version)
        res = self.communicate(scone_input)
        if res is None or res[0] == "NIL":
            return -1

        if len(version) == 0:
            scone_input = '(authorized_to_use? {%s} {%s})' % (user_name, software_name)
        else:
            scone_input = '(authorized_to_use? {%s} {%s})' % (user_name, software_name + "_" + version)
        res = self.communicate(scone_input)
        print res
        if res[0] == 'NIL':
            return False
        else:
            return True
        # continue checking user's group's access

    """
    Add new vulnerability (rule) into of our detection system
    """
    def add_software_vulnerability(self, software_name, version=None, compare=None):
        self.vulnerabilities += [[software_name, version, compare]]

    """
    return the vul at index as a [software_name, version, compare] list
    """
    def get_software_vulnerability(self, index):
        if index < 0 or index >= len(self.vulnerabilities):
            return -1
        return self.vulnerabilities[index]

    """
    Examining already added vulnerabilities for the given newly added software/task/user
    target is either software/task/user, item is the name of the software/task/user,
    note that software may include version in the string representation

    return all indexes of vulnerability that will affect the item
    """
    def check_all_vulnerability(self, target, item):
        r = []
        for i, [software_name, version, compare] in enumerate(self.vulnerabilities):
            res = self.check_vulnerability(target, software_name, version, compare)
            # print res, target, software_name, version, compare
            if res != [] and (item in res):
                r += [i]
        return r

    """
    CLI should call this one, this will remember the added vulnerability
    """
    def check_vulnerability_and_add_it(self, target, software_name=None, version=None, compare=None):
        res = self.check_vulnerability(target, software_name, version, compare)
        self.add_software_vulnerability(software_name, version, compare)
        return res

    """
    Check vulnerability for user/task/software given the reported software_name
    Return list of affected user/task/software
    User compare to specify software version range that is affected
    This will also add the new vulnerability into the vulnerability knowledge base
    """
    def check_vulnerability(self, target, software_name, version=None, compare=None):
        if target != 'user' and target != 'task' and target != 'software':
            return []
        if version is None:
            # scone_input = "(type-node? {%s})" % software_name
            # res = self.communicate(scone_input)
            # if res[0] != 'T':
            #     return -1
            # (user_check_vulnerability {OpenSSL})
            scone_input = '(%s_check_vulnerability {%s})' % (target, software_name)
            res = self.communicate(scone_input)
            if target in ['user', 'task']:
                return list(set(map(lambda x: x[1:-1], res[:-1])))
            return list(set(map(lambda x: ' '.join(re.split('\s|\{|\}', x)[1:-2]), res[:-1])))
        elif compare != 'equal' and compare != 'newer' and compare != 'older':
            return []
        else:
            # (user_check_vulnerability_newer {python} "2.7")
            # scone_input = "(type-node? {%s})" % (software_name + "_" + version)
            # res = self.communicate(scone_input)
            # if res[0] != 'T':
            #     return []
            scone_input = '(%s_check_vulnerability_%s {%s} "%s")' % (target, compare, software_name, version)
            res = self.communicate(scone_input)
            if target in ['user', 'task']:
                return list(set(map(lambda x: x[1:-1], res[:-1])))
            if compare == 'equal':
                return list(set(map(lambda x: ' '.join(re.split('\s|\{|\}', x)[1:-2]), res[:-1]))) + [software_name + "_" + version]
            return list(set(map(lambda x: ' '.join(re.split('\s|\{|\}', x)[1:-2]), res[:-1])))

    """
    Given a user and a task, print a list of softwares that
    the user is not yet authorized to execute.
    """
    def check_access(self, user_name, task):
        scone_input = "(indv-node? {%s})" % user_name
        res = self.communicate(scone_input)
        if res[0] != 'T':
            return -1
        scone_input = "(indv-node? {%s})" % task
        res = self.communicate(scone_input)
        if res[0] != 'T':
            return -1

        scone_input = '(access_check {%s} {%s})' % (user_name, task)
        res = self.communicate(scone_input)
        return list(set(map(lambda x : ' '.join(re.split('\s|\{|\}', x)[1:-2]), res[:-1])))

    def run(self):
        daemon = Pyro4.Daemon()
        ns = Pyro4.locateNS()
        uri = daemon.register(self)
        ns.register('scone', uri)
        daemon.requestLoop()

    def test(self):
        print self.communicate("")

    '''
        Create a new gpu given a existed cpu brand and a new cpu version
        return -1 if this new version already exists
               -2 if brand name does not exist
               0  if creates successfully
    '''
    def create_cpu(self, brand_name, new_cpu_version):
        cpu_full_name = brand_name + "_" + new_cpu_version
        scone_input = "(type-node? {%s})" % cpu_full_name
        res = self.communicate(scone_input)
        if res is None or res[0] == "T":
            return -1

        scone_input = "(type-node? {%s})" % brand_name
        res = self.communicate(scone_input)
        if res is None or res[0] == "NIL":
            return -2

        scone_input = "(new-type {%s} {%s})" % (cpu_full_name, brand_name)
        self.communicate(scone_input)

        # (x-is-the-y-of-z (new-string {"3"}) {version of hardware resources} {Intel Core CPU_i3})
        scone_input = '(x-is-the-y-of-z (new-string {"%s"}) {version of hardware resources} {%s})'\
                      % (new_cpu_version, cpu_full_name)
        self.communicate(scone_input)
        return 0

    '''
        Create a new gpu given a existed cpu brand and a new cpu version
        return -1 if this new version already exists
               -2 if brand name does not exist
               0  if creates successfully
    '''
    def create_gpu(self, brand_name, new_gpu_version):
        gpu_full_name = brand_name + "_" + new_gpu_version
        scone_input = "(type-node? {%s})" % gpu_full_name
        res = self.communicate(scone_input)
        if res is None or res[0] == "T":
            return -1

        scone_input = "(type-node? {%s})" % brand_name
        res = self.communicate(scone_input)
        if res is None or res[0] == "NIL":
            return -2

        scone_input = "(new-type {%s} {%s})" % (gpu_full_name, brand_name)
        self.communicate(scone_input)

        scone_input = '(x-is-the-y-of-z (new-string {"%s"}) {version of hardware resources} {%s})' \
                      % (new_gpu_version, gpu_full_name)
        self.communicate(scone_input)
        return 0

    '''
        Create a new os given a existed cpu brand and a new cpu version
        return -1 if this new version already exists
               -2 if brand name does not exist
               0  if creates successfully
    '''
    def create_os(self, brand_of_os, new_version_of_os):
        os_full_name = brand_of_os + "_" + new_version_of_os
        scone_input = "(type-node? {%s})" % os_full_name
        res = self.communicate(scone_input)
        if res is None or res[0] == "T":
            return -1

        scone_input = "(type-node? {%s})" % brand_of_os
        res = self.communicate(scone_input)
        if res is None or res[0] == "NIL":
            return -2

        scone_input = "(new-type {%s} {%s})" % (os_full_name, brand_of_os)
        self.communicate(scone_input)

        # (x-is-the-y-of-z (new-string {"10.2"}) {version of operating system} {MacOS_10.2})
        scone_input = '(x-is-the-y-of-z (new-string {"%s"}) {version of operating system} {%s})' \
                      % (new_version_of_os, os_full_name)
        self.communicate(scone_input)
        return 0

    '''
    Check whether user have enough hardware resources to perform this task
    return 1,  if user can
           0,  if user cannot
           -1, if hardware_type is invalid
           -2, if task does not exist
           -3, if user does not exist
    '''
    # (task_check_user_CPU {user 6} {VR Game Development})
    def task_check_user_hardware(self, hardware_type, task_name, user_name):
        if hardware_type != 'CPU' and hardware_type != 'GPU':
            return -1
        scone_input = "(indv-node? {%s})" % task_name
        res = self.communicate(scone_input)
        if res[0] != 'T':
            return -2
        scone_input = "(indv-node? {%s})" % user_name
        res = self.communicate(scone_input)
        if res[0] != 'T':
            return -3
        # task_check_user_CPU
        scone_input = '(task_check_user_%s {%s} {%s})' \
                      % (hardware_type, user_name, task_name)
        res = self.communicate(scone_input)
        if res is None or res[0] == "NIL":
            return 0
        else:
            return 1