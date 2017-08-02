from time import sleep
from multiprocessing import Process

import Pyro4

from state_modifier import StateModifier
from state_inquirer import StateInquirer
from scone import Scone
import os


def run_scone():
    Scone().run()  # run scone service


def run_state_modifier():
    StateModifier().run()


def run_state_inquirer():
    StateInquirer().run()


def bootstrap_ns():
    os.system('pyro4-ns')  # start name server


def main():
    Process(target=bootstrap_ns).start()
    Process(target=run_scone).start()
    sleep(5)  # wait enough time to make sure scone service is registered
    Process(target=run_state_modifier).start()
    Process(target=run_state_inquirer).start()
    sleep(10)
    print "begin terminal"
    SCONE = Pyro4.Proxy('PYRONAME:scone')
    while True:
        print 'Please select the function you want to use (enter number):'
        print '1:  Create a new software that installed on current machine'
        print "2:  Set a existed software's dependencies"
        print "3:  Install a software with more version"
        print "4:  Create a new task"
        print "5:  Set the software requirements of a existed task"
        print "6:  Let a user perform an existed task"
        print "7:  Authorize user group to use an existed software"
        print "8:  Assign user to existed groups"
        print "9:  Create a new user"
        print "10: Create a new user group"
        print "11: Check if user can execute this software"
        print "12: Check if some target has this vulnerability"
        print "13: Show details of added vulnerability in the KB"  # fake, stored in python engine
        str_input = raw_input()
        if not str_input.isdigit():
            print "Invalid input! Please try again"
            continue
        user_input = int(str_input)
        if user_input < 1 or user_input > 13:
            print "Invalid input! Please try again"
            continue
        if user_input == 1:
            print "Please enter your new software name: "
            new_software_name = raw_input()
            print "Please enter the list of specific software version (end with single return):"
            version_list = []
            while True:
                new_version = raw_input()
                if len(new_version) == 0:
                    break
                version_list.append(new_version)
            res = SCONE.create_software(new_software_name, version_list)
            if res == -1:
                print "This software already exists"
            else:
                print "Those software and version creation succeeds: "
                for r in res:
                    print r[0]
                    if r[1]:
                        print "ALERT: See vulnerability #" + ', '.join(map(str, r[1])) + ' for why the above newly added software has vulnerability'
        elif user_input == 2:
            print "Please enter the software name:"
            software_name = raw_input()
            print "Please enter the list of existed software that is depended by above software (ends with single return):"
            software_list = []
            while True:
                new_existed_software = raw_input()
                if len(new_existed_software) == 0:
                    break
                software_list.append(new_existed_software)
            res = SCONE.add_software_dependencies(software_name, software_list)
            if len(res) == 0:
                print "add successfully"
            else:
                print "Those software does not exist: " + ", ".join(res)
        elif user_input == 3:
            print "Please enter the existed software name:"
            software_name = raw_input()
            print "Please list a new version that will be installed on the system:"
            new_version = raw_input()
            res = SCONE.add_software_version(software_name, new_version)
            if res == 0:
                print "Versions are set successfully"
            elif res == 1:
                print "This version is already installed"
            else:
                print "Software does not exist"
        elif user_input == 4:
            print "Please enter the new task name:"
            new_task_name = raw_input()
            res = SCONE.user_create_task(new_task_name)
            if res == -1:
                print "This task already exists"
            else:
                print "task created successfully"
        elif user_input == 5:
            # user_task_requires_software
            print "Please enter the existed task name:"
            task_name = raw_input()
            print "Please enter the list of software that is needed by this task (ends with single return):"
            software_list = []
            while True:
                software_name = raw_input()
                if len(software_name) == 0:
                    break
                software_list.append(software_name)
            res = SCONE.user_task_requires_software(task_name, software_list)
            if res == -1:
                print "Task does not exist yet"
            elif len(res) > 0:
                print "Those software do not exist now:"
                print " ,".join(res)
            else:
                print "Requirements are added successfully"
        elif user_input == 6:
            # user_task_performed_by
            print "Please enter the name of user who wants to perform a task:"
            user_name = raw_input()
            print "Please enter the task that user wants to perform"
            task_name = raw_input()
            res = SCONE.user_task_performed_by(task_name, user_name)
            if res == -1:
                print "User or task does not exist"
            elif len(res) != 0:
                print "User has to first gain those software's authorities to perform this task: "
                print ", ".join(res)
        elif user_input == 7:
            # user_group_is_authorized_to_exec
            print "Please enter the intended user group's name:"
            group_name = raw_input()
            print "Please enter the list of software that authorizes user group to execute (ends with single return):"
            software_list = []
            while True:
                software_name = raw_input()
                if len(software_name) == 0:
                    break
                software_list.append(software_name)
            res = SCONE.user_group_is_authorized_to_exec(group_name, software_list)
            if res == -1:
                print "Group or user does not exist"
            else:
                print "Authorization succeeds"
        elif user_input == 8:
            # assign_user_to_groups
            print "Please enter intended user's name:"
            user_name = raw_input()
            print "Please enter the list of target groups (ends with return):"
            group_list = []
            while True:
                group_name = raw_input()
                if len(group_name) == 0:
                    break
                group_list.append(group_name)
            res = SCONE.assign_user_to_groups(user_name, group_list)
            if res != 0:
                print "Those groups do not exist: "
                print " ,".join(res)
            else:
                print "Assigning succeeds"
        elif user_input == 9:
            # create_user
            print "Please enter a new user name:"
            new_user_name = raw_input()
            print "Please enter a user id:"
            user_id = raw_input()
            print "Please enter a user email:"
            user_email = raw_input()
            print "Please assign a user group:"
            group_name = raw_input()
            print "Please enter an existed operating system:"
            os_name = raw_input()
            print "Please enter an existed processor:"
            processor_name = raw_input()
            res = SCONE.create_user(new_user_name, user_id, user_email, os_name, processor_name, group_name)
            if res == 1:
                print "User already exists"
            elif res == 0:
                print "User create succeeds"
            elif res == -2:
                print "operating system does not exist"
            elif res == 3:
                print "processor does not exist"
            else:
                print "Group does not exist"
        elif user_input == 10:
            # create_user_group
            print "Please enter the name of group that you want to create:"
            new_group_name = raw_input()
            res = SCONE.create_user_group(new_group_name)
            if res == -1:
                print "Group already exists"
            else:
                print "Group create succeeds"
        elif user_input == 11:
            # check_user_can_use_software
            print "Please enter user name:"
            user_name = raw_input()
            print "Please enter software name:"
            software_name = raw_input()
            print "Please enter specific version(if no specific version, just type return):"
            version = raw_input()
            res = SCONE.check_user_can_use_software(user_name, software_name, version)
            if res:
                print "Yes, this user is authorized to use the software"
            else:
                print "No, you have to first authorize the user with access privilege"
        elif user_input == 12:
            while True:
                print '1: Check which user'
                print '2: Check which task'
                print '3: Check which software'
                print '4: Back'
                user_input = raw_input()
                if user_input.isdigit() and (1 <= int(user_input) <= 4):
                    break
                print "invalid input! Please try again"
            if user_input == '4':
                continue
            target = ['user', 'task', 'software'][int(user_input) - 1]
            print 'Please enter software name (without version)'
            software_name = raw_input()
            print 'Please enter version of software'
            version = raw_input()
            if len(version) != 0:
                while True:
                    print 'compare type:'
                    print '1: ='
                    print '2: >'
                    print '3: <'
                    print '4: Back'
                    user_input = raw_input()
                    if user_input.isdigit() and (1 <= int(user_input) <= 4):
                        break
                    print "invalid input! Please try again"
                if user_input == '4':
                    continue
                compare = ['equal', 'newer', 'older'][int(user_input) - 1]
            else:
                version = None
                compare = None
            res = SCONE.check_vulnerability_and_add_it(target, software_name, version, compare)
            if res == -1:
                print 'software not exists'
                continue
            print 'List of ' + target + ' affected:'
            print ', '.join(res)
        elif user_input == 13:
            print "Input the number of the vulnerability"
            user_input = raw_input()
            if not user_input.isdigit():
                break
            res = SCONE.get_software_vulnerability(int(user_input))
            if res == -1:
                print "Invalid number of vulnerability"
                break
            else:
                if res[1] is None:
                    print "Software %s has vulnerability" % res[0]
                else:
                    print "Software %s with version %s %s has vulnerability" % (res[0], res[2], res[1])
        else:
            print "invalid input! Please try again"
            continue
        print "Please enter to continue"
        raw_input()


if __name__ == "__main__":
    main()
