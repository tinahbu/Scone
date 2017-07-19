from time import sleep

import Pyro4
import osquery


class StateModifier(object):
    def __init__(self):
        self.scone = Pyro4.Proxy('PYRONAME:scone')
        sleep(1)

    def run(self):
        SCONE = self.scone
        while True:
            # do with scone
            # print SCONE.create_software("ABC", ["1.4", "3.5"])
            # print SCONE.create_software("DEF", ["1.4", "3.5"])
            # print SCONE.add_software_dependencies("ABC_1.4", ["DEF_1.4", "DEF_3.5", "DEF_3.3"])
            # print SCONE.add_software_version("ABC", "2.4")
            # print SCONE.add_software_version("ABC", "2.4")
            # print SCONE.add_software_version("AYC", "2.4")
            #
            #
            #
            # ret = SCONE.user_create_task("Qiaoyu's task")
            # print ret
            # ret = SCONE.user_create_task("Shopping cart development")
            # print ret
            #
            # ret = SCONE.user_task_requires_software("Qiaoyu's task", ["123", "456"])
            # print ret
            #
            # ret = SCONE.user_task_performed_by("CNN for product recommendation", "user 1")
            # print ret
            #
            # ret = SCONE.user_task_performed_by("CNN for product recommendation", "user 3")
            # print ret
            #
            # print SCONE.create_user_group("new group")
            # print SCONE.create_user_group("new group")
            #
            #
            # print SCONE.create_user("new usr", "123", "wef@125.com", "new group")
            # print SCONE.create_user("new usr", "123", "wef@125.com", "new group")
            # print SCONE.create_user("new u2sr", "123", "wef@125.com", "new ")
            # print SCONE.create_user("new u2sr", "123", "wef@125.com")
            #

            # print SCONE.create_user("Qiaoyu Deng", "2012211616", "qdeng@andrew.cmu,edu")
            # print SCONE.assign_user_to_groups("Qiaoyu Deng", ["backend developer"])
            # print SCONE.check_user_can_use_software("Qiaoyu Deng", "Expresso")
            # print SCONE.check_user_can_use_software("Qiaoyu Deng", "Apache")
            # print "end!"

            print SCONE.create_user("Qiaoyu Deng", "2012211616", "qdeng@andrew.cmu.edu")
            print SCONE.assign_user_to_groups("Qiaoyu Deng", ["backend developer"])
            print SCONE.create_software("Hadoop", ["1.0.0", "1.1.0"])
            print SCONE.add_software_dependencies("Hadoop", ["Python"])
            print SCONE.add_software_version("Hadoop", "1.1.1")
            print SCONE.user_create_task("Qiaoyu's task")
            print SCONE.user_task_requires_software("Qiaoyu's task", ["Hadoop"])
            print SCONE.user_group_is_authorized_to_exec("backend developer", ["Hadoop"])
            print SCONE.user_task_performed_by("Qiaoyu's task", "Qiaoyu Deng")
            print SCONE.check
            print "end"
            break

