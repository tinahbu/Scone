import Pyro4
import osquery


class StateModifier(object):
    def __init__(self):
        self.scone = Pyro4.Proxy('PYRONAME:scone')
        self.osq = osquery.SpawnInstance()
        self.osq.open()

    def run(self):
        SCONE = self.scone
        OSQUERY = self.osq.client
        while True:
            res = OSQUERY.query('select timestamp from time')
            if res.status.code != 0:
                print("Error running the query: %s" % res.status.message)
                continue

            for row in res.response:
                for key, val in row.iteritems():
                    print("%s => %s" % (key, val))

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
            ret = SCONE.user_task_performed_by("CNN for product recommendation", "user 3")
            print ret
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

            print SCONE.create_user("Qiaoyu Deng", "2012211616", "qdeng@andrew.cmu,edu")
            print SCONE.assign_user_to_groups("Qiaoyu Deng", ["backend developer"])

            print SCONE.check_access("user 3", "CNN for product recommendation")
            print SCONE.check_access("user 1", "CNN for product recommendation")
            print "end!"
            break

