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
            print SCONE.create_software("ABC", ["1.4", "3.5"])
            print SCONE.create_software("DEF", ["1.4", "3.5"])
            print SCONE.add_software_dependencies("ABC_1.4", ["DEF_1.4", "DEF_3.5", "DEF_3.3"])
            print SCONE.add_software_version("ABC", "2.4")
            print SCONE.add_software_version("ABC", "2.4")
            print SCONE.add_software_version("AYC", "2.4")



            ret = SCONE.user_create_task("Qiaoyu's task")
            print ret
            ret = SCONE.user_create_task("Shopping cart development")
            print ret

            ret = SCONE.user_task_requires_software("Qiaoyu's task", ["123", "456"])
            print ret
            print "end!"
            break

