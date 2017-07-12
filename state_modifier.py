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
            ret = SCONE.user_create_task("Qiaoyu's task")
            print ret
            ret = SCONE.user_create_task("Shopping cart development")
            print ret

            ret = SCONE.user_task_requires_software("Qiaoyu's task", ["123", "456"])
            print ret

            ret = SCONE.task_performed_by("CNN for product recommendation", "user 1")
            print ret
            print "end!"
            break

