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
            SCONE.create_software("Left4Dead")
            SCONE.create_software("WoW")
            break

