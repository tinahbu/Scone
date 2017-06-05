import Pyro4
import osquery


class StateModifier(object):
    def __init__(self):
        self.scone = Pyro4.Proxy('PYRONAME:scone')

    def run(self):
        while True:
            # do with osquery
            # do with scone
            self.scone.interface1()
            break
