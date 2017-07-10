import Pyro4


class StateInquirer(object):
    def __init__(self):
        self.scone = Pyro4.Proxy('PYRONAME:scone')

    def run(self):
        while True:
            SCONE = self.scone
            # do with scone
            # SCONE.interface2()
            break

