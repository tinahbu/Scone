import Pyro4


class StateInquirer(object):
    def __init__(self):
        self.scone = Pyro4.Proxy('PYRONAME:scone')

    def run(self):
        while True:
            # do with scone
            self.scone.interface2()
            break

