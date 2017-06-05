import Pyro4 as Pyro4
from threading import RLock

@Pyro4.expose
class Scone(object):
    def __init__(self):
        self.lock = RLock()

    def interface1(self):
        self.lock.acquire()
        print 123
        self.lock.release()

    def interface2(self):
        self.lock.acquire()
        print 456
        self.lock.release()

    def run(self):
        daemon = Pyro4.Daemon()
        ns = Pyro4.locateNS()
        uri = daemon.register(self)
        ns.register('scone', uri)
        daemon.requestLoop()
