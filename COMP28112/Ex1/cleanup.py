import im
import time

server = im.IMServerProxy('https://web.cs.manchester.ac.uk/e11226oc/comp28112_ex1/IMserver.php')

server.clear()
server.__setitem__("RM", "Start")