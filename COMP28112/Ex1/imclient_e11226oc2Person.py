import im
import time

server = im.IMServerProxy('https://web.cs.manchester.ac.uk/e11226oc/comp28112_ex1/IMserver.php')

server.clear()

server['c1'] = 'send' #Set state of User 1 to send
server['c2'] = 'recieve' #Set state of User 2 to receive

def c1message():
    print(server['client2'])
    client1Message = input('Please type your message: ') #Ask User for message
    if(server['c1'] == b'send\n'): #Check status of server
        
        server['client1'] = client1Message #If User's turn then send message
        server['c1'] = 'receive'
        server['c2'] = 'send'
    else: 
        time.sleep(5)
        c1message() #Otherwise, wait 5 seconds and try again

def c2message():
    print(server['client1'])
    client2Message = input('Please type your message: ') #Ask User for message 
    if(server['c2'] == b'send\n'): #Check status of server
        
        server['client2'] = client2Message #If User's turn then send message
        server['c2'] = 'receive'
        server['c1'] = 'send'
    else:
        time.sleep(5)
        c2message() #Otherwise, wait 5 seconds and try again

while True:
    c1message()
    c2message()

