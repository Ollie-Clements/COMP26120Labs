import im
import time

server = im.IMServerProxy('https://web.cs.manchester.ac.uk/e11226oc/comp28112_ex1/IMserver.php')

def cmessage():
    if(server[usr] == b'send\n'): # Check status of client, send 
        print(server['RM'])
        clientMessage = input('Please type your message: ') # Ask User for message
        server.__setitem__('RM', clientMessage) # If User's turn then send message
        if (clientMessage == "exit"): 
            server.clear()
            exit() # Terminates the program
        server.__setitem__(usr, 'receive')
        temp = server["Turn"]
        temp = temp[:-1]
        temp = temp.decode("utf-8")
        server.__setitem__(temp, "send")
        server.__setitem__("Turn", usr)
        cmessage()
    else: 
        time.sleep(1) # Otherwise, wait 1 second and try again
        cmessage() 

server.clear()
print("Please enter 'exit' if you wish to terminate the chat at any point")
usr = input("Please enter your name: ")

if (server["RM"] == b"\n"):
    server.__setitem__(usr, 'send') # Set state of User 1 to send - needs to be changed 
    server.__setitem__("RM", "hold")
else:
    server.__setitem__(usr, 'receive')
    server.__setitem__("Turn", usr)

cmessage()