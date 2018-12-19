#!/usr/bin/env python3

import socket
import threading
import time
from colorama import Fore, Style

def print_received(socket):
    while True:
        data = socket.recv(4096)
        if data:
            print(f"{Fore.RED}Recieved:\n{data.decode('utf-8')}{Style.RESET_ALL}", end="")

with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
    s.connect(("mirage-xmpp.dev", 5222))

    threading.Thread(target=print_received, args=(s,), daemon=True).start()

    for line in open("demo-stanzas.xml", "r"):
        line = line.strip()
        print(f"Preparing to send:\n{Fore.GREEN}{line}{Style.RESET_ALL}\n", end="")
        input("Press enter to send\n")
        s.send(line.encode())
        time.sleep(1)
    s.close()
