import matplotlib.pyplot as plt

x = []
y = []

f = open("D:/scala/testProject/myscalaproject/point.txt", "r")
lines = f.readlines()
f.close()

for line in lines:
    print(line.split(','))

