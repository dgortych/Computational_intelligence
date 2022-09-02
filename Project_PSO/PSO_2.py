import math
import matplotlib.pyplot as plt
import numpy as np

def f1(x1):
    return x1

def f2(x1,x2,x3):
    return 1 - 1/(4*math.pi**2)*(x1+math.pi)*2 + abs(x2-5*math.cos(x1))**(1/3) + abs(x3 - 5*math.sin(x1))**(1/3)


x1_1 = 2
x1_2 = 2
x2 = 2
x3 = 2

#x = np.linspace(-2,2,100)

#p = plt.plot(x,f2(x,x,x))
#plt.show()

#dziedziny

dx1 = np.arange(-np.pi,np.pi,0.1)

dx2_3 = np.arrange(-5,5,0.1)


#wartości dla prawdziwych parametrów
y1 = f1(x1_1)
y2 = f2(x1_2,x2,x3)


#Wagi
import random

m1_v = np.repeat(NA,1000)
m2_v<-rep(NA,1000)
par_m<-matrix(ncol=3,nrow=1000)
#1000 punktów startowych i 1000 wag:
for i in range(1000):
  w1 = random.random()
  w2 = 1 - w1
  #losowe startowe a b i c
  a<-runif(1,0,5)
  b<-runif(1,0,5)
  c<-runif(1,0,5)
  best<-optim_nm(start=c(a,b,c),k=3,fun=misfit)
  m1_v[i]<-m1(best$par)
  m2_v[i]<-m2(best$par)
  par_m[i,]<-best$par


plot(m1_v,m2_v,pch=19)







