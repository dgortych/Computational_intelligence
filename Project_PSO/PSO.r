
#Ilosc wymiarow
ndim <-2

#funkcja testowa
rosen <- function(x) {
  res <- 0
  for (i in 1:(ndim - 1)){
    res <- res + (1-x[i])^2+100*(x[i+1]-x[i]^2)^2
  }
  return (res)
}


x <-c( seq(-2,2,0.1))

coords<-list()
for (i in 1:ndim){
  coords[[i]]<-seq(-2,2,0.1)
}

n<-length(x)

grid<-expand.grid(coords)

z<-rosen(grid)

zm<-matrix(as.matrix(z), ncol=n)


#Wykres

#levels0 = seq(0, 1, 0.1)
#levels1 = seq(1, 10, 1)
#levels2 = seq(10, 5000, 50)

#plot(coords[1], coords[2], type='n')
#contour(x, y, zm, add=TRUE, lty=2, levels = levels2)
#contour(coords[1], coords[2], zm, add=TRUE, lty=2, levels = c(levels0, levels1, levels2))
#^na pewno mo¿na zrobiæ to ³adniej :) Jak? 


bb<-list()
velocity <- list()
best <- list()

for (i in 1:ndim){
  bb[[i]]<-runif(16, min = -2, max = 2)
  velocity[[i]] <- rep(0,16)
  best[[i]] <- bb[[i]]
}
b<-data.frame(bb)
velocity <- data.frame(velocity)
best <- data.frame(best)


#budujemy ramkê z pozycjami i wartoœci¹ funkcji celu
z<-as.numeric(as.matrix(rosen(b)))
b<-cbind(b,z)

#ustalamy który tak jest najlepszy
best_bird<-which.min(b$z)

#budujemy pust¹ ramkê na zapisanie evolucji rozwi¹zania 
best_evo<-data.frame(bx=double(),by=double(),z=double())
#bb[[ndim+1]]

#stale/wagi

damping <- 0.7298
c1 <-  0.5
c2 <-  0.8

run <- 1
k <- 1

while (run){
  #pêtla po ptaszkach 
  for (i in 1:16){
    
    for(j in 1:ndim){
      velocity[,j][i] <- damping*(velocity[,j][i] + c1*runif(1)*( best[,j][i] - b[,j][i] ) + c2*runif(1)*(b[,j][best_bird] - b[,j][i] ))
      b[,j][i] <- b[,j][i] + velocity[,j][i]
    }
    
      b$z[i] <- rosen( b[i,][1:ndim] )
      
    if( b$z[i] < rosen( best[i,] )){
       for(j in 1:ndim){
           best[,j][i] <- b[,j][i]
       }
    }
  } #ptaszki 
  
  #który ptaszek jest teraz najlepszy?
  best_bird<-which.min(b$z)
  
  #zapis ewolucji rozwi¹zania
  best_evo[k,]<-b[best_bird,]

  #warunek stopu
 if( sd( unlist(b$z) ) < 0.001^ndim ){
    run <- 0
  }
  
  k <- k+1
 
} #iteracje 

#dorysowanie œcie¿ki znajdowania rozwi¹zania 
points(best_evo$bx,best_evo$by,t="b",pch=19)

