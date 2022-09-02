#funkcja testowa
rosen <- function(x) {
  return( 100 * (x[2] - x[1] * x[1])^2 + (1 - x[1])^2 )
}

#narysujmy j¹ w zakresie od -2 do 2
x <- seq(-2,2,0.1)
y <- seq(-2,2,0.1)

n<-length(x)

xy<-expand.grid(x,y)

z<-rosen(xy)

zm<-matrix(as.matrix(z), ncol=n)

plot(x, y, type='n')
contour(x, y, zm, add=TRUE, lty=2, nlevels = 100)

#Z1
#levels0 = seq(0,1,0.1)
#levels1 = (seq(0, 10, 1))
#levels2 = (seq(10, 5000, 50))

#plot(x, y, type='n')
#contour(x, y, zm, add=TRUE, lty=2, levels = c(levels0, levels1, levels2))


###pso bare bones###

#inicjalizacja
bx<-runif(16, min = -2, max = 2)
by<-runif(16, min = -2, max = 2)

b<-data.frame(bx,by)

#budujemy ramkê z pozycjami i wartoœci¹ funkcji celu
z<-as.numeric(as.matrix(rosen(b)))
b<-cbind(b,z)

#ustalamy który tak jest najlepszy
best_bird<-which.min(b$z)

#budujemy pust¹ ramkê na zapisanie evolucji rozwi¹zania 
best_evo<-data.frame(bx=double(),by=double(),z=double())


x_velocity <- rep(0,16)
y_velocity <- rep(0,16)
best_x <- bx
best_y <- by
b <- cbind(b,x_velocity,y_velocity,best_x,best_y)

damping <- 0.7298
c1 <-  0.5
c2 <-  0.8


for (k in 1:1000){ 
  
  #pêtla po ptaszkach 
  for (i in 1:16){
    
    b$x_velocity[i] <- damping*(b$x_velocity[i] + c1*runif(1)*( b$best_x[i] - b$bx[i] ) + c2*runif(1)*(b$bx[best_bird] - b$bx[i] ))
    b$y_velocity[i] <- damping*(b$y_velocity[i] + c1*runif(1)*( b$best_y[i] - b$by[i] ) + c2*runif(1)*(b$by[best_bird] - b$by[i] ))
    
    b$bx[i] <- b$bx[i] + b$x_velocity[i]
    b$by[i] <- b$by[i] + b$y_velocity[i]
    
    
    b$z[i] <- rosen(c( b$bx[i],b$by[i]))
    
    if ( b$z[i] < rosen( c(b$best_x[i], b$best_y[i])) ){
      b$best_x[i] <- b$bx[i]
      b$best_y[i] <- b$by[i]
    }
   
  } #ptaszki 
  
  
  #który ptaszek jest teraz najlepszy?
  best_bird<-which.min(b$z)
  
  #zapis ewolucji rozwi¹zania
  best_evo[k,]<-b[best_bird,]
} #iteracje 

#dorysowanie œcie¿ki znajdowania rozwi¹zania 
points(best_evo$bx,best_evo$by,t="b",pch=19)
