
#Wczytanie danych
data <- read.table(file = "D:/Studia Materia³y/Semestr6/IO/CZ2/Projekt/diamonds-k-dl.txt", sep=',', header = TRUE, dec='.')
#podstawowe informacje
str(data) 
summary(data) 
#Zawe¿enie danych
data <- data[sample(nrow(data),1000),]
#Zmiana danych kategorycznych 
columns_to_transorm <- c('cut','color', 'clarity')

data[columns_to_transorm] <- lapply(data[columns_to_transorm] , factor)

data[, columns_to_transorm] <- sapply(data[, columns_to_transorm], unclass)

library(corrplot)
#Macierze korelacyjne
cor_m_p<-round(cor(data, method = 'pearson'),2) 
cor_m_p
corrplot(cor_m_p, method = 'number', type = 'upper', diag = FALSE)
cor_m_s<-round(cor(data, method = 'spearman'),2) 
cor_m_s 
corrplot(cor_m_s, method = 'number', type = 'upper', diag = FALSE)

#Standaryzacja
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
sc_data <- data.frame(data, scale(data, center = mins, scale = maxs - mins))

sets <- sample(1:nrow(data), 0.80 * nrow(data)) 
train_data<-sc_data[sets,] 
test_data<-sc_data[-sets,]

#Model
library(neuralnet) 
set.seed(1234) 

nn_data1 <- neuralnet(price.1~carat.1,
                        data = train_data,
                        hidden = c(2, 3),
                        act.fct="tanh",
                        algorithm = "rprop+",
                        linear.output = TRUE)

plot(nn_data1)
pr_nn_data1<- compute(nn_data1, test_data) 
pr<-unlist(pr_nn_data1$net.result) 


#ocena jakosci modelu
nn_data1_mod<-unlist(nn_data1 [["net.result"]]) 
R2<-(cor(train_data$price.1, nn_data1_mod))^2
R2


se<- mean(train_data$price.1-nn_data1_mod)
se

plot(train_data$price.1, nn_data1_mod, col = "red", main = 'Real vs Predicted') 


#jesli mniej niz 10 to smodel popawny, jesli Inf to poprawic 0 w danych
library(MLmetrics) 
mape_m1<-MAPE(pr, test_data$price.1)
mape_m1



nn_data2 <- neuralnet(price.1~carat.1+z.1+x.1+y.1,
                      data = train_data,
                      hidden = c(2, 2),
                      act.fct="tanh",
                      algorithm = "rprop+",
                      linear.output = TRUE)

plot(nn_data2)
pr_nn_data2<- compute(nn_data2, test_data) 
pr<-unlist(pr_nn_data2$net.result) 


#ocena jakosci modelu
nn_data2_mod<-unlist(nn_data2 [["net.result"]]) 
R2<-(cor(train_data$price.1, nn_data2_mod))^2
R2

se<- mean(train_data$price.1-nn_data2_mod)
se

plot(train_data$price.1, nn_data2_mod, col = "red", main = 'Real vs Predicted') 

mape_m1<-MAPE(pr, test_data$price.1)
mape_m1

