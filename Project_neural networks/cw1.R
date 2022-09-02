
library(liver) 
data(cereal) 

#podgl¹d danych 
str(cereal) 
summary(cereal) 
#wykonanie macierzy korelacyjnej 
library(corrplot) 
## corrplot 0.90 loaded 
?cor 
cor_m_p<-round(cor(cereal[4:16], method = 'pearson'),2) 
cor_m_p
corrplot(cor_m_p, method = 'number', type = 'upper', diag = FALSE)
cor_m_s<-round(cor(cereal[4:16], method = 'spearman'),2) 
cor_m_s 
corrplot(cor_m_s, method = 'number', type = 'upper', diag = FALSE)

#wizualizacja wybranych zale¿noœci 
plot(cereal$rating~cereal$calories)
plot(cereal$rating~cereal$sugars)
plot(cereal$rating~cereal$protein)
plot(cereal$rating~cereal$fiber)


#przygotowanie danych , skalowanie tak aby wszystkie wartosc ibylo pomiedzy 0 - 1
maxs <- apply(cereal[,-c(1:3)], 2, max) 
mins <- apply(cereal[,-c(1:3)], 2, min)
sc_cereal <- data.frame(cereal[1:3], scale(cereal[,-c(1:3)], center = mins, scale = maxs - mins))
#usuiecie 0 w rating do obliczenia MAPE
sc_cereal <- sc_cereal[-c(11),]

sets <- sample(1:nrow(sc_cereal), 0.80 * nrow(sc_cereal)) 
train_cereal<-sc_cereal[sets,] 
test_cereal<-sc_cereal[-sets,]



#model 
library(neuralnet) 
set.seed(1234) 
#zmienne ilosciowe trafiaja jako jeden neuron, jakosciowe - tyle ile pozpimow zmiennej\
#czyli i nas trafiaja dwa neurony bo mamy dwie zmienne ilosciowe ( calories i sugars)

nn_cereal1 <- neuralnet(rating~calories+sugars+fiber+protein+fat,
                        data = train_cereal,
                        hidden = c(2, 3),
                        act.fct="tanh",
                        algorithm = "rprop+",
                        linear.output = TRUE)

plot(nn_cereal1)
pr_nn_cereal1<- compute(nn_cereal1, test_cereal) 
pr<-unlist(pr_nn_cereal1$net.result) 

#ocena jakoœci modelu ( porownujemy modele i wybieramy ten dla ktorego R2 jest blizsze 1)
nn_cereal1_mod<-unlist(nn_cereal1 [["net.result"]]) 
R2<-(cor(train_cereal$rating, nn_cereal1_mod))^2
R2


se<- mean(train_cereal$rating-nn_cereal1_mod)
se

plot(train_cereal$rating, nn_cereal1_mod, col = "red", main = 'Real vs Predicted') 

#reszty modelu
#dla wyliczonych reszt powinniœmy dodatkowo sprawdziæ czy spe³niaj¹ za³o¿enia procesu bia³oszumowego 
#Dla prognoz/zbioru testowego wyliczamy: MAPE 


#jesli mniej niz 10 to smodel popawny, jesli Inf to poprawic 0 w danych
library(MLmetrics) 
mape_m1<-MAPE(pr, test_cereal$rating)
mape_m1


#zmodyfikowac funkcje aktywacji, jesli to nie wystarczy to pozmieniac neurony,zmienne , strona neuralnet