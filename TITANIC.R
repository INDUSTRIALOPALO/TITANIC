#' Regresi�n log�stica utilizando la base de datos del Titanic
#' Librer�a para an�lisis gr�fico de los valores presentes y perdidos
library(Amelia)   #'Permite la representaci�n visual del dataset
library(dplyr)    #'Extensi�n dedicada al an�lisis de datasets
library(reshape2) #'Extensi�n utilizada para el ajuste gr�fico
library(ggplot2)
library(plotly)
df.train<- read.csv('~/R/train.csv',header=T,na.strings=c(""))
#'Contar n�mero de valores perdidos
sapply(df.train,function(x) sum(is.na(x)))
#'Contar n�mero de valores presentes
sapply(df.train, function(x) length(unique(x)))

#'Rrevisar las caracter�sticas del dataset
print(str(df.train))

#'Generar un mapa de color con los valores presentes y perdidos

#'Funci�n para aplicar ggplot a missmap
x<-df.train
x %>% 
  is.na %>%
  melt %>%
  ggplot(data = .,
         aes(x = Var2,
             y = Var1)) +
  geom_raster(aes(fill = value)) +
  scale_fill_grey(name = "",
                  labels = c("Presentes","Perdidos")) +
  theme_classic() + 
  theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
  labs(x = "Variables en el Dataset",
       y = "Observaciones")

#'Con el fin de trabajar con entradas m�s consistentes, se eliminan a continuaci�n las columnas que no aportan informaci�n, como el ID (columna 1), Nombre (Columna 4),El n�mero del ticket (Columna 9) y Cabin (Columna 11 con m�s datos ausentes)
data <- subset(df.train, select = c(2,3,5,6,7,8,10,12))
#'Nuevamente se genera el mapa de valores perdidos. donde s�lo quedan ausentes algunas edades
x<-data
x %>% 
  is.na %>%
  melt %>%
  ggplot(data = .,
         aes(x = Var2,
             y = Var1)) +
  geom_raster(aes(fill = value)) +
  scale_fill_grey(name = "",
                  labels = c("Presentes","Perdidos")) +
  theme_classic() + 
  theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
  labs(x = "Variables en el Dataset",
       y = "Observaciones")


#'El dataset es tranformado mediante el relleno de los valores perdidos de la edad, para este caso, se trabajar� con el promedio lo cual se ver� reflejado en el histograma de frecuencias
data$Age[is.na(data$Age)] <- round(mean(data$Age,na.rm=T), digits = 1)
#'Al observar el comportamiento de los datos, se puede apreciar que el promedio tiene un comportamiento dispar a los dem�s, ya que el mayor recuento de las otras categor�as se encuentra en 27, frente a 202; por tanto, se procede a eliminar las respuestas ausentes.
bp_a <- ggplot(data,aes(x=Age, fill = factor(Age)))+geom_histogram(bins= 60, alpha =0.5, show.legend = NA) 
ggplotly()
#'Eliminaci�n de los registros ausentes para la variable Edad
data <- subset(df.train, select = c(2,3,5,6,7,8,10,12))
data <- data[!is.na(data$Age),]
#'Eliminaci�n de los registros ausentes para la variable categ�rica Embarked
data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL

#'las variables codificadas por R (categ�ricas), se pueden estudiar mediante la funci�n de contraste
contrasts(data$Sex)
contrasts(data$Embarked)


#' El comportamiento de las variables predictoras puede ser analizado mediante diagramas de frecuencia, conteo o histogramas
#'Sobrevivi�
bp_s <- ggplot(data,aes(x=Survived, fill = factor(Survived))) + geom_bar()
ggplotly()
#'Clase del tiquete
bp_pc <- ggplot(data,aes(x=Pclass, fill = factor(Pclass)))+geom_bar()
ggplotly()
#'G�nero
bp_s <- ggplot(data,aes(x=Sex, fill = factor(Sex)))+geom_bar()
ggplotly()
#'Edad
bp_a <- ggplot(data,aes(x=Age, fill = factor(Age)))+geom_histogram(bins= 60, alpha =0.5, show.legend = NA) 
ggplotly()
#'SibSp
bp_si <- ggplot(data,aes(x=SibSp, fill = factor(SibSp)))+geom_bar()
ggplotly()
#'Parch
bp_pa <- ggplot(data,aes(x=Parch, fill = factor(Parch)))+geom_bar()
ggplotly()
#'Fare
bp_f <- ggplot(data,aes(x=Fare, fill = factor(Fare)))+geom_histogram(bins= 30, alpha =0.5, show.legend = NA) 
ggplotly()
#'Construcci�n del modelo de regresi�n log�stico binario
model <- glm(Survived ~.,family=binomial(link='logit'),data=data)
#'Salidas del modelo
summary(model)
#' Teniendo en cuenta lo anterior, se identifican ciertas variables que inciden en la probabilidad de supervivencia al evento.
#' Para el caso puntual de la edad (variable cont�nua), por cada a�o de m�s, la probabilidad de supervivencia disminuye en 0.043 (aproximadamente)
#' De manera similar, al aumentar el tipo de clase (Pclass) indica una menor calidad categor�a y por ende, disminuye la probabilidad de supervivencia en 1.20 aproximadamente.
newdata1 <- with(data, data.frame(Pclass=as.integer(factor(rep(1:3, each = 2))), Sex=factor(rep(c("female","male"), each = 6)), Age = mean(Age), SibSp = mean(SibSp),Parch=as.integer( factor(rep(0:2, each = 2))), Fare = mean(Fare),Embarked=factor(rep(c("C","Q","S"), each = 2))))
newdata1$SurvivedP <- predict(model, newdata = newdata1, type = "response")
newdata2 <- with(data, 
                 data.frame(Pclass=as.integer(factor(rep(1:3, each = 20))), 
                      Sex=factor(rep(c("female","male"), each = 60)), 
                      Age = rep(seq(from =min(Age), to= max(Age), length.out = 120)), 
                      SibSp = as.integer( factor(rep(0:3, each = 10))),
                      Parch=as.integer( factor(rep(0:2, each = 20))), 
                      Fare = rep(seq(from =min(Fare), to= max(Fare), length.out = 12)), 
                      Embarked=factor(rep(c("C","Q","S"), each = 20))))
newdata2$SurvivedP <- predict(model, newdata = newdata2, type = "response")
newdata2
newdata3 <- cbind(newdata2, predict(model, newdata = newdata2, type = "link", se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
ggplot(newdata3, aes(x = Age, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,ymax = UL, fill = Sex), alpha = 0.2) + geom_line(aes(colour = Sex), size = 1)
ggplotly()

ggplot(newdata3, aes(x = Age, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,ymax = UL, fill = Sex), alpha = 0.2) + geom_line(aes(colour = Sex), size = 1)
ggplotly()

ggplot(newdata3, aes(x = Age, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,ymax = UL, fill = Embarked), alpha = 0.2) + geom_line(aes(colour = Embarked), size = 1)
ggplotly()


ggplot(newdata3, aes(x = Parch, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,ymax = UL, fill = Embarked), alpha = 0.2) + geom_line(aes(colour = Embarked), size = 1)
ggplotly()
