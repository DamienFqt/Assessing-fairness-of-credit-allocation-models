#### Cas Astra ####

## Etape 1 : Importer et vérifier les données ##
credit.data <- read.csv2('CreditScoring.csv', stringsAsFactors = TRUE)
str(credit.data)
summary(credit.data)

# vérifier la collinéarité entre les variables explicatives
cor(credit.data[-c(1,3,6,7)])

## Etape 2 : Transformer les données pour faciliter l'analyse

# reordonner les variables catégorielles
credit.data$Home <- relevel(credit.data$Home, "rent")
credit.data$Marital <- relevel(credit.data$Marital, "single")
credit.data$Job<- relevel(credit.data$Job, "fulltime")

## Etape 3 : Préparer les données pour la régression logistique

# créer deux bases de données : une première pour construire le modèle, une deuxième pour le valider
credit.train <- credit.data[1:2980, ]
credit.test <- credit.data[-c(1:2980), ]

## Etape 4 : Lancer l'algorithme de régression logistique

credit.model <- glm(Decision ~., family=binomial(link='logit'), data=credit.train) # Le "~" représente "est une fonction de" et le "." représente "toutes les variables"
summary(credit.model)

# relancer l'alogrithme sans les variables non significatives
credit.model <- glm(Decision ~ Seniority + Home + Marital + Age + Job 
                    + Expenses + Income + Assets + Debt + Amount + Price, 
                    family=binomial(link='logit'), data=credit.train)
summary(credit.model)

## Etape 5 : Evaluer la précision du modèle

# lancer le modèle avec les données de validation
credit.predict <- predict(credit.model, newdata=credit.test, type='response') # L'argument type='response' renvoie les probabilités
head(credit.predict)

# estimer à partir de la probabilité la valeur de la variable decision (0/1)
credit.predict <- ifelse(credit.predict > 0.5, 1, 0)
head(credit.predict)

# calculer le pourcentage de prédictions justes
PredictionAccuracy <- mean(credit.predict == credit.test$Decision)
print(paste('Prediction accuracy =',round(PredictionAccuracy, digits=2)))

# tranformer les variables credit.predict et credit.test$Decision en factors
credit.predict <- factor(credit.predict, levels = c(0,1), labels = c("predicted reject", "predicted accept"))
credit.test$Decision <- factor(credit.test$Decision, levels = c(0,1), labels = c("rejected", "accepted"))

# réaliser un tableau croisé et un mosaicplot entre les deux variables
table(credit.predict,credit.test$Decision)
prop.table(table(credit.predict,credit.test$Decision),2)
mosaicplot(table(credit.predict,credit.test$Decision), color = TRUE)

## Etape 6 : Utiliser le modèle pour traiter un dossier
predictors <- data.frame(Seniority=5, Home="owner", Marital="single", Age=50, Job="freelance", 
                         Expenses=50, Income=300, Assets=0, Debt=0, Amount=500, Price=400)
predict(credit.model, predictors, type="response")
