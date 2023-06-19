############################################################################################################################

## 0. Required packages

library( car )
library( ellipse )
library( faraway )
library( leaps )
library(MASS)
library( GGally)
library(rgl)
library(corrplot)
library( Matrix )

############################################################################################################################

## 1. Carichiamo il dataset

dati = read.csv("House.csv") 
dati = dati[ , c(1,2,3,4,5,6,7,8,9)]
dim(dati) # 1000 osservazioni
head(dati)
summary(dati)
dati = na.omit(dati)
dim(dati) # 994 osservazioni 

############################################################################################################################

## 2. Visualizziamo i dati

# a. Descrittiva
#x11()
par( mfrow = c( 1, 2 ) )
hist(dati$median_house_value, main = "Histogram" )
boxplot( dati$median_house_value, main = "Boxplot", pch = 16, col = 'lavender' )
#migliorare assi

# b. Pairs
#x11()
pairs(dati[ , c('longitude', 'latitude', 'housing_median_age', 'total_rooms', 'total_bedrooms', 'population', 'households', 'median_income', 'median_house_value')], pch = 16)

# c.GGPairs
#x11()
#ggpairs(data = dati, title ="Relationships between predictors & response", 
 #       lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))

############################################################################################################################

## 3. Modello lineare

# a. originale
g = lm( median_house_value ~ longitude + latitude + housing_median_age + total_rooms + total_bedrooms + population + households + median_income, data = dati )
summary( g ) # Radj = 0.6424
vif(g) # longitude           latitude housing_median_age        total_rooms     total_bedrooms         population 
       # 3.680771           3.599631           2.348495          17.324922         157.402228          14.102432 
       # households      median_income 
       # 176.591071           1.899470 
AIC(g) # 24469.87
step(g) # dice togli: total rooms, total bedrooms, housing-median-age

# b. senza total rooms
g = lm( median_house_value ~ longitude + latitude + housing_median_age + total_bedrooms + population + households + median_income, data = dati )
summary( g ) # Radj = 0.6425
vif(g) # longitude           latitude housing_median_age     total_bedrooms         population         households 
       # 3.622005           3.512761           2.342108         157.299608           9.616282         174.203925 
       # median_income 
       # 1.335929 
AIC(g) # 24468.52

# c. senza total rooms e total bedrooms
g = lm( median_house_value ~ longitude + latitude + housing_median_age + population + households + median_income, data = dati )
summary( g ) # Radj = 0.6427
vif(g) # longitude           latitude housing_median_age         population         households      median_income 
       # 3.622004           3.485665           2.299794           9.466305           9.199392           1.234356 
AIC(g) # 24467

# d. senza total rooms, total bedrooms e housing-median-age
g = lm( median_house_value ~ longitude + latitude + population + households + median_income, data = dati )
summary( g ) # Radj = 0.6427
vif(g) # longitude      latitude    population    households median_income 
       # 3.379261      3.212722      9.380365      8.724027      1.229353 
AIC(g) # 24465.94

############################################################################################################################

## 4. tabella di collinearità

# a. tabella
X = dati [ , c(1,2,6,7,8)] #not considering the response variable
#x11()
corrplot(cor(X), method='number')
corrplot(cor(X), method='color')

# b. modello senza total rooms, total bedrooms, housing-median-age e interazioni longitude*latitude e population*households
g = lm( median_house_value ~ longitude + latitude + longitude*latitude +  population + households + population*households + median_income, data = dati )
summary( g ) # Radj = 0.669
# population*households non è per nulla significativo

# c. modello senza total rooms, total bedrooms, housing-median-age e interazioni longitude*latitude
g = lm( median_house_value ~ longitude + latitude + longitude*latitude +  population + households + median_income, data = dati )
summary( g ) # Radj = 0.6691

# d. finale
dati_2 = dati[ , c(1,2,6,7,8,9)]
g = lm( median_house_value ~ longitude + latitude + longitude*latitude + population + households + median_income, data = dati_2 )
summary(g)
vif(g) # longitude      latitude    population    households median_income 


g = lm( median_house_value ~ longitude + latitude + longitude*latitude + households + median_income, data = dati_2 )
summary(g)
vif(g)
############################################################################################################################

## 5. Diagnostics

# a. Leverages
lev = hatvalues( g )
p = g$rank # p = 7
n = dim(dati_2)[1] # n = 994

watchout_points_lev = lev[ which( lev > 2 * p/n  ) ]
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ]

#x11()
plot( g$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages", 
      pch = 16, col = 'black' )

abline( h = 2 * p/n, lty = 2, col = 'red' )

watchout_points_lev = lev[ which( lev > 2 * p/n  ) ]
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ]

points( g$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'red', pch = 16 )

lev [ lev >  2 * p / n ]
sum = sum(   lev >  2 * p / n  ) # 87

# Fit the model without leverages - rimuovo 87 osservazioni
dati_lev = dati_2[lev<2 * p / n,]
dim(dati_lev) # 907 rianenti
g_lev = lm(median_house_value ~ longitude + latitude + longitude*latitude + population + households + median_income, data = dati_2, subset = ( lev < 2 * p / n ) )
summary( g_lev ) # Radj = 0.666

############################################################################################################################

# b. Standardized Residuals
gs = summary(g)
res_std = g$res/gs$sigma

watchout_ids_rstd = which( abs( res_std ) > 2 )
watchout_rstd = res_std[ watchout_ids_rstd ]

#x11()
plot( g$fitted.values, res_std, ylab = "Standardized Residuals", main = "Standardized Residuals" )
abline( h = c(-2,2), lty = 2, col = 'orange' )
points( g$fitted.values[watchout_ids_rstd], 
        res_std[watchout_ids_rstd], col = 'red', pch = 16 )
points( g$fitted.values[watchout_ids_lev], 
        res_std[watchout_ids_lev], col = 'orange', pch = 16 )
legend('topright', col = c('red','orange'), 
       c('Standardized Residuals', 'Leverages'), pch = rep( 16, 2 ), bty = 'n' )

res_std [ abs(res_std) >  2  ]
sum = sum(  abs(res_std) >  2  ) # 40

# Fit the model without Standardized Residuals - rimuovo 40 osservazioni
dati_star = dati_2[abs( res_std ) < 2,]
dim(dati_star) # 954 rianenti
g_star = lm(median_house_value ~ longitude + latitude + longitude*latitude + population + households + median_income, data = dati_2, subset = ( abs( res_std ) < 2 ))
summary( g_star ) # Radj = 0.7969

############################################################################################################################

# c. Studentized Residuals
stud = rstandard( g )

watchout_ids_stud = which( abs( stud ) > 2 )
watchout_stud = stud[ watchout_ids_stud ]

#x11()
plot( g$fitted.values, stud, ylab = "Studentized Residuals", main = "Studentized Residuals", pch = 16 )
points( g$fitted.values[watchout_ids_stud], 
        stud[watchout_ids_stud], col = 'blue', pch = 16 )
points( g$fitted.values[watchout_ids_rstd], 
        res_std[watchout_ids_rstd], col = 'red', pch = 16 )
points( g$fitted.values[watchout_ids_lev], 
        stud[watchout_ids_lev], col = 'orange', pch = 16 )
abline( h = c(-2,2), lty = 2, col = 'orange' )
legend('topright', col = c('blue','red','orange'), 
       c('Studentized Residual', 'Standardized Residuals', 'Leverages'), pch = rep( 16, 3 ), bty = 'n' )
# Studentized residuals and Standardized residuals identify the same influential points in this case.

############################################################################################################################

# d. Cook's distance
Cdist = cooks.distance( g )

p = g$rank # p = 7
n = dim(dati_2)[1] # n = 994

watchout_ids_Cdist = which( Cdist > 4/(n-p) ) 
watchout_Cdist = Cdist[ watchout_ids_Cdist ]

#x11()
par( mfrow = c( 1, 3 ) )
plot( g$fitted.values, Cdist, pch = 16, xlab = 'Fitted values', 
      ylab = 'Cooks Distance', main = 'Cooks Distance' )
points( g$fitted.values[ watchout_ids_Cdist ], Cdist[ watchout_ids_Cdist ], 
        col = 'green', pch = 16 )

plot( g$fitted.values, stud, pch = 16, xlab = 'Fitted values', 
      ylab = 'Studentized Residuals', main = 'Studentized Residuals' )
points( g$fitted.values[ watchout_ids_stud ], stud[ watchout_ids_stud ], 
        col = 'blue', pch = 16 )

plot( g$fitted.values, lev, pch = 16, xlab = 'Fitted values', 
      ylab = 'Leverages', main = 'Leverages' )
points( g$fitted.values[ watchout_ids_lev ], lev[ watchout_ids_lev ],
        col = 'orange', pch = 16 )

id_to_keep = !( 1:n %in% watchout_ids_Cdist )
Cdist [ id_to_keep ==  FALSE  ]
sum = sum(  id_to_keep ==  FALSE  ) # 55

# Fit the model without Standardized Residuals - rimuovo 55 osservazioni
dati_C = dati_2[ id_to_keep ==  TRUE,]
dim(dati_C) # 939 rianenti
g_C = lm( median_house_value ~ longitude + latitude + longitude*latitude + population + households + median_income, dati_2[ id_to_keep, ] )
summary( g_C ) # Radj = 0.7976

############################################################################################################################

# a+b+c+d
lev = hatvalues( g )
p = g$rank # p = 7
n = dim(dati_2)[1] # n = 994
# Fit the model without leverages - rimuovo 87 osservazioni
dati_lev = dati_2[lev<2 * p / n,]
dim(dati_lev) # 907 rianenti
g_lev = lm(median_house_value ~ longitude + latitude + longitude*latitude + population + households + median_income, data = dati, subset = ( lev < 2 * p / n ) )
summary( g_lev ) # Radj = 0.666

##
gs = summary(g_lev)
res_std_new = g_lev$res/gs$sigma
watchout_ids_rstd_new = which( abs( res_std_new ) > 2 )
watchout_rstd_new = res_std_new[ watchout_ids_rstd_new ]
# Fit the model without Standardized Residuals - rimuovo 41 osservazioni
dati_star_new = dati_lev[abs( res_std_new ) < 2,]
dim(dati_star_new) # 875 rianenti
g_star_new = lm(median_house_value ~ longitude + latitude + longitude*latitude + population + households + median_income, data = dati_lev, subset = ( abs( res_std_new ) < 2 ))
summary( g_star_new ) # Radj = 0.7894

##
Cdist_new = cooks.distance( g_star_new )

p_new = g_star_new$rank # p = 7
n_new = dim(dati_star_new)[1] # n = 875

watchout_ids_Cdist_new = which( Cdist_new > 4/(n_new-p_new) ) 
watchout_Cdist_new = Cdist_new[ watchout_ids_Cdist_new ]

id_to_keep_new = !( 1:n_new %in% watchout_ids_Cdist_new )
# Fit the model without Standardized Residuals - rimuovo 52 osservazioni
dati_C_new = dati_star_new[ id_to_keep_new ==  TRUE,]
dim(dati_C_new) # 821 rianenti
g_C_new = lm( median_house_value ~ longitude + latitude + longitude*latitude + population + households + median_income, dati_star_new[ id_to_keep_new, ] )
summary( g_C_new ) # Radj = 0.821


############################################################################################################################

# 3. Hypotheses of the model - Homoschedasticity 

# a. Modello base
#x11()
plot( g$fit, g$res, xlab = "Fitted", ylab = "Residuals", 
      main = "Residuals vs Fitted Values - Base", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )  # variabilità sembra sufficientemente uniforme

# Normality
# QQ plot
#x11()
qqnorm( g$res, ylab = "Raw Residuals", main = "Normal Q-Q Plot - Base", pch = 16 )
qqline( g$res )
# Andamento non rettilineo, l'ipotesi non è verificata

# Alternatively, we can plot
#x11()
plot(g, which=1 ) 

# Shapiro-Wilk normality test
shapiro.test( g$res ) # 2.2 e-16
# Since p-value is very low, I have evidence to reject $H_0$.

############################################################################################################################

# b. Modello passato per leverages
#x11()
plot( g_lev$fit, g_lev$res, xlab = "Fitted", ylab = "Residuals", 
      main = "Residuals vs Fitted Values - Lavarages", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )  # variabilità sembra sufficientemente uniforme

# Normality
# QQ plot
#x11()
qqnorm( g_lev$res, ylab = "Raw Residuals", main = "Normal Q-Q Plot - Lavarages", pch = 16 )
qqline( g_lev$res )
# Andamento non rettilineo, l'ipotesi non è verificata

# Shapiro-Wilk normality test
shapiro.test( g_lev$res ) # 2.2e-16
# Since p-value is very low, I have evidence to reject $H_0$.

############################################################################################################################

# c. Modello passato per residui
#x11()
plot( g_star$fit, g_star$res, xlab = "Fitted", ylab = "Residuals", 
      main = "Residuals vs Fitted Values - Residuals", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )  # variabilità sembra sufficientemente uniforme

# Normality
# QQ plot
#x11()
qqnorm( g_star$res, ylab = "Raw Residuals", main = "Normal Q-Q Plot - Residuals", pch = 16 )
qqline( g_star$res )
# Andamento non rettilineo, l'ipotesi è verificata

# Shapiro-Wilk normality test
shapiro.test( g_star$res ) # 5.813 e-05
# Since p-value is very low, I have evidence to reject $H_0$.

############################################################################################################################

# d. Modello passato per Cook
#x11()
plot( g_C$fit, g_C$res, xlab = "Fitted", ylab = "Residuals", 
      main = "Residuals vs Fitted Values - Cook", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )  # variabilità sembra sufficientemente uniforme

# Normality
# QQ plot
#x11()
qqnorm( g_C$res, ylab = "Raw Residuals", main = "Normal Q-Q Plot - Cook", pch = 16 )
qqline( g_C$res )
# Andamento quasi rettilineo, l'ipotesi non è verificata

# Shapiro-Wilk normality test
shapiro.test( g_C$res ) # 0.0001963
# Since p-value is very low, I have evidence to reject $H_0$.

############################################################################################################################

# e. Modello passato per tutto
#x11()
plot( g_C_new$fit, g_C_new$res, xlab = "Fitted", ylab = "Residuals", 
      main = "Residuals vs Fitted Values - Tutti", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )  # variabilità sembra sufficientemente uniforme

# Normality
# QQ plot
#x11()
qqnorm( g_C_new$res, ylab = "Raw Residuals", main = "Normal Q-Q Plot - Tutti", pch = 16 )
qqline( g_C_new$res )
# Andamento adeguatamente rettilineo, l'ipotesi è verificata

# Shapiro-Wilk normality test
shapiro.test( g_C_new$res ) # 0.02556
# Since p-value is very high, I have no evidence to reject $H_0$, which is the gaussianity of the data.

############################################################################################################################

# 4. BOX COX PER NORMALITA' DEI RESIDUI 

# a. modello passato per Cook
#x11()
b = boxcox( g_C, lambda = seq(-3,3,by=0.01) )
best_lambda_ind = which.max( b$y )
best_lambda = b$x[ best_lambda_ind ]
best_lambda

# lambda = 0.77
# Applico trasformazione logaritmica alla variabile risposta
mod = lm( (median_house_value^(best_lambda)-1)/best_lambda ~ longitude + latitude + longitude*latitude + population + households + median_income, data = dati_C )
summary(mod) # Radj = 0.789

#x11()
qqnorm( mod$residuals, main = "Normal Q-Q Plot - Box Cox - Cook" )
qqline( mod$residuals, col = 'blue' )
mod_res = mod$residuals/summary(mod)$sigma
plot( dati_C$median_house_value, mod_res, xlab = 'Median house value',  ylab = 'Standarzized residuals'  )

indici=sample(1:length(dati_2$latitude), 994, replace=F)
dati_compatti=dati[indici,]
mod = lm( (median_house_value^(best_lambda)-1)/best_lambda ~ latitude + population + households + median_income, data = dati_C )
shapiro.test( residuals( mod ) ) # 0.09069

############################################################################################################################

# b. Modello passato per tutti
#x11()
b = boxcox( g_C_new, lambda = seq(-3,3,by=0.01) )
best_lambda_ind = which.max( b$y )
best_lambda = b$x[ best_lambda_ind ]
best_lambda

# lambda = 0.75
# Applico trasformazione logaritmica alla variabile risposta
mod = lm( (median_house_value^(best_lambda)-1)/best_lambda ~ longitude + latitude + longitude*latitude + population + households + median_income, data = dati_C_new )
summary(mod)

#x11()
qqnorm( mod$residuals )
qqline( mod$residuals, col = 'blue', main = "Normal Q-Q Plot - Box Cox - Tutti" )
mod_res = mod$residuals/summary(mod)$sigma
plot( dati_C_new$median_house_value, mod_res, xlab = 'Median house value',  ylab = 'Standarzized residuals'  )

indici=sample(1:length(dati_2$latitude), 812, replace=F)
dati_compatti=dati[indici,]
mod = lm( (median_house_value^(best_lambda)-1)/best_lambda ~ longitude + latitude + longitude*latitude + population + households + median_income, data = dati_C_new )
shapiro.test( residuals( mod ) ) # 0.07355

############################################################################################################################

## 9. Prediction

# a. Compute the Confidence Interval for the prediction of the average outcome.
longitude = na.omit( dati_C_new[ , c( 1 )] )
latitude = na.omit( dati_C_new[ , c( 2 )] )
population = na.omit( dati_C_new[ , c( 3 )] )
households = na.omit( dati_C_new[ , c( 4 )] )
median_income = na.omit( dati_C_new[ , c( 5 )] )
median_house_value = na.omit( dati_C_new[ , c( 6 )] )

grid_0 = seq( min( longitude ), max( longitude ), (max( longitude ) - min( longitude ))/100 )
grid_1 = seq( min( latitude ), max( latitude ), (max( latitude ) - min( latitude ))/100 )
grid_2 = seq( min( population ), max( population ), (max( population ) - min( population ))/100 )
grid_3 = seq( min( households ), max( households ), (max( households ) - min( households ))/100 )
grid_4 = seq( min( median_income ), max( median_income ), (max( median_income ) - min( median_income ))/100 )
grid = c( t(grid_0), t(grid_1), t(grid_2), t(grid_3), t(grid_4) ) 
grid = cbind( grid_0, grid_1, grid_2, grid_3, grid_4 ) 

# automatic prediction
y.pred = predict.lm( g_C_new, data.frame( longitude = grid_0, latitude = grid_1, population = grid_2, households = grid_3, median_income = grid_4 ), interval = "confidence", se = T )
names( y.pred )
y.pred$fit

# standard error
y.pred$se

y = y.pred$fit[, 1]
y_inf = y.pred$fit[, 2]
y_sup = y.pred$fit[, 3]

IC = cbind( y, y_inf, y_sup )
IC

# Plot the CI of predictions.
for( i in 1:5 )
{ 
  #x11()
  matplot( grid[,i], y.pred$fit, lty = c( 1, 2, 2 ), 
           col = c( 1, 'blue', 'blue' ), type = "l", xlab = "altezza",
           ylab = "peso", main = 'IC per la media della risposta' )
  points( median_house_value, na.omit( dati_C_new[ , c( i )] ), col = "black", pch = 16 )
}
############################################################################################################################

# a. Compute the Confidence Interval for the prediction of the average outcome.
latitude = na.omit( dati_C_new[ , c( 1 )] )
median_house_value = na.omit( dati_C_new[ , c( 5 )] )

grid_1 = seq( min( latitude ), max( latitude ), (max( latitude ) - min( latitude ))/100 )
population = na.omit( dati_C_new[ , c( 2 )] )
grid_2 = seq( min( population ), max( population ), (max( population ) - min( population ))/100 )
households = na.omit( dati_C_new[ , c( 3 )] )
grid_3 = seq( min( households ), max( households ), (max( households ) - min( households ))/100 )
median_income = na.omit( dati_C_new[ , c( 4 )] )
grid_4 = seq( min( median_income ), max( median_income ), (max( median_income ) - min( median_income ))/100 )
grid = c( t(grid_1), t(grid_2), t(grid_3), t(grid_4) ) 
grid = cbind( grid_1, grid_2, grid_3, grid_4 ) 

# automatic prediction
y.pred = predict( g_C_new, data.frame( latitude = grid_1, population = grid_1, households = grid_1, median_income = grid_4 ), interval = "confidence", se = T )
names( y.pred )
y.pred$fit

# standard error
y.pred$se

y = y.pred$fit[, 1]
y_inf = y.pred$fit[, 2]
y_sup = y.pred$fit[, 3]

IC = cbind( y, y_inf, y_sup )
IC

# Plot the CI of predictions.
#x11()
matplot( grid, y.pred$fit, lty = c( 1, 2, 2 ), 
         col = c( 1, 'blue', 'blue' ), type = "l", xlab = "altezza",
         ylab = "peso", main = 'IC per la media della risposta' )
points( median_house_value, latitude, col = "black", pch = 16 )

############################################################################################################################

# b. Compute the Prediction Interval for the one new observation. In this case the standard errors are:

y.pred2 = predict.lm( g_C_new, data.frame( latitude = grid_1, population = grid_1, households = grid_1, median_income = grid_4 ), interval = "prediction", se = T )
# fornisce direttamente gli estremi inf e sup, che prima abbiamo costruito a mano (in un altro caso)

# standard error
y.pred2$se.fit

y = y.pred2$fit[, 1]
y_inf = y.pred2$fit[, 2]
y_sup = y.pred2$fit[, 3]

IP = cbind( y, y_inf, y_sup )
IP

#x11()
matplot( grid, y.pred2$fit, lty = c( 1, 2, 2 ), col = c( 1, 2, 2 ), type = "l",
         xlab = "altezza", ylab = "peso", main = 'IP per singole osservazioni' )
points( median_house_value, latitude, col = "blue", pch = 16 )

############################################################################################################################

# c. Compare the Intervals obtained at 9.a. and 9.b.
#x11()
matplot( grid, y.pred2$fit, lty = c( 1, 2, 2 ), col = c( 1, 2, 2 ), type = "l", xlab = "altezza", ylab = "peso", 
         main = "IC per la media e IP per singole osservazioni" )
lines( grid, y.pred$fit[ , 2 ] , col = "blue", lty = 2, xlab = "altezza", ylab = "peso" )
lines( grid, y.pred$fit[ , 3 ] , col = "blue", lty = 2, xlab = "altezza", ylab = "peso" )
points( median_house_value, latitude, col = "black", pch = 16 )

# According to theory, the prediction interval is broader that the confidence interval (see the standard errors) 
# and all the points are inside the prediction interval, while only few of them are inside the confidence interval.

############################################################################################################################
