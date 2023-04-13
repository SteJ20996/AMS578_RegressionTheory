# AMS 580 project code
# By Yizhen Jia, student ID 111520996
# Please run the code by parts

# Part (1) library packages
library(mice)
library(MASS)

# Part (2) Reading csv dataset, point out missing value, and report observations
yizjia = read.csv('~/Desktop/Spring 2022/AMS 578/578 proj yizjia/data_316445.csv',header = TRUE,sep = ",")
md.pattern(yizjia) # no missing
nrow(yizjia) # 2505 observations

# Part (3) Boxcox the dataset and calculate lambda
bc <- boxcox(yizjia$Y~.,data = yizjia)
lambda = bc$x[which.max(bc$y)]
lambda # 0.8686869

# Part (4) Using linear to get main coefficients
lin = lm((Y)^lambda ~.,data = yizjia) 
L = step(lin,direction = 'both')
summary(L) # denote E1 E4 E8, MRS = 0.5558, ARS = 0.5544

biq = lm((yizjia$Y)^lambda ~(E2*E3*E7*G20*G23),data = yizjia) 
Biq = step(biq,direction='both')
summary(Biq)

# Part (5) Using bi quadrature and coeff denoted in part (4) to get main coefficients
biq = lm((yizjia$Y)^lambda ~(E1+E4+E8)^4,data = yizjia) 
Biq = step(biq,direction='both')
summary(Biq) # denote no more, MRS = 0.554, ARS = 0.5528

plot(resid(Biq)~fitted(Biq), main = 'Residual Plot')