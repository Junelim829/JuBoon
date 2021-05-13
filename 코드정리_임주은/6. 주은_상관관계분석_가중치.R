getwd()
setwd("C:/Users/Jooeun/Desktop")

library(data.table)
library(tidyverse)
library(caret)
library(wCorr)
library(corrplot)
library(GGally)
library(car)
library(fmsb)
library(gvlma)


total =  fread('total_gu_new.csv', data.table=F)
total = total[,c(1,2,6,7,8,9)]
total_name = total[,1]


colnames(total) = c('gu','a','b','c','d','e', 'f')


lm_fit <- lm(a~b+c+d+e+f , data = total)
summary(lm_fit)



cor(a, b+c+d+e+f)
vif(lm_fit)


cor = cor(total[,c(2,3,4,5,6)], use='all.obs', method='pearson')
as.data.frame(cor)
corrplot(cor, method='number')
cor

A = abs(colSums(cor))
A = as.data.frame(A)
B = A[1,]

b_cor = cor[1,2]/B
c_cor = cor[1,3]/B
d_cor = cor[1,4]/B
e_cor = cor[1,5]/B


wow = c(b_cor, c_cor, d_cor, e_cor)
wow = as.data.frame(wow)

rownames(wow) = c('5천만원이상비율','CCTV 비율','아동수','기초수급가구비율')
colnames(wow) = c('가중치')

wow = t(wow)
as.data.frame(wow)
write.csv(wow, '가중치.csv')





total_dong =  fread('total_dong_new.csv', data.table=F)
total_name2 = total_dong[,2]

final = (wow[,1]*total_dong[,6] + wow[,2]*total_dong[,7] + wow[,3]*total_dong[,8] + wow[,4]*total_dong[,9])
#final = abs(final)
#final = 1- final
final = as.data.frame(final)

colnames(final) = c('위험도 지수')

risk2 <- cbind(total_name2, final)
colnames(risk2) = c('동','위험도 지수')


risk2[,2] = risk2[,2]*100

write.csv(risk2, '동별 위험도 지수.csv')

#오름차순
risk3 <- risk2[,2]
risk3 = sort(risk3)
risk3 = as.data.frame(risk3)
colnames(risk3) = c('위험도 지수')
risk_down = left_join(risk3, risk2, by='위험도 지수')
risk_down = risk_down[-3,]
head(risk_down)

#내림차순
risk4 <- risk2[,2]
risk4 = sort(risk4, decreasing=TRUE)
risk4 = as.data.frame(risk4)
colnames(risk4) = c('위험도 지수')
risk_up = left_join(risk4, risk2, by='위험도 지수')
head(risk_up)



