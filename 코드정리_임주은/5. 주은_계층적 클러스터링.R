#dist 함수는 데이터 간 거리 계산 - 거리가 멀수록 차이가 큼

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
library(reshape)
library(flexclust)
library(NbClust)
library(factoextra)
library(cluster)

total =  fread('dong_efa.csv', data.table=F)
dc = fread('total_dong_new.csv',data.table=F)

dc = dc[,c(2, 11)]
colnames(dc) = c('동','대체기관수')
total[,8] = total[,8]/100
total = total[,c(1,2,8)]
real_total = left_join(total, dc, by = '동')

hc_1 <- scale(real_total[,c(2,3)])
d = dist(hc_1)

fit <- hclust(d, method='average')
plot(fit, hang=-2, cex=0.1)
nc <- NbClust(hc_1, distance="euclidean", method="average")
plot_total= plot(fit, hang=-10, k=2)

plot_total2 = rect.hclust(fit, k=2)
clusters <- cutree(fit, k=2)
table(clusters)
clusters = as.data.frame(clusters)

total_final <- cbind(real_total, clusters)
total_final <- as.data.frame(total_final)


#아직 : ggplot(cl, aes(x=동, y=기초수급가구비율, color=factor(cluster)))+geom_point()
fviz_nbclust(total_final, FUN= hcut, method = "silhouette")  



#시각화
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(ggplot2)

map = readOGR('C:/Users/Jooeun/Desktop/Z_SOP_BND_ADM_DONG_PG/Z_SOP_BND_ADM_DONG_PG.shp')

new_map = fortify(map, region = 'ADM_DR_CD')
#일단 여기서 오류남

new_map$id = as.numeric(new_map$id)
seoul_map = new_map[new_map$id <= 1174099,]

library(readxl)
library(dbplyr)
library(dplyr)

dong_info = read_excel('행정동코드_매핑정보_2018.xlsx',sheet=1,col_names = TRUE) %>% as.data.frame()
dong_info = dong_info[-1,] 

#신사동 구분
total_final[(total_final$행정구 == '관악구') & (total_final$행정동 == '신사동'), 'dong'] = '신사동_관'
total_final[(total_final$행정구 == '강남구') & (total_final$행정동 == '신사동'), 'dong'] = '신사동_강'


dong_info[(dong_info$시군구명 == '관악구') & (dong_info$행정동명 == '신사동'), '행정동명'] = '신사동_관'
dong_info[(dong_info$시군구명 == '강남구') & (dong_info$행정동명 == '신사동'), '행정동명'] = '신사동_강'



# 행정동코드 숫자형태로 바꿔주기
dong_info = dong_info %>%
  mutate(행정동코드 = as.integer(통계청행정동코드)) %>%
  dplyr::select(행정동코드,시군구명,행정동명)

# 만든 데이터셋과 행정동 메타데이터: 항동 제외하고는 행정동명 일치!
setdiff(dong_info$행정동명,total_final$행정동)
setdiff(total_final$행정동,dong_info$행정동명)


map_data = left_join(dong_info, seoul_map, by=c('행정동코드'='id'))

options(scipen=999)




# 보호기관수 시각화
map_data %>%
  left_join(total_final,by = c('행정동명' = '동')) %>%
  ggplot(aes(x = long,y = lat,group = group)) +
  geom_polygon(aes(fill =위험도지수),color = 'lightgrey') +
  scale_fill_gradient2('value',low = 'blue', high = 'pink') +
  scale_color_gradient2('value',low = 'red', high = 'blue') +
  ggtitle('위험도지수 시각화') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill='white', color='black', linetype='solid'))

