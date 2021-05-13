#찐 최종..

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

total =  fread('이상치 삭제.csv', data.table=F)

hc <- scale(total[,c(3,4)])
d = dist(hc)

fit <- hclust(d, method='ward.D')

plot(fit, hang=-1, cex=0.1)
nc <- NbClust(hc, distance="euclidean", method="ward.D")
plot_total= plot(fit, hang=-10, k=4)

plot_total2 = rect.hclust(fit, k=4)
clusters <- cutree(fit, k=4)
table(clusters)
clusters = as.data.frame(clusters)

total_2 <- cbind(total, clusters)
total_2 <- as.data.frame(total_2)

fviz_nbclust(total_2, FUN= hcut, method = "silhouette", cex=1)  

clusplot(total_2, clus = total_2$clusters , color=TRUE, lines=0, cex=1)

#추가해볼게
total_2 %>% group_by(행정동) %>% summarise(mean_1 = mean(factor1), mean_2 = mean(학교제외))

k = kmeans(total_2, 3)

k$cluster

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
total_2[(total_2$행정구 == '관악구') & (total_2$행정동 == '신사동'), 'dong'] = '신사동_관'
total_2[(total_2$행정구 == '강남구') & (total_2$행정동 == '신사동'), 'dong'] = '신사동_강'


dong_info[(dong_info$시군구명 == '관악구') & (dong_info$행정동명 == '신사동'), '행정동명'] = '신사동_관'
dong_info[(dong_info$시군구명 == '강남구') & (dong_info$행정동명 == '신사동'), '행정동명'] = '신사동_강'



# 행정동코드 숫자형태로 바꿔주기
dong_info = dong_info %>%
  mutate(행정동코드 = as.integer(통계청행정동코드)) %>%
  dplyr::select(행정동코드,시군구명,행정동명)

# 만든 데이터셋과 행정동 메타데이터: 항동 제외하고는 행정동명 일치!
setdiff(dong_info$행정동명,total_2$행정동)
setdiff(total_2$행정동,dong_info$행정동명)


map_data = left_join(dong_info, seoul_map, by=c('행정동코드'='id'))

options(scipen=999)




# 보호기관수 시각화
map_data %>%
  left_join(total_2,by = c('행정동명' = '행정동')) %>%
  ggplot(aes(x = long,y = lat,group = group)) +
  geom_polygon(aes(fill =factor1),color = 'lightgrey') +
  scale_fill_gradient2('value',low = 'blue', high = 'pink') +
  scale_color_gradient2('value',low = 'red', high = 'blue') +
  ggtitle('위험도지수 시각화') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill='white', color='black', linetype='solid'))
