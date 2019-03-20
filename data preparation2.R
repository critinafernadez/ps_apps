library("tidyverse")
library(tidyr)
data=read.csv("googleplaystore_user_reviews.csv")
(head(data))
class(data)
(summary(data$App))

# elimino la segunda columna
# quito las filas con NA

data2=data %>%
  drop_na() %>%
  #group_by(Sentiment)%>%
  select(-Translated_Review,-Sentiment_Polarity,-Sentiment_Subjectivity)
#columnas guya para agrupar por appsssss
valor = data2 %>% group_by(App)%>%mutate(countpos = sum(Sentiment=='Positive')) %>%
  mutate(countneut = sum(Sentiment=='Neutral')) %>% 
  mutate(counteg = sum(Sentiment=='Negative'))%>%select(-Sentiment)

valor2= valor[!duplicated(valor),]
tablefinal=valor2[-2,]



