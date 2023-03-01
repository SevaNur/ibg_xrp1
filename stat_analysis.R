#скрипт для подсчета статистики
#данные подаем уже отсортированные по возрастанию


#install a working environment
setwd('C:/Users/SevdaN/YandexDisk/LaGERinD/Users/Sevda/scripts')

#load and prepeare data
data<-read.csv('data_for_plot.csv', sep = ";")
list<-unique(data$name)
len<-length(list)


#проверяем данные на нормальность тест Колмогорова-Смирнова
for (i in 1:len) {
  name=list[i]
  cat(name)
  print(ks.test(data[data$name == name,]$distances, 'pnorm'))
}

#проверяем данные на нормальность тест Шапиро-Уилка
for (i in 1:len) {
  name=list[i]
  cat(name)
  print(shapiro.test(data[data$name == name,]$distances))
}


#оба теста показывают, что данные распределены не в соответствии с нормальным распределением
#поэтому используем непараметрический критерий Уилкоксона для сравнения облученных и необлученных выборок

i=1 #обнуляем i
while (i < len) {
  
  name1=list[i]
  name2=list[i+1]
  cat(name1, ' vs ', name2)
  print(wilcox.test(data[data$name == name1,]$distances, data[data$name == name2,]$distances))
  i=i+2
  
}

#используем непараметрический критерий Уилкоксона для сравнения с контролем

i=3 #обнуляем i
while (i < len) {
  
  name1=list[i]
  name2=list[i+1]
  cat(name1, ' vs ', list[1])
  print(wilcox.test(data[data$name == name1,]$distances, data[data$name == list[1],]$distances))
  cat(name2, ' vs ', list[2])
  print(wilcox.test(data[data$name == name2,]$distances, data[data$name == list[2],]$distances))
  i=i+2
  
}