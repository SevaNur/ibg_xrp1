#������ ��� �������� ����������
#������ ������ ��� ��������������� �� �����������


#install a working environment
setwd('C:/Users/SevdaN/YandexDisk/LaGERinD/Users/Sevda/scripts')

#load and prepeare data
data<-read.csv('data_for_plot.csv', sep = ";")
list<-unique(data$name)
len<-length(list)


#��������� ������ �� ������������ ���� �����������-��������
for (i in 1:len) {
  name=list[i]
  cat(name)
  print(ks.test(data[data$name == name,]$distances, 'pnorm'))
}

#��������� ������ �� ������������ ���� ������-�����
for (i in 1:len) {
  name=list[i]
  cat(name)
  print(shapiro.test(data[data$name == name,]$distances))
}


#��� ����� ����������, ��� ������ ������������ �� � ������������ � ���������� ��������������
#������� ���������� ����������������� �������� ���������� ��� ��������� ���������� � ������������ �������

i=1 #�������� i
while (i < len) {
  
  name1=list[i]
  name2=list[i+1]
  cat(name1, ' vs ', name2)
  print(wilcox.test(data[data$name == name1,]$distances, data[data$name == name2,]$distances))
  i=i+2
  
}

#���������� ����������������� �������� ���������� ��� ��������� � ���������

i=3 #�������� i
while (i < len) {
  
  name1=list[i]
  name2=list[i+1]
  cat(name1, ' vs ', list[1])
  print(wilcox.test(data[data$name == name1,]$distances, data[data$name == list[1],]$distances))
  cat(name2, ' vs ', list[2])
  print(wilcox.test(data[data$name == name2,]$distances, data[data$name == list[2],]$distances))
  i=i+2
  
}