#building a violin plot with ggplot2 
library(dplyr)
library('ggplot2')

#install a working environment
setwd('C:/Users/SevdaN/YandexDisk/LaGERinD/Users/Sevda/scripts')
data<-read.csv('data_for_plot.csv', sep = ";")

#load and prepare data
sample_info = aggregate(data$distances, list(data$name), median)
colnames(sample_info)=c('name', 'mediana')
sample_info= merge(x= data %>% group_by(name) %>% summarize(num=n()),
                   y = sample_info)

#plot without p-value
data %>%
  left_join(sample_info) %>%
  mutate(myaxis = paste0(name, "\n", "n=", num, "\n", "mediana=", mediana)) %>%
  ggplot( aes(x=myaxis, y=distances, fill=name), ) +
  ggtitle("Violin plots representing 3D DNA FISH distances")+
  theme(text = element_text (family = "Tahoma", face="bold", colour="black", size=14))+
  theme(plot.title = element_text(hjust = 0.5, size=20))+
  geom_violin(width=1, color="darkgrey")+
  labs(x='',y = "distances (µm)")+
  geom_boxplot(outlier.alpha = 0, add="mean_se", coef = 0, width=0.1, color="#363636") +
  scale_fill_manual(values = c("#8B1C62","#CD2990","#8B3A3A","#CD5555","#36648B","#4F94CD","#458B74","#66CDAA","#8B7355","#CDAA7D"))

#plot without p-value
# data %>%
#   left_join(sample_info) %>%
#   mutate(myaxis = paste0(name, "\n", "n=", num, "\n", "mediana=", mediana)) %>%
#   ggplot( aes(x=myaxis, y=distances, fill=name), ) +
#   ggtitle("Violin plots representing 3D DNA FISH distances")+
#   theme(text = element_text (family = "Tahoma", face="bold", colour="black", size=14))+
#   theme(plot.title = element_text(hjust = 0.5, size=20))+
#   geom_violin(width=1, color="darkgrey")+
#   labs(x='',y = "distances (µm)")+
#   geom_boxplot(outlier.alpha = 0, add="mean_se", coef = 0, width=0.1, color="#363636") +
#   scale_fill_manual(values = c("#8B1C62","#CD2990","#8B3A3A","#CD5555","#36648B","#4F94CD","#458B74","#66CDAA","#8B7355","#CDAA7D"))+
#   
  
  