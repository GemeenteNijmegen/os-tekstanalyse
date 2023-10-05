
#-----------------------------------------------------------------------------------------------

# Reporting sentiment and subjectivity by sentence cluster

#-----------------------------------------------------------------------------------------------

#Cluster membership distribution 
plot.title = paste0('Cluster membership distribution sentences')
cluster_dis<-ggplot(sent_txt, aes(factor(best_cl), fill=factor(best_cl))) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  labs(x = "cluster", y="sentences (%)",title=plot.title, subtitle = paste0('wordembedding: ', scope,', optimal clustering method: ', best_method), fill="best_cl" ) +
  theme_minimal() + 
  theme(legend.position = "none") +
  scale_fill_viridis_d() 
cluster_dis
plot.nme = paste0(scope.prefix,'_',best_method_dr,'_',best_method_cl,'_cluster_distribution_sentences.png')
plot.store <-paste0(report.loc,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * aspect_ratio, dpi=dpi, bg=bgcolor)

#Distribution of sentiment 
plot.title = paste0('Sentiment by sentence cluster')
cb1<-ggplot(sent_txt, aes(x=as.factor(best_cl), fill=as.factor(sentiment_bin3_labs)))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0.9), vjust=-0.5)+
  theme_minimal() + 
  scale_fill_viridis_d() +
  labs(title=plot.title, subtitle=paste0('wordembedding: ', scope,', optimal clustering method: ', best_method), fill="sentiment class" ) +
  xlab("cluster") +
  ylab('sentiment class, %') +
  scale_y_continuous(labels = scales::percent) 
cb1
plot.nme = paste0(scope.prefix,'_',best_method_dr,'_',best_method_cl,'_sentence_sentiment_distribution.png')
plot.store <-paste0(report.loc,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * aspect_ratio, dpi=dpi, bg=bgcolor)

#Distribution of subjectivity 
plot.title = paste0('Subjectivity by sentence cluster')
cb2<-ggplot(sent_txt, aes(x=as.factor(best_cl), fill=as.factor(subjectivity_bin3_labs)))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0.9), vjust=-0.5)+
  theme_minimal() + 
  scale_fill_viridis_d() +
  labs(title=plot.title, subtitle = paste0('wordembedding: ', scope,', optimal clustering method: ', best_method), fill="subjectivity class" ) +
  xlab("cluster") +
  ylab('subjectivity class, %') +
  scale_y_continuous(labels = scales::percent) 
cb2
plot.nme = paste0(scope.prefix,'_',best_method_dr,'_',best_method_cl,'_sentence_subjectivity_distribution.png')
plot.store <-paste0(report.loc,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * aspect_ratio, dpi=dpi, bg=bgcolor)

#Dispersion of subjectivity 
plot.title = paste0('Subjectivity by sentence cluster (score)')
bp1<-ggplot(sent_txt, aes(x = factor(best_cl), y =subjectivity_score,fill=factor(best_cl))) + 
  geom_boxplot(width=0.6) + 
  stat_summary(fun=mean, geom="point", shape=5, size=4) +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(title=plot.title, subtitle = paste0('wordembedding: ', scope,', optimal clustering method: ', best_method), fill="cluster" ) +
  xlab("cluster") +
  ylab("subjectivity")
bp1
plot.nme = paste0(scope.prefix,'_',best_method_dr,'_',best_method_cl,'_sentence_subjectivity_dispersion.png')
plot.store <-paste0(report.loc,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * aspect_ratio, dpi=dpi, bg=bgcolor)

#Dispersion of sentiment
plot.title = paste0('Sentiment by sentence cluster (score)')
bp2<-ggplot(sent_txt, aes(x = factor(best_cl), y =sentiment_score,fill=factor(best_cl))) + 
  geom_boxplot(width=0.6) + 
  stat_summary(fun=mean, geom="point", shape=5, size=4) +
  theme_minimal() + 
  scale_fill_viridis_d() +
  labs(title=plot.title, subtitle=paste0('wordembedding: ', scope,', optimal clustering method: ', best_method), fill="cluster" ) +
  xlab("cluster") +
  ylab("sentiment")
bp2
plot.nme = paste0(scope.prefix,'_',best_method_dr,'_',best_method_cl,'_sentence_sentiment_dispersion.png')
plot.store <-paste0(report.loc,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * aspect_ratio, dpi=dpi, bg=bgcolor)

message("For custom Ggplots, please use the drag-and-drop interface : see addins menu in RStudio > Ggplot2 builder")