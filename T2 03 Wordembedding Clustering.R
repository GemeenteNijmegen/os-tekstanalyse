
#-----------------------------------------------------------------------------------------------

# T2 CLUSTERING

#-----------------------------------------------------------------------------------------------

# Clustering of observations 

#set pointer (words, documents)

for (lvl in lvl_start:2) {

  if(lvl==1) {
    message("Clustering words...")
    
    #feed reduced WORD space
    lvl_nme<-"words"
    #number of clusters (see T2 00 init Wordembedding.R)
    k<-kw
  } else {
    message("Clustering documents...")
    
    #feed reduced DOCUMENT space
    lvl_nme<-"documents"
    #number of clusters (automatic or set)
    k<-ks
  }  

#set pointer (tsne,umap)
dr<-1
  for (dr in 1:2) {
  
  if(dr==1) {
    dr_nme<-"TSNE"
    perplexity_lbl<-paste0("perplexity=",perplex)
    
    if(lvl==1) {
    srce_tmp<-tsne_store

    } else {
    srce_tmp<-tsne_sv_store %>%
    left_join(perception_doc, by="doc_id") 
    }
  } else {
    dr_nme<-"UMAP"
    perplexity_lbl<-""
    
    if(lvl==1) {
    srce_tmp<-umap_store
    } else {
      srce_tmp<-umap_sv_store  
    } 
  }

if(k<13) {   
#qualitative color scheme from ColorBrewer
colors_cust<-brewer.pal(k, "Paired")    
} else {
#so you need many colors? Let's  join all qualitative palettes 
qual_col_pals=brewer.pal.info[brewer.pal.info$category == 'qual',]
colors_cust=unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
}

#-----------------------------------------------------------------------------------------------
# 1 K-mediods (PAM)

#K-mediods (PAM) clustering method
pam_fit<-cluster::pam(subset(srce_tmp, select=all_of(c("X1","X2"))), k)

#mediods
#pam.med<-as.data.frame(pam_fit$medoids)
#pam.med$clus_color<-as.numeric(as.vector(1:k))

title=paste0(scope.prefix,' > ',dr_nme,' > K-mediods (PAM) of ',lvl_nme, ' ',scope.prefix, ' ',perplexity_lbl, ' k=',k, ' ellipse=norm')
pam_plot<-factoextra::fviz_cluster(pam_fit, geom="point", ellipse.type="norm", show.clust.cent=TRUE) +
  #geom_label_repel(aes(label=pam.med$clus_color), data=pam.med[,c("X1","X2")]) +
  theme_minimal() +
  labs(title=title)
(pam_plot) 
plot.nme=paste0(scope.prefix,'_',dr_nme,'_pam_',lvl_nme,'_k',k,'_p',perplex,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height=graph_height , width=graph_height * aspect_ratio,dpi=dpi)

#add clustermemberschip to TSNE dataframe
if(lvl==1) {
  #words
  if(dr==1) {
  tsne_store$tsne_cl_pam<-pam_fit$clustering
  } else {
  umap_store$umap_cl_pam<-pam_fit$clustering  
  }
} else {
  #sentences
  if(dr==1) {
  tsne_sv_store$tsne_cl_pam<-pam_fit$clustering
  } else {
  umap_sv_store$umap_cl_pam<-pam_fit$clustering  
  }
}

#-----------------------------------------------------------------------------------------------
# 2 TSNE > Kmeans 

#Kmeans clustering methods (kmeans, spherical, kmeans++)
#Kmeans is more prone to outliers than PAM

km<-1
#20220830 skipped km=3 : kmeanspp cos LICORS package is removed from CRAN
for (km in 1:2) {  
  
  if("clus_color" %in% colnames(srce_tmp))
  {
    srce_tmp$clus_color<-NULL
  }
  
  if(km==1) {
    #kmeans
    km_meth<-''
    kmeans_fit=stats::kmeans(subset(srce_tmp, select=all_of(c("X1","X2"))), k,iter.max=1000,algorithm=c("Forgy"))  
  } else if (km==2) {
    #spherical kmeans
    km_meth<-'spherical'
    srce_tmp2<- as.matrix(srce_tmp[,c("X1","X2")])
    
    #Spherical k-Means Clustering 
    kmeans_fit=skmeans::skmeans(as.matrix(srce_tmp[,c("X1","X2")]), k, m=1, weights=1, control=list())
  } else if (km==3) {
    #kmeans++
    km_meth<-'km++'
    kmeans_fit=LICORS::kmeanspp(subset(srce_tmp, select=all_of(c("X1","X2"))), k, start="random", iter.max=1000)  
  } else {
    message ("no clustering method available")
  }
 
#add clustermemberschip to dataframe

if(lvl==1) {
  #words
  if(km==1) { 
    if(dr==1) {
    tsne_store$tsne_cl_kmeans<-kmeans_fit$cluster
    srce_tmp$clus_color<-kmeans_fit$cluster
    } else {
      umap_store$umap_cl_kmeans<-kmeans_fit$cluster
      srce_tmp$clus_color<-kmeans_fit$cluster  
    }
  } else if (km==2) {
    if(dr==1) {
    tsne_store$tsne_cl_spherickmeans<-kmeans_fit$cluster
    srce_tmp$clus_color<-kmeans_fit$cluster
    } else {
      umap_store$umap_cl_spherickmeans<-kmeans_fit$cluster
      srce_tmp$clus_color<-kmeans_fit$cluster  
    }
  } else if (km==3) {
    if(dr==1) {
    tsne_store$tsne_cl_kmeanspp<-kmeans_fit$cluster
    srce_tmp$clus_color<-kmeans_fit$cluster
    } else {
      umap_store$umap_cl_kmeanspp<-kmeans_fit$cluster
      srce_tmp$clus_color<-kmeans_fit$cluster  
    }
  } else {}
} else {
  #sentences  
  if(km==1) { 
    if(dr==1) {
    tsne_sv_store$tsne_cl_kmeans<-kmeans_fit$cluster
    srce_tmp$clus_color<-kmeans_fit$cluster
    } else {
      umap_sv_store$umap_cl_kmeans<-kmeans_fit$cluster
      srce_tmp$clus_color<-kmeans_fit$cluster  
    }
  } else if (km==2) {
    if(dr==1) {
    tsne_sv_store$tsne_cl_spherickmeans<-kmeans_fit$cluster
    srce_tmp$clus_color<-kmeans_fit$cluster
    } else {
      umap_sv_store$umap_cl_spherickmeans<-kmeans_fit$cluster
      srce_tmp$clus_color<-kmeans_fit$cluster  
    }
  } else if (km==3) {
    if(dr==1) {
    tsne_sv_store$tsne_cl_kmeanspp<-kmeans_fit$cluster
    srce_tmp$clus_color<-kmeans_fit$cluster
    } else {
      umap_sv_store$umap_cl_kmeanspp<-kmeans_fit$cluster
      srce_tmp$clus_color<-kmeans_fit$cluster  
    }
  } else {}
}

#centroids
km.cent<-srce_tmp %>% group_by(clus_color) %>% select(X1, X2) %>% summarize_all(mean)  
  
## plotting the results with Kmeans clustering
plot.title=paste0(scope.prefix,' > ',dr_nme, ' > ',km_meth,' Kmeans of ',lvl_nme, ' ',scope.prefix, ' ',perplexity_lbl, ' k=',k, ' ellipse=norm')
tsne_plot<-ggplot(srce_tmp, aes(X1, X2, 
                                  color=as.factor(clus_color))) +
  geom_point(shape=srce_tmp$clus_color,na.rm=FALSE,
              show.legend=TRUE) + 
  geom_label_repel(aes(label=clus_color), 
                     data=km.cent) +
  stat_ellipse(type="norm", linetype=2) +
  stat_ellipse(type="t") +
  ggtitle(plot.title) +
  theme_minimal() +
  xlab('') +
  ylab('') +
  #geom_text(aes(label=row.names(X)),color="#ababab") +
  scale_colour_manual(name="cluster",values=colors_cust) + 
  scale_shape_manual(values=c(0:40)) + 
  geom_text(aes(label=""), size=3, vjust=1, color="black")
(tsne_plot) 
plot.nme=paste0(scope.prefix,'_',dr_nme,'_',km_meth,'kmeans_',lvl_nme,'_k',k,'_p',perplex,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height=graph_height , width=graph_height * aspect_ratio,dpi=dpi)
}

  if(lvl==1) {
    tsne_ss<-tsne_store
    umap_ss<-umap_store
  } else {
    tsne_ss<-tsne_sv_store
    umap_ss<-umap_sv_store
  }
}
 
#-----------------------------------------------------------------------------------------------

# Compare cluster methods for words (for TSNE and UMAP)

#-----------------------------------------------------------------------------------------------

#within.cluster.ss measurement shows how closely related observations are in 
#clusters; the smaller the value, the more closely related are observations within the cluster

#avg.silwidth is a measurement that considers how closely related objects 
#are within the cluster and how clusters are separated from each other.

  #TSNE  
  #PAM on TSNE
  cs1_1=fpc::cluster.stats(stats::dist(tsne_ss),as.numeric(tsne_ss$tsne_cl_pam))
  (silwidth_pam_tsne<-cs1_1[c("within.cluster.ss","avg.silwidth")])
  
  #Kmeans on TSNE
  cs1_2=fpc::cluster.stats(stats::dist(tsne_ss),as.numeric(tsne_ss$tsne_cl_kmeans))
  (silwidth_kmeans_tsne<-cs1_2[c("within.cluster.ss","avg.silwidth")])
  
  #Spherical Kmeans on TSNE
  cs1_3=fpc::cluster.stats(stats::dist(tsne_ss),as.numeric(tsne_ss$tsne_cl_spherickmeans))
  (silwidth_spherickmeans_tsne<-cs1_3[c("within.cluster.ss","avg.silwidth")])
  
  #Kmeans++ on TSNE
  #cs1_4=fpc::cluster.stats(stats::dist(tsne_ss),as.numeric(tsne_ss$tsne_cl_kmeanspp))
  #(silwidth_kmeanspp_tsne<-cs1_4[c("within.cluster.ss","avg.silwidth")])
  
  #UMPA
  #K-mediods on UMAP
  #PAM on Umap
  cs2_1=fpc::cluster.stats(stats::dist(umap_ss),as.numeric(umap_ss$umap_cl_pam))
  (silwidth_pam_umap<-cs2_1[c("within.cluster.ss","avg.silwidth")])
  
  #Kmeans on UMAP
  cs2_2=fpc::cluster.stats(stats::dist(umap_ss),as.numeric(umap_ss$umap_cl_kmeans))
  (silwidth_kmeans_umap<-cs2_2[c("within.cluster.ss","avg.silwidth")])
  
  #Spherical Kmeans on UMAP
  cs2_3=fpc::cluster.stats(stats::dist(umap_ss),as.numeric(umap_ss$umap_cl_spherickmeans))
  (silwidth_spherickmeans_umap<-cs2_3[c("within.cluster.ss","avg.silwidth")])
  
  #Kmeans++ on UMAP
  #cs2_4=fpc::cluster.stats(dist(umap_ss),as.numeric(umap_ss$umap_cl_kmeanspp))
  #(silwidth_kmeanspp_umap<-cs2_4[c("within.cluster.ss","avg.silwidth")])
  
  
  (silwidth<-as.data.frame(rbind(silwidth_pam_tsne,silwidth_kmeans_tsne,silwidth_spherickmeans_tsne,
                                   #silwidth_kmeanspp_tsne,
                                   silwidth_pam_umap,silwidth_kmeans_umap,silwidth_spherickmeans_umap
                                   #,silwidth_kmeanspp_umap
                                   )))
  
  silwidth$within.cluster.ss<-as.numeric(silwidth$within.cluster.ss) 
  silwidth$methods<-row.names(silwidth)
  silwidth$methods<-as.factor(silwidth$methods)  
  
  #Plot consistency of observations within cluster 
  plot.title=paste0('Measurement of consistency ',lvl_nme,' within cluster based on ',scope.prefix)
  cluster_ss<-ggplot(silwidth, aes(x=methods, y=within.cluster.ss)) +
    ggtitle(plot.title) +
    labs(x="Clustering method", y="Within.cluster.ss (lower is better)") +
    geom_boxplot()
  (cluster_ss)
  plot.nme=paste0(scope.prefix,'_within_cluster_ss_best_method_',lvl_nme,'.png')
  plot.store <-paste0(plots.loc,plot.nme)
  ggsave(plot.store, height=graph_height , width=graph_height * aspect_ratio,dpi=dpi)
  
  
  #-----------------------------------------------------------------------------------------------
  # Best combination of methods
  
  best_method<-row.names(silwidth[silwidth$within.cluster.ss == min(silwidth$within.cluster.ss),])
  best_method<-gsub("_"," ",best_method)
  best_method<-trimws(gsub("silwidth","",best_method))
  best_method
  
  best_method_dr_umap<-grepl("umap",best_method)
  ifelse(best_method_dr_umap==TRUE,best_method_dr<-"UMAP",best_method_dr<-"TSNE")
  
  message("Best dimention reduction method ", lvl_nme ," : ", best_method_dr)
  
  best_method_cl_pam<-grepl("pam",best_method)
  best_method_cl_pam
  best_method_cl_skm<-grepl("spheric",best_method)
  best_method_cl_skm
  #best_method_cl_kmp<-grepl("kmeanspp",best_method)
  #best_method_cl_kmp
  
  #whatever...
  ifelse(best_method_cl_pam==TRUE,best_method_cl<-"PAM",ifelse(best_method_cl_skm==TRUE,best_method_cl<-"SPHERICKMEANS",best_method_cl<-"KMEANS"))
                                                              # ifelse(best_method_cl_kmp==TRUE,best_method_cl<-"KMEANSPP",best_method_cl<-"KMEANS")))

  message("Best clustering method ", lvl_nme ," : ", best_method_cl)
  
  #-----------------------------------------------------------------------------------------------
  # Writing cluster membership to csv
  
  if(lvl==1) {
    #words
    wv_store<-umap_store %>% 
      select(-cols, -tfidf_mn, -total_sum, -lemma, -upos, -xpos,-subjectivity) %>%
      rename(X1_UMAP=X1, X2_UMAP=X2) %>%
      cbind(tsne_store) %>% 
      rename(X1_TSNE=X1, X2_TSNE=X2) %>%
      rownames_to_column('words')
    
    cluster_words<-paste0(data.loc,"cluster-membership-words-",scope.prefix,"-k",k,"-p",perplex,".csv")
    write.csv(wv_store, file=cluster_words,row.names=FALSE)
  }
  
  if(lvl==2) {
    #sentences 
    sv_store<-umap_sv_store  %>%
      select(-cols)  %>%
      rename(X1_UMAP=X1, X2_UMAP=X2) %>% 
      left_join(tsne_sv_store, by="doc_id") %>% rename(X1_TSNE=X1, X2_TSNE=X2)
   
    # %>% rownames_to_column('doc_id')
    cluster_sentences<-paste0(data.loc,"cluster-membership-sentences-",scope.prefix,"-k",k,".csv")
    write.csv(sv_store, file=cluster_sentences,row.names=FALSE)
    
    annotate_clusters<-annotate_df %>%
      left_join(sv_store, by="doc_id")
  
  }  
  
}

#garbage collecting
objects_to_remove <- c("tsne_ss","umap_ss")
remove_objects(objects_to_remove)
