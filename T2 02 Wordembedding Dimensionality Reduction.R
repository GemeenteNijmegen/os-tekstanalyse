
#-----------------------------------------------------------------------------------------------

# T2 DIMENSIONALITY REDUCTION

#----------------------------------------------------------------------------------------------

# Reduce n-dimensional space -as represented by vectors- to 2 dimensions

#----------------------------------------------------------------------------------------------
# two methods : TSNE and UMAP 
#-----------------------------------------------------------------------------------------------

for (lvl in lvl_start:2) {
  
  if(lvl==1) {
    #words
    dr_df<-train_dr_df
    lvl_nme<-"words"
    
    message("Dimensionality reduction word vectors...")
  } else {
    #documents
    if(scope!='BERT') {
    vec_sum<-rowSums( sv_wei_df[,cols] )
    
    sv_wei_df$vec_sum<-vec_sum
    
    dr_df<-sv_wei_df %>%
      filter(vec_sum!=0) %>%
      select(-vec_sum)
    } else {
      
      dr_df<-sv_wei_df
    }
    
    lvl_nme<-"documents"
    message("Dimensionality reduction document vectors...")
  }

    
#-----------------------------------------------------------------------------------------------
# 1 T-Distributed Stochastic Neighbor Embedding (TSNE)
#-----------------------------------------------------------------------------------------------

#TSNE is a non-linear technique for dimensionality reduction 
#well-suited for the visualization of high-dimensional data

#TSNE preserves local structure but not always global structure nor density structure. 

#reduce features to two dimensions with TSNE
#tuning parameters :
#perplexity : local/global structure trade-off
#theta : speed/accuracy trade-off 

#more info: https://distill.pub/2016/misread-tsne/


#pca : dimensional reduction as initial step
#set 'pca' and 'pca_scale' to TRUE to speed-up (only when needed) 
#reduce n-dimensional vectors to to 2 dimensions
  
tsne_model<-Rtsne::Rtsne(subset(dr_df, select=all_of(cols)), dims = 2, perplexity = perplex, 
                    pca = TRUE, verbose=TRUE, max_iter = 300, theta=th, check_duplicates = FALSE)

#re-attach row id
tsne = cbind.data.frame(dr_df,tsne_model$Y)
if (has_rownames(tsne)) {tsne<-remove_rownames(tsne)}

#coordinates of observations start with X
names(tsne)[names(tsne) == "1"]<-"X1"
names(tsne)[names(tsne) == "2"]<-"X2"

if(lvl==1) {
tsne<-column_to_rownames(tsne, var = "word")
tsne$word<-row.names(tsne)
tsne_store<-tsne
tsne_words<-tsne

} else {
  tsne<-column_to_rownames(tsne, var = "doc_id")  
  tsne$doc_id<-as.integer(row.names(tsne))
  
  tsne_sv_store<-tsne
  
  tsne<-tsne %>%  
  full_join(perception_doc,by = "doc_id" )
  tsne$subjectivity<-tsne$subjectivity_score
}

head(tsne,2)

# create TSNE plot
colors = rainbow(length(unique(row.names(tsne))))
names(colors) = unique(row.names(tsne))

plot.title = paste0('TSNE ',lvl_nme,' cloud ',scope.prefix)
tsne_plot<-ggplot(tsne, aes(X1, X2)) +
  #geom_point(aes(X1, X2, size = count, alpha = .1, color = avg_sentiment)) +
  geom_text(aes(X1, X2, label = row.names(tsne),
  colour=subjectivity),size = 2) +
  geom_point(size=0.25) +
  xlab("") + ylab("") +
  ggtitle(plot.title) +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  scale_color_viridis(discrete = FALSE, option = "C", na.value="darkgrey")+
  scale_fill_viridis(discrete = FALSE) + 
  theme_void()
(tsne_plot)
plot.nme = paste0(scope.prefix,'_TSNE_',lvl_nme,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * aspect_ratio, dpi = dpi)


#-----------------------------------------------------------------------------------------------
# 2 Uniform Manifold Approximation and Projection (UMAP)
#-----------------------------------------------------------------------------------------------

#UMAP is also a non linear dimensionality reduction algorithm in the same family as TSNE.
#UMAP's output results in more compact, separated clusters compared to TSNE.

#UMAP can be applied to new data points
#UMAP claims to preserve both local and most of the global structure in the data
#UMAP keeps a significant portion of the high-dimensional local structure in lower dimensionality.
#UMAP is therefore better than TSNE to compare the relative position of clusters

#more info: https://pair-code.github.io/understanding-umap/

# set up UMAP tuning parameters
custom.config<-umap.defaults
custom.config$n_neighbors<-10       # number of neighbors, defaults to 15
custom.config$n_epochs<-300         # number of iterations for convergence, defaults to 200
custom.config$random_state<-seed    # random seed

umap_model<-umap::umap(subset(dr_df, select=all_of(cols)), config=custom.config)
head(umap_model$layout, 3)

umap <-as.data.frame(umap_model$layout)

#coordinates of observations start with X
names(umap)[names(umap) == "V1"]<-"X1"
names(umap)[names(umap) == "V2"]<-"X2"

#re-attach vectors
umap = cbind.data.frame(dr_df,umap)

if(lvl==1) {
if (has_rownames(umap)) {umap<-remove_rownames(umap)}  
umap<-column_to_rownames(umap, var = "word")
#umap$word<-row.names(umap)
umap_store <-umap
head(umap,2)
} else {

umap_sv_store <-umap
  
umap<-umap %>%  
  full_join(perception_doc,by="doc_id")

if (has_rownames(umap)) {umap<-remove_rownames(umap)}
umap<-column_to_rownames(umap, var = "doc_id")
umap$doc_id<-as.numeric(row.names(umap))

umap$subjectivity<-umap$subjectivity_score

}

plot.title = paste0('UMAP ',lvl_nme,' cloud ',scope.prefix)
umap_plot<-ggplot(umap, aes(x=X1, y=X2)) +
  geom_text(aes(X1, X2, label = row.names(umap),
                colour=subjectivity),size = 2) +
  geom_point(size=0.25) +
  xlab("") + ylab("") +
  ggtitle(plot.title) +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  scale_color_viridis(discrete = FALSE, option = "C", na.value="darkgrey")+
  scale_fill_viridis(discrete = FALSE) +
  theme_void() 
(umap_plot)
plot.nme = paste0(scope.prefix,'_UMAP_',lvl_nme,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)

}

#garbage collecting
#rm(tsne,umap)

#-----------------------------------------------------------------------------------------------

#compare  Dimensionality  Reduction  algorithms  in terms  of  loss  of quality 
#higher Rnx value is better (low K represents local space, high K represents global space)
#Plot the R_NX curve for different embeddings

#embed_methods<-c("UMAP","tSNE")

#words
#word_emb<-lapply(embed_methods, function(x)
#  embed(subset(wv_df, select=-c(word)), x))
#names(word_emb)<-embed_methods
#plot_R_NX(word_emb)

#documents
#sent_emb<-lapply(embed_methods, function(x)
#  dimRed::embed(subset(sv_wei_df, select=all_of(cols)), x))
 #names(sent_emb)<-embed_methods

# plot.title = paste0('Compare dimension reduction methods documents ',scope.prefix)

#plot_R_NX(sent_emb) + 

#ggtitle(plot.title)+
#  ggplot2::theme(legend.title = ggplot2::element_blank(),
#                 legend.position = c(0.5, 0.1),
#                 legend.justification = c(0.5, 0.1))

#plot.nme = paste0(scope.prefix,'_compare_dimensionreduction_documents.png')
#plot.store <-paste0(plots.loc,plot.nme)
#ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)
