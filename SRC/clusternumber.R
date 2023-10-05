
#-----------------------------------------------------------------------------------------------

#  'Optimal' number of clusters

#-----------------------------------------------------------------------------------------------

if(par_processing==TRUE) {
cl <- makeCluster(thread)
doParallel::registerDoParallel(cl)
}

#Optimal number of WORD clusters 

#message("Detect optimal number of word clusters...")
#for (ki in 3:15)
#  asw[[ki]] <- pam(subset(umap, select=all_of(c("X1","X2"))), ki) $ silinfo $ avg.width
#kw <- which.max(asw)

if(k_optimal==TRUE) {
#Optimal number of SENTENCE clusters 

message("Detect optimal number of sentence clusters within your range ", k_min, ":", k_max)

#create empty list of length k_max
asw <- numeric(k_max)
#par_processing<-FALSE
if(par_processing==TRUE) {
#parallel processing
  asw <- foreach::foreach(ki = k_min:k_max, .combine = 'c') %dopar% {
  cluster::pam(subset(umap_sv_store, select=dplyr::all_of(c("X1","X2"))), ki) $ silinfo $ avg.width
  }
} else {
  #for (ki in 1:k_max)
  #asw[[ki]] <- cluster::pam(subset(umap_sv_store, select=dplyr::all_of(c("X1","X2"))), ki) $ silinfo $ avg.width
  asw <- foreach::foreach(ki = k_min:k_max) %do% {
  cluster::pam(subset(umap_sv_store, select=dplyr::all_of(c("X1","X2"))), ki) $ silinfo $ avg.width
  }
}
  
k_offset=k_min-1

#optimal number of clusters within range (k_min:k_max)
ks <- which.max(asw) + k_offset

message("\nOptimal number of sentence clusters -within your range- is ", ks)

message("\n!!!IMPORTANT!!!:\n you might want to choose a larger number of document clusters (ks) for more differentiation
see XXX_n_clusters_documents.png in de PLOTS directory for alternative value")

#plots
lvl_nme<-NULL

for(lvl in lvl_start:2) {
  
if(lvl==1) {
#words    
  dr_method_nclust<-umap_store
  lvl_nme <- "words"
} else {
#sentences  
  dr_method_nclust<-umap_sv_store
  lvl_nme <- "documents"
}

#three methods: elbow, silhouette and GAP. We choose Silhouette here.
plot.nme = paste0(scope.prefix, '_n_clusters_',lvl_nme,'.png')
plot.store <-paste0(plots.loc,plot.nme)

# 3 methods (choose just one)
# I. Elbow method
#fviz_nbclust(tsne, kmeans, method = "wss") +
#  geom_vline(xintercept = 4, linetype = 2)+
#  labs(subtitle = "Elbow method")

# II. Silhouette method
fviz_nbclust(subset(dr_method_nclust, select=all_of(c("X1","X2"))), kmeans, method = "silhouette") + 
labs(subtitle = paste0("Silhouette method on ", lvl_nme))

# III. GAP method (regular, 10 iterations)
#fviz_nbclust(dr_method_nclust, kmeans, method = "gap_stat")+
#  labs(subtitle = "GAP method")

# GAP heavy-duty version (when regular not converging)
#CusKmeansFUN <- function(x,k) list(cluster=kmeans(x, k, iter.max=50))
#fviz_nbclust(dr_method_nclust, FUNcluster=CusKmeansFUN, method="gap_stat") +
#labs(subtitle = "GAP method - heavy duty")

ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi,device='png')
}
}