#condensed sentence, cluster membership and perception   
sent_txt <- condensed_sent %>%
  full_join(sv_store,by="doc_id") %>%
  full_join(perception_doc,by="doc_id") %>%
  subset(doc_id %in% vec_relevant_docs)

#report best clustering method
if(best_method_dr=="TSNE" & best_method_cl=="PAM") {
  sent_txt$best_cl <- sent_txt$tsne_cl_pam 
} else if (best_method_dr=="TSNE" & best_method_cl=="KMEANS") {
  sent_txt$best_cl <- sent_txt$tsne_cl_kmeans   
} else if (best_method_dr=="TSNE" & best_method_cl=="SPHERICKMEANS") {
  sent_txt$best_cl <- sent_txt$tsne_cl_spherickmeans
} else if (best_method_dr=="TSNE" & best_method_cl=="KMEANSPP") {
  sent_txt$best_cl <- sent_txt$tsne_cl_kmeanspp
} else if (best_method_dr=="UMAP" & best_method_cl=="PAM") {
  sent_txt$best_cl <- sent_txt$umap_cl_pam
} else if (best_method_dr=="UMAP" & best_method_cl=="KMEANS") {
  sent_txt$best_cl <- sent_txt$umap_cl_kmeans
} else if (best_method_dr=="UMAP" & best_method_cl=="SPHERICKMEANS") {
  sent_txt$best_cl <- sent_txt$umap_cl_spherickmeans
}  else if (best_method_dr=="UMAP" & best_method_cl=="KMEANSPP") {
  sent_txt$best_cl <- sent_txt$umap_cl_kmeanspp
} else {
  sent_txt$best_cl <- sent_txt$tsne_cl_kmeans  
}

#same for annotate_clusters : plz do this more efficiently later!!!
if(best_method_dr=="TSNE" & best_method_cl=="PAM") {
  annotate_clusters$best_cl <- annotate_clusters$tsne_cl_pam 
} else if (best_method_dr=="TSNE" & best_method_cl=="KMEANS") {
  annotate_clusters$best_cl <- annotate_clusters$tsne_cl_kmeans   
} else if (best_method_dr=="TSNE" & best_method_cl=="SPHERICKMEANS") {
  annotate_clusters$best_cl <- annotate_clusters$tsne_cl_spherickmeans
} else if (best_method_dr=="TSNE" & best_method_cl=="KMEANSPP") {
  annotate_clusters$best_cl <- annotate_clusters$tsne_cl_kmeanspp
} else if (best_method_dr=="UMAP" & best_method_cl=="PAM") {
  annotate_clusters$best_cl <- annotate_clusters$umap_cl_pam
} else if (best_method_dr=="UMAP" & best_method_cl=="KMEANS") {
  annotate_clusters$best_cl <- annotate_clusters$umap_cl_kmeans
} else if (best_method_dr=="UMAP" & best_method_cl=="SPHERICKMEANS") {
  annotate_clusters$best_cl <- annotate_clusters$umap_cl_spherickmeans
} else if (best_method_dr=="UMAP" & best_method_cl=="KMEANSPP") {
  annotate_clusters$best_cl <- annotate_clusters$umap_cl_kmeanspp
} else {
  annotate_clusters$best_cl <- annotate_clusters$tsne_cl_kmeans  
}

#save full registration
sent_ex <- paste0(report.loc,'documents-complete-',scope.prefix,'-',best_method_dr,'-',best_method_cl,'-k',k,'.csv')
write.csv(sent_txt, file=sent_ex,row.names=FALSE)