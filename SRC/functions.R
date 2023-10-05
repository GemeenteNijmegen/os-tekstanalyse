
#-----------------------------------------------------------------------------------------------  

#Functions

#-----------------------------------------------------------------------------------------------

#remove objects
#You can call the function remove_objects with a character vector of object names you want to remove
remove_objects <- function(objects) {
  existing_objects <- ls()
  objects_to_remove <- intersect(existing_objects, objects)
  if (length(objects_to_remove) > 0) {
    rm(list=objects_to_remove, envir=.GlobalEnv)
  }
}


#-----------------------------------------------------------------------------------------------

#coocurrence word network plot function for BTM 
btm_cooc <- function(model, 
                     tokens = stats::terms(model, type = "tokens", top_n = 5), 
                     biterms = stats::terms(model, type = "biterm", top_n = 5),
                     topic_nr = 1, 
                     top_n = 30, 
                     title = sprintf("Biterm Topic Model topic %s", topic_nr),
                     subtitle = "cooccurrences of most important words", 
                     edge_colour = "red",
                     node_colour = "darkgreen",
                     node_size = 4,
                     ...){
  stopifnot(requireNamespace("data.table"))
  stopifnot(requireNamespace("ggraph"))
  stopifnot(requireNamespace("igraph"))
  stopifnot(requireNamespace("ggplot2"))
  topic_nr <- as.integer(topic_nr)
  stopifnot(topic_nr > 0 & topic_nr <= length(tokens))
  
  cooc <- biterms$biterms
  cooc <- cooc[cooc$topic %in% topic_nr & 
                 (cooc$term1 %in% tokens[[topic_nr]]$token | 
                    cooc$term2 %in% tokens[[topic_nr]]$token), ]
  cooc <- setDT(cooc)
  cooc <- cooc[, list(freq = .N), by = list(term1, term2)]
  cooc <- setDF(cooc)
  
  wordnetwork <- head(cooc, top_n)
  wordnetwork <- wordnetwork[, c("term1", "term2", "freq")]
  wordnetwork <- igraph::graph_from_data_frame(wordnetwork)
  g <- ggraph::ggraph(wordnetwork, layout = "fr") +
    ggraph::geom_edge_link(ggplot2::aes(width = freq, edge_alpha = freq), edge_colour = edge_colour) +
    ggraph::geom_node_text(ggplot2::aes(label = name), col = node_colour, size = node_size) +
    ggraph::theme_graph(...) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(title = title, subtitle = subtitle)
  g
}

#-----------------------------------------------------------------------------------------------

#plot annotation relations
plot_annotation <- function(x, size = 3){
  stopifnot(is.data.frame(x) & all(c("sentence_id", "token_id", "head_token_id", "dep_rel",
                                     "token_id", "token", "lemma", "upos", "xpos", "feats") %in% colnames(x)))
  x <- x[!is.na(x$head_token_id), ]
  x <- x[x$sentence_id %in% min(x$sentence_id), ]
  edges <- x[x$head_token_id != 0, c("token_id", "head_token_id", "dep_rel")]
  edges$label <- edges$dep_rel
  g <- igraph::graph_from_data_frame(edges,
                             vertices = x[, c("token_id", "token", "lemma", "upos", "xpos", "feats")],
                             directed = TRUE)
  ggraph(g, layout = "linear") +
    geom_edge_arc(ggplot2::aes(label = dep_rel, vjust = -0.20),
                  arrow = grid::arrow(length = unit(4, 'mm'), ends = "last", type = "closed"),
                  end_cap = ggraph::label_rect("wordswordswords"),
                  label_colour = "red", check_overlap = TRUE, label_size = size) +
    geom_node_label(ggplot2::aes(label = token), col = "darkgreen", size = size, fontface = "bold") +
    geom_node_text(ggplot2::aes(label = upos), nudge_y = -0.35, size = size) +
    theme_graph(base_family = "Arial Narrow") +
    labs(title = "udpipe output", subtitle = "tokenisation, parts of speech tagging & dependency relations")
}

#-----------------------------------------------------------------------------------------------

#Similarity of words with GloVe
get_similar_words <- function(reference_word, word_embeddings) {
  
  #Find closest aligned word embeddings based on cosine similarity
  tryCatch({
    word <- word_embeddings[reference_word, , drop = FALSE]
  },
  error = function(e) {
    stop("The supplied word (", word, ") is not part of the created vocabulary.")
  }
  )
  
  cos_sim <- text2vec::sim2(
    x = word_embeddings, 
    y = word, 
    method = "cosine", 
    norm = "l2"
  )
  
  head(sort(cos_sim[,1], decreasing = TRUE), 5)
}

#-----------------------------------------------------------------------------------------------

#convert emoticons to words
convert_emoticons_dutch <- function(text) {
  # define a list of emoticons and their corresponding sentiment words in Dutch
  emoticons <- c(":)", ":(", ";)", ":D", ":P", ":o", "<3")
  sentiments <- c("blij", "verdrietig", "knipoog", "lach", "tong", "verbaasd", "hart")
  
  # replace each emoticon with its corresponding sentiment word
  for (i in 1:length(emoticons)) {
    pattern <- paste0("([[:punct:][:space:]]|^):(([[:punct:][:space:]]|$))")
    replacement <- paste0("\\1", sentiments[i], "\\2")
    text <- gsub(pattern, replacement, text, ignore.case = TRUE)
  }
  
  # return the converted text
  return(text)
}