



#StarSpace is a general-purpose neural model for efficient learning of entity embeddings for solving a wide variety of problems
#text classification, document similarity

#Under construction 
#2021-05-17

library(ruimtehol)
data("dekamer", package = "ruimtehol")


dekamer$x <- strsplit(dekamer$question, "\\W")

#question
dekamer$x <- sapply(dekamer$x, FUN = function(x) paste(setdiff(x, ""), collapse = " "))
dekamer$x <- tolower(dekamer$x)

#class
dekamer$y <- strsplit(dekamer$question_theme, split = ",")
dekamer$y <- lapply(dekamer$y, FUN=function(x) gsub(" ", "-", x))

#set.seed(123456789)
model <- embed_tagspace(x = dekamer$x, y = dekamer$y,
                        dim = 50, 
                        lr = 0.01, epoch = 40, loss = "softmax", adagrad = TRUE, 
                        similarity = "cosine", negSearchLimit = 50,
                        ngrams = 2, minCount = 2)
plot(model)                        

text <- c("de nmbs heeft het treinaanbod uitgebreid via onteigening ...",
          "de migranten komen naar europa de asielcentra ...")                   
predict(model, text, k = 3)  


#predict(model, "koning filip", k = 10, type = "knn")
#predict(model, "koning filip", k = 10, type = "embedding")
