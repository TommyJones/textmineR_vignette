
rm(list=ls())

library(textmineR)

data(nih_sample)


# Select a document

doc <- nih_sample$ABSTRACT_TEXT[ 10 ]

# Parse it into sentences
doc <- stringi::stri_split_boundaries(doc, type = "sentence")[[ 1 ]]

names(doc) <- seq(along = doc)

# Turn those sentences into a DTM, use stemming & bi-grams
dtm <- CreateDtm(doc, 
                 ngram_window = c(1, 2),
                 stem_lemma_function = function(x) SnowballC::wordStem(x, "porter"))

# TF-IDF Frequency re-weighting
idf <- log(nrow(dtm) / colSums(dtm > 0))

tfidf <- t(dtm) * idf

tfidf <- t(tfidf)

# Calculate document-to-document cosine similarity
csim <- tfidf / sqrt(rowSums(tfidf * tfidf))

csim <- csim %*% t(csim)

# Turn that cosine similarity matrix into a nearest-neighbor network
nn <- csim

diag(nn) <- 0

nn <- apply(nn, 1, function(x){
  x[ x < sort(x, decreasing = TRUE)[ 2 ] ] <- 0
  x
})

nn <- nn * 100

g <- igraph::graph_from_adjacency_matrix(nn, mode = "directed", weighted = TRUE)

plot(g)

# Calculate eigenvalue centrality
ec <- igraph::eigen_centrality(g)

# Return top 3 central sentences as the summary
summary <- doc[ names(ec[[ 1 ]])[ order(ec[[ 1 ]], decreasing = T) ][ 1:2 ] ]

summary <- summary[ order(as.numeric(names(summary))) ]

paste(summary, collapse = " ")
