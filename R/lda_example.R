
rm(list=ls())

library(textmineR)


# Load some data into the workspace 
data(nih_sample)

# Create a document term matrix
dtm <- CreateDtm(nih_sample$ABSTRACT_TEXT[ 1:99 ], 
                 doc_names = nih_sample$APPLICATION_ID[ 1:99 ], 
                 ngram_window = c(1, 2))


# explore basic frequencies & curate vocabulary
tf <- TermDocFreq(dtm = dtm)

# Eliminate words appearing less than 2 times or in more than half of the
# documents
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]

dtm <- dtm[ , vocabulary]

# Fit an LDA topic model using variational EM
model <- FitLdaModel(dtm = dtm, k = 10, 
                     alpha = 0.1, beta = 0.05,
                     iterations = 500,
                     method = "gibbs")

names(model)

# Get the R-squared of this model
model$r2 <- CalcTopicModelR2(dtm = dtm, phi = model$phi, theta = model$theta)

model$r2

# top 5 terms of the model according to phi & phi-prime
model$top_terms <- GetTopTerms(phi = model$phi, M = 5)

# probabilistic coherence counting top 5 terms in each topic
model$coherence <- CalcProbCoherence(phi = model$phi, dtm=dtm)

hist(model$coherence, col = "blue")
abline(v = 0, lwd = 3)

summary(model$coherence)

# give a hard in/out assignment of topics in documents
model$assignments <- model$theta

model$assignments[ model$assignments < 0.05 ] <- 0

model$assignments <- model$assignments / rowSums(model$assignments)

model$assignments[ is.na(model$assignments) ] <- 0


# Get some topic labels using n-grams from the DTM
model$labels <- LabelTopics(assignments = model$assignments, 
                            dtm = dtm,
                            M = 2)

model$doc_count <- colSums(model$assignments > 0)

# Create a summary matrix to view topics
model$topic_summary <- data.frame(topic = rownames(model$phi),
                                  top_terms = apply(model$top_terms, 2, 
                                                    function(x) paste(x, collapse=", ")),
                                  labels = apply(model$labels, 1, 
                                                 function(x) paste(x, collapse=", ")),
                                  coherence = round(model$coherence, 3),
                                  doc_count = model$doc_count, 
                                  stringsAsFactors=FALSE)


View(model$topic_summary)

# phi-prime, P(topic | words) for classifying new documents
model$phi_prime <- CalcPhiPrime(phi = model$phi, 
                                theta = model$theta, 
                                p_docs = rowSums(dtm))

# vectorize our new document
new_doc <- nih_sample$ABSTRACT_TEXT[ 100 ]

names(new_doc) <- nih_sample$APPLICATION_ID[ 100 ]

new_dtm <- CreateDtm(new_doc, ngram_window = c(1, 2))

# classify new topics with phi_prime
common_vocab <- intersect(colnames(new_dtm), colnames(model$phi_prime))

new_topics <- new_dtm[ , common_vocab ] %*% t(model$phi_prime[ , common_vocab])

# normalize rows
new_topics <- new_topics / rowSums(new_topics)

# give a hard in/out assignment and re-normalize
new_topics[ new_topics < 0.05 ] <- 0

new_topics <- new_topics / rowSums(new_topics)

# plot result
par(mar=c(5,11,4,2) + 0.1)
barplot(new_topics[ 1 , ], horiz=T, las=2, names.arg = model$labels[ , 1 ])
par(mar=c(5,4,4,2) + 0.1)

