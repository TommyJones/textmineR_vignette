strawman_fun <- function(docs){
  names(docs) <- 1:length(docs)
  stopwords <- c(tm::stopwords("english"), tm::stopwords("SMART"))
  corp <- tm::Corpus(tm::VectorSource(docs))
  corp <- tm::tm_map(corp, tm::content_transformer(tolower))
  corp <- tm::tm_map(x=corp, tm::removeWords, stopwords)
  dtm <- tm::DocumentTermMatrix(corp)
  dtm
}

data(nih_sample)

strawman_times <- sapply(1:10, function(x){
  system.time(strawman_fun(nih_sample$ABSTRACT_TEXT))
})

textminer_times <- sapply(1:10, function(x){
  system.time(textmineR::CreateDtm(doc_vec = nih_sample$ABSTRACT_TEXT,
                                   remove_punctuation = FALSE,
                                   remove_numbers = FALSE))
})

y <- c(mean(strawman_times[ "elapsed" , ]), mean(textminer_times[ "elapsed" , ]))

z <- barplot(y,
             names.arg = c("tm", "textmineR"),
             main="Mean Seconds to Create a DTM\nof 500 NIH Grant Abstracts",
             col="steelblue", cex.names = 1.5, yaxt="n",
             ylim = c(0, max(y) + 0.1))
text(x=c(z[1, ], z[2, ]), 
     y = y + par("cxy")[2]/1.1, 
     labels = round(y, 2),
     cex=1.25, 
     xpd=TRUE)

round(y[ 1 ] / y[ 2 ], 1)

