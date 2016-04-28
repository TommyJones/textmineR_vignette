### Count the relative popularity of slam and Matrix libraries by their 
  # dependencies.

library(magrittr)

matrix_page <- scan("https://cran.r-project.org/web/packages/Matrix/index.html", 
                    what="", sep="\n")

slam_page <- scan("https://cran.r-project.org/web/packages/slam/index.html", 
                    what="", sep="\n")

CountUse <- function(x){
  
  # paste everything together
  result <- paste(x, collapse = "\n")
  
  # use some regex to remove html etc
  result <- result %>% 
    stringr::str_replace_all("<[^<>]+>", " ") %>%
    stringr::str_replace_all(" +", " ")
  
  # Find the relevant sections & isolate their contents
  result <- stringr::str_split(result, "\n")[[ 1 ]]
  
  loc <- which(stringr::str_detect(result, "Reverse dependencies:"))
  
  result <- result[ (loc + 1):length(result) ]
  
  result <- result[ ! stringr::str_detect(result, "Reverse\\&") ]
  
  result <- result[ ! result == " " ]
  
  result <- unlist(stringr::str_split(result, ",| "))
  
  # Count the number of dependencies
  length(unique(result))
  
}

matrix_count <- CountUse(matrix_page)

slam_count <- CountUse(slam_page)

y <- c(matrix_count, slam_count)

z <- barplot(y,
             names.arg = c("Matrix", "slam"),
             main="Number of Reverse Dependencies,\nImports, Suggests, etc.",
             col="steelblue", cex.names = 1.5, yaxt="n",
             ylim = c(0, max(y) + 100))
text(x=c(z[1, ], z[2, ]), 
     y = y + par("cxy")[2]/1.1, 
     labels = round(y, 2),
     cex=1.25, 
     xpd=TRUE)

# barplot(c(Matrix=matrix_count, slam=slam_count))
