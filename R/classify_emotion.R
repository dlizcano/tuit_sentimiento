classify_emotion <- function(textColumns,algorithm="bayes",prior=1.0,verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  lexicon <- read.csv("data/lexicon/emotions_sp.csv",encoding="UTF-8",header=FALSE)
  
  
  counts <- list(Enojo=length(which(lexicon[,2]=="Enojo")),Repulsion=length(which(lexicon[,2]=="Repulsion")),Miedo=length(which(lexicon[,2]=="Miedo")),Alegria=length(which(lexicon[,2]=="Alegria")),Tristeza=length(which(lexicon[,2]=="Tristeza")),Sorpresa=length(which(lexicon[,2]=="Sorpresa")),total=nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(Enojo=0,Repulsion=0,Miedo=0,Alegria=0,Tristeza=0,Sorpresa=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    
    for (word in words) {
      for (key in names(scores)) {
        emotions <- lexicon[which(lexicon[,2]==key),]
        index <- pmatch(word,emotions[,1],nomatch=0)
        if (index > 0) {
          entry <- emotions[index,]
          
          category <- as.character(entry[[2]])
          count <- counts[[category]]
          
          score <- 1.0
          if (algorithm=="bayes") score <- abs(log(score*prior/count))
          
          if (verbose) {
            print(paste("WORD:",word,"CAT:",category,"SCORE:",score))
          }
          
          scores[[category]] <- scores[[category]]+score
        }
      }
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    if (best_fit == "Repulsion" && as.numeric(unlist(scores[2]))-3.09234 < .01) best_fit <- NA
    documents <- rbind(documents,c(scores$Enojo,scores$Repulsion,scores$Miedo,scores$Alegria,scores$Tristeza,scores$Sorpresa,best_fit))
  }
  
  colnames(documents) <- c("Enojo","Repulsion","Miedo","Alegria","Tristeza","Sorpresa","BEST_FIT")
  return(documents)
}