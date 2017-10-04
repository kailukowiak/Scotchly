
 

library(dplyr)

library(tidyr)
  

## Loading data

 
scotchImport <- read.table("/Users/kailukowiak/Scotchly/scotch.csv", header = F, sep = ",", stringsAsFactors = FALSE)

  

 
head(scotchImport)
tail(scotchImport)
  

#We need to remove the last two rows

 
scotch1 <- scotchImport[1:110, ]
scotch1$V2 <- NULL
scotch1[1,1] <- "Category"
  

 
scotch <- scotch1
scotch2 <- scotch1[1:2,] 

scotchNames <- scotch2 %>% 
  t() %>% 
  tbl_df() %>% 
  rename(categ = "1", colorcol = "2") %>% 
  unite(name1, c("categ", "colorcol"), sep = "_", remove = TRUE)

colnames(scotch) = scotchNames$name1
scotch <- scotch[-c(1:2),]
  

#That was a lot of ugly code. If I have time before the presentation I'll tidy it up.


#Further tidying to remove non numeric data. (Factor data can be ignored because it is in dummy variable form.) 
 
library(FNN)
scotch$`_DISTRICT` <- NULL
scotch$`_REGION` <- NULL
df = as.data.frame(sapply(scotch, as.numeric))
df$Category_NAME <- NULL


#Now to create a function to find euclidean distance. 

n = "NULL"
readScotch <- function()
  { 
  print(scotch$Category_NAME) # IMport name
  dfN = filter(df, color_wyne == 2)
  while(n != "Done"){
    n = readline(prompt = "Enter your favorite scotch from the above list ('Done' to exit): ") # Get promt
    scotch_dex = filter(scotch, Category_NAME == n) 
    scotch_dex = scotch_dex[, -1] #remove the name column to compare with KNN
    scotch_dex = as.numeric(scotch_dex) # Necessary to remove sting types.
    dfN = rbind(dfN, scotch_dex) # Add a row to the dataframe. Need the row vector, not DF
  }
  dfN = drop_na(dfN)
  avgN = dfN %>% colMeans()
  avgN <- as.data.frame(avgN) %>% t() %>% as.data.frame()
  colnames(avgN) <- colnames(dfN)
  x <- knnx.index(data.matrix(df), data.matrix(avgN), k = 4)
  x <- x[1,1]
  print(paste("The scotch most like the suggestions is:", scotch[x, 1]))
}
  