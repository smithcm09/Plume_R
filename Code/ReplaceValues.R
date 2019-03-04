
#Replace variables with watec heights where able
#testing
a <- c(1,2,3) # original vector PPH
b <- c(8,9) # index vector
c <- replace(a,c(2,3),b) # replacement values

index_vec <- c(5,15,21,22,24,33,34,62,68)
repValues <- c(1250,1620,1494,1116,1584,1296,1224,1530,1692)
PPH_altered <- replace(PPH, index_vec, repValues)

