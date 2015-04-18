A <- read.csv("right_order.csv", stringsAsFactors = FALSE)
B <- read.csv("messed_order.csv", stringsAsFactors = FALSE)
right_names <- names(A)
ret <- B["person_id"]
for (i in 2:length(right_names))
{
  ret <- cbind(ret, B[,right_names[i]])
}
names(ret) <- right_names
ret[is.na(ret)] <- ""
write.csv(ret, "ordered.csv", row.names = FALSE)
ret