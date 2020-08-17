x <- c("a", "b", "c", "d")
for(i in x) {
  print(i)
}
y <- matrix(1:6, 2, 3)
for(i in seq_len(nrow(y))){
  for(j in seq_len(ncol(y))){
    print(y[i,j])
  }
}
z <- 5
while(z >= 3 && z <= 10){
  print(z)
  coin <- rbinom(1, 1, 0.5)
  if(coin == 1){ ## random walk
    z <- z + 1
  }
  else {
    z <- z - 1
  }
}
