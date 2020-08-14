
# set up df
th <- 9
L <- 100
df <- data.frame(i=1:L, data=rnorm(L, mean=10))
df$val <- ifelse(df$data < th, NA, df$data)

# find all NA values
inx <- which(is.na(df$val))

# correct for first and last indices
if (!(1%in%inx)) inx <- c(1, inx)
if (!(L%in%inx)) inx <- c(inx, L)

# Find start and stop of non-NA 'gaps'
M <- length(inx)
inx2 <- c(inx[2:M], 0) - inx
start <- which(inx2 > 1)
stop <- start + 1

# find max of each non-NA gap
max.inx <- c()
for (i in 1:length(start)) {
  s1 <- start[i]
  s2 <- stop[i]
  r1 <- ifelse(inx[s1]==1, inx[s1], inx[s1]+1)
  r2 <- ifelse(inx[s2]==L, inx[s2], inx[s2]-1)
  range <- r1:r2
  sub <- df[range,]
  max.inx <- c(max.inx, sub$i[sub$val == max(sub$val, na.rm=T)])
}
df$maxes <- df$i %in% max.inx 

# make a nice plot :)
library(ggplot2)
ggplot(df, aes(i, data)) + 
  geom_line() +
  geom_point(data=df[df$maxes,], aes(x=i, y=data), color='magenta') +
  geom_abline(slope=0, intercept=th, color='blue', linetype="dashed") +
  geom_vline(xintercept=inx[start], color='gray', linetype='dashed')
