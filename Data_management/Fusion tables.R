setwd("~/Stage/Perma-M1/Programmes/Fusion_releves")
R1 = read.table("R1.txt",h=T)
R2 = read.table("R2.txt",h=T)


R3 = R1+R2
### Fusion
R12 = merge(R1,R2,by="code",all=T, sort = T)

R12

### Technique de BG 1
library(dplyr)
resu<-bind_rows(R1,R2) %>% group_by(code)%>%   
  summarise_all(sum)
resu

### Technique de BG 2
res <- aggregate(. ~ code, rbind(R1,R2), sum)
res
