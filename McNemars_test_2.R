# take the 4 files from McNemars and import them here 

ptm <- proc.time()
cr_col_sz<- read.csv(file="cr_col_size.csv", header=TRUE, sep=",")

cr_oc<- read.csv(file="cr_data.csv", header=TRUE, sep=",")

tr_col_sz<- read.csv(file="tr_col_size.csv", header=TRUE, sep=",")

tr_oc<- read.csv(file="tr_data.csv", header=TRUE, sep=",")





p1 = dim(tr_col_sz)
p = p1[1] #gets the number of partitions

#create 0 matrices to store values
T_1 = matrix( rep( 0, len=p), nrow = p);
T_0 = matrix( rep( 0, len=p), nrow = p);

C_1 = matrix( rep( 0, len=p), nrow = p);
C_0 = matrix( rep( 0, len=p), nrow = p);

B_max = matrix( rep( 0, len=p), nrow = p);
C_max = matrix( rep( 0, len=p), nrow = p);

#main loop to calculate B_max and C_max
for (i in 1:p) {
  T_1[i] = sum(tr_oc[1:tr_col_sz[i,1],i])
  T_0[i] = tr_col_sz[i,1] - T_1[i]
  
  C_1[i] = sum(cr_oc[1:cr_col_sz[i,1],i])
  C_0[i] = cr_col_sz[i,1] - C_1[i]
  
  B_max[i] = min(T_0[i], C_1[i])
  C_max[i] = min(T_1[i], C_0[i])
}


B =sum(B_max)
C=sum(C_max)

#Z score, objective function of robust McNemar's test
z=(B-C-1)/sqrt(B+C)

# z = -29 and this means that the two variables EDUC and METHUSE have no effect. however, it is recommended to add more variables 

#p-value
pnorm(z,mean=0, sd=1, lower.tail=FALSE)


proc.time() - ptm
