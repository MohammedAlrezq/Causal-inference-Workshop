
# based on avg treatment effect , here we want ot know if it is statistical signifinat or not 
# also enter the vraibels selected from OAENET as they are confounders 
# 
control <- read.csv(file="control.csv", header=TRUE, sep=",")

treatment <- read.csv(file="treatment.csv", header=TRUE, sep=",")

control = subset(control, select = -c(X))
treatment =  subset(treatment, select = -c(X))
control = subset(control, select = -c(shg_flag)) # remove treatment variable 
treatment =  subset(treatment, select = -c(shg_flag)) # remove treatment variable
#Read data files 

t_G <- split(treatment, list( treatment$METHUSE,treatment$EDUC )) 


c_G <- split(control, list( control$METHUSE, control$EDUC)) 



tr_length=matrix( rep( 0, len=length(t_G)), nrow = 1);
cr_length=matrix( rep( 0, len=length(t_G)), nrow = 1);

# check which partition has how many samples

for (i in 1:length(t_G)) {
  tr_length[i]=nrow(t_G[[i]])
  # nam_tr = paste("out_tr",i,sep="")
  # assign(nam_tr, t_G[[i]]$change)
  
  cr_length[i]=nrow(c_G[[i]])  
  # nam_cr = paste("out_cr",i,sep="")
  # assign(nam_cr, c_G[[i]]$change)
  
}

#creating the matrices to store the outputs
tr_out=matrix( rep( 0), nrow = max(tr_length), ncol = length(t_G));
cr_out=matrix( rep( 0), nrow = max(cr_length), ncol = length(c_G));

#collecting the outputs for each partition in a matrix form
for (i in 1:length(t_G)) {
  if ( tr_length[i] == 0 | cr_length[i] == 0 ){
    next
  } else {
    tr_out[1:tr_length[i],i]=t_G[[i]]$treatment_flag
    cr_out[1:cr_length[i],i]=c_G[[i]]$treatment_flag
  }
}

 #identifying the columns/partitions wghich has 0 entries/samples
 L = c();
 i = 1;
 for ( i in 1:length(t_G)){
   if ( tr_length[i] == 0 | cr_length[i] == 0 ){
     b = i
     L = c (L,b)
     i = i+1
   }
 }
 
 ## note, removed _f 
 
# #eliminating the columns/partitions which has 0 entries/samples
 tr_length_f = tr_length[, -L]
 cr_length_f = cr_length[, -L]
 
 tr_out_f = tr_out[, -L]
 cr_out_f = cr_out[, -L]

 ## note, removed _f because we dint run the above code because we set drop = true
 
#writing into text and csv files
write.table(tr_out_f, "tr_data.txt", sep="\t");
write.table(tr_out_f, file="tr_data.csv",sep=",",row.names=F);


write.table(cr_out_f, "cr_data.txt", sep="\t")
write.table(cr_out_f, file="cr_data.csv",sep=",",row.names=F)



write.table(tr_length_f, "tr_col_size.txt", sep="\t");
write.table(tr_length_f, file="tr_col_size.csv",sep=",",row.names=F)

write.table(cr_length_f, "cr_col_size.txt", sep="\t");
write.table(cr_length_f, file="cr_col_size.csv",sep=",",row.names=F)



##################################################################################################
