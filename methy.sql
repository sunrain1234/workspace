 chr1num<-function(file,header=T){
count<-1
pt<-file(file,"r")
name<-NULL
line<-readLines(pt,n=1)
name<-unlist(strsplit(line,split=','))
line<-readLines(pt,n=1)
while(length(line)!=0){
data<-unlist(strsplit(line,split=','))
if(data[1]=="chr1"){
count=count+1
}
line<-readLines(pt,n=1)
}
return(count)
close(pt)
}

num
[1] 2375163

data<-fread(input="D:\\2017小学期\\BIC\\GM12878WGBS.csv",sep=",",nrow=2375163,header=T)
total.read<-sapply(data[,3],as.numeric)
methy.read<-sapply(data[,4],as.numeric)
methy.level<-matrix(1:2375163,nrow<-2375163)
read<-cbind(methy.read,total.read,methy.level)
read[,3]<-read[,1]/read[,2]
read[read[,2]==read[,1],3]<-1-0.001
read[read[,2]==0,3]<-0.001
methy.level<-read[,3]
B<-function(p,q){
	f<-function(x){
	x^(p-1)*(1-x)^(q-1)
}
return(integrate(f,0,1)$value)
}

P<-function(i,a){
p<-1
for(j in (i-50):(i+50)){
p<-p*choose(total.read[j],methy.read[j])*B(methy.read[j]+a,total.read[j]-methy.read[j]+a)/B(a,a)
}
return(p)
}

a<-function(i){
a1<-0
a2<-0
aj<-0.1
j<-1
while(aj<3){
a1<-a1+aj*P(i,aj)
a2<-a2+P(i,aj)
j<-j+1
aj<-aj+0.1
}
return(a1/a2)
}

p<-function(i,a){
return(methy.level[i]^(a-1)*(1-methy.level[i])^(a-1)/B(a,a))
}
i<-matrix(51:2375113,ncol=1)
A<-a(i)
a1 <- Vectorize(a)
 a1(51:2375113)

for(j in (i-50):(i+50)){
p<-p*choose(total.read[j],methy.read[j])*B(methy.read[j]+a,total.read[j]-methy.read[j]+a)/B(a,a)
}