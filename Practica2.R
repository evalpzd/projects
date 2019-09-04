#Ejercicio 1 terminado
Id<-c(rep(1:4,each=3,len=12))
x<-c(seq(1,43,along.with=Id))
y<-c(seq(-20,0,along.with = Id))
Letter<-rep(c("a","b","c"),len=12)
data_frame<-data.frame(Id,Letter,x,y)

#Ejercicio 2 

x.a<-subset(data_frame,Letter=="a",select=x) 
y.a<-subset(data_frame,Letter=="a",select=y) 
x.b<-subset(data_frame,Letter=="b",select=x) 
y.b<-subset(data_frame,Letter=="b",select=y)
x.c<-subset(data_frame,Letter=="c",select=x) 
y.c<-subset(data_frame,Letter=="c",select=y)
AB<-data.frame(x.a,y.a,x.b,y.b,x.c,y.c)
names(AB)<-c("x.a","y.a","x.b","y.b","x.c","y.c")


#Ejercicio 3 
Age<-c(14,12,15,10)
ID<-c(seq(1:4))
df1<-data.frame(ID,Age)
Sex<-c("F","M","M","F")
Code<-c("a","b","c","d")
df2<-data.frame(ID,Sex,Code)
M<-merge(df1,df2)

#Ejercicio 4 
Score<-c(100,98,94,99)
ID2<-c(seq(4,1))
df3<-data.frame(ID2,Score)
row.names(df3)<-c(1:4)
names(df3)<-c("ID","Score")
N<-merge(M,df3)

#Ejercicio 5
#1)
N$Sex<-NULL
N$Code<-NULL
#2) (falta)
N1<-melt(N)
N1$Sex<-NULL; N1$Code<-NULL
names(N1)<-c("Ind","Values")

#Ejercicio 6
mean_tree<-apply(trees,2,mean)
min_tree<-apply(trees,2,min)
max_tree<-apply(trees,2,max)
sum_tree<-apply(trees,2,sum)
Girth<-c(mean_tree[1],min_tree[1],max_tree[1],sum_tree[1])
Height<-c(mean_tree[2],min_tree[2],max_tree[2],sum_tree[2])
Volume<-c(mean_tree[3],min_tree[3],max_tree[3],sum_tree[3])
A<-data.frame(Girth,Height,Volume)

#Ejercicio 7
#1)
A<-A[order(A$Girth),]

#2)
rownames(A)<-c("mean","min","max","tree")

#Ejercicio 8
Ints<-integer()
Logicals<-logical()
Doubles<-double ()
Characters<-character()
empty_df<-data.frame(Ints,Logicals,Doubles,Characters)

#Ejercicio 9
X<-c(1,2,3,1,4,5,2)
Y<-c(0,3,2,0,5,9,3)
XY<-data.frame(X,Y)
row_dup<-duplicated(XY)
uni_row<-XY[!row_dup,]

#Ejercicio 10
Titanic<-data.frame(Titanic)
help(merge) 
columnas<-c(F,T,T,F,T)
first_class<-subset(Titanic,Class=="1st" & Survived=="No")[,columnas]

