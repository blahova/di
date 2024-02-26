collection_integral <- function(funkcia, mi, kolekcia)
{
  pocet<-length(funkcia)
  #prava strana je ta funkcia?
  #kolekcia bude binarne napr. "101","001" atd
  rhs<- funkcia
  dir<- rep("<=",length(funkcia))
  dec<-strtoi(kolekcia,2) #kolekcia decimalne
  matica <- matrix(nrow = pocet, ncol = length(kolekcia) )
  
  for (i in 1:length(kolekcia)) 
  {
    for(j in 0:(pocet-1))
    {
      #print(as.numeric(substr(kolekcia[i],pocet-j,pocet-j)))
      if(as.numeric(substr(kolekcia[i],pocet-j,pocet-j))==1)
      {
        matica[j+1,i]<-mi[dec[i]]
      }
      else
      {
        matica[j+1,i]<-0
      }
    }
  }
  
}

collection_integral(c(0.5,0.3),c(0.2,0.3,0.7),c("01","10","11"))


test<-c("123","456")
for(i in 1:length(test))
{
  for(j in 0:2)
  {
    print(substr(test[i],3-j,3-j))
  }
}

#lp() v lpSolve

#INT -> BIN
#toto mi najviac sedi do toho ze ako by sa to malo premienat, ale je to z R.utils
intToBin(as.numeric(c(5,10)))

#BIN -> INT
#toto neni z nejakej konkretnej package
strtoi(c("0101","1"),2)

mat<-matrix(nrow=3,ncol=3)
for(i in 1:3)
{
  for(j in 1:3)
  {
    if(i==j)
    {
      mat[i,j]<-1
    }
    else
    {
      mat[i,j]<--j
    }
  }
}
mat[1,]
print(c(mat[1,1],mat[2,1]))
