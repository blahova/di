library(lpSolve)

collection_integral <- function(f, mi, system)
{
  pocet<-length(f)
  #prava strana je ta funkcia?
  #kolekcia bude binarne napr. "101","001" atd
  
  
  for(i in 1:length(system))
  {
    funkcia<-c()
    kolekcia<-system[[i]] #kolekcia ktoru prechadzam
    rhs<-c(f,rep(0,length(kolekcia)))
    dir<- c(rep("<=",length(f)),rep(">=",length(kolekcia)))
    dec<-strtoi(kolekcia,2) #kolekcia decimalne
    matica <- matrix(nrow = pocet+length(kolekcia), ncol = length(kolekcia))
    for(j in 1:length(kolekcia))
    {
      funkcia<-c(funkcia,mi[dec[j]])
      for (k in 0:(pocet-1))
      {
        if(as.numeric(substr(kolekcia[j],pocet-k,pocet-k))==1)
        {
          matica[k+1,j]<-mi[dec[j]]
          #matica[k+1,j]<-1
          
        }
        else
        {
          matica[k+1,j]<-0
          
        }
      }
      for (k in 1:length(kolekcia))
      {
        if(j==k)
        {
          matica[k+pocet,j]<-1
        }
        else
        {
          matica[k+pocet,j]<-0
        }
      }
    }
    vysledok<-lp("max",funkcia,matica,dir,rhs)
    print(matica)
    print(rhs)
    print(vysledok)
    print(vysledok$solution)
    print(dir)
  }

}

collection_integral(c(1,2,3),c(1,16,25,81,25,100,196),
                    list(c("111","011","101","001"),c("011","001"),c("001")))




for (i in 1:length(kolekcie)) 
{
  for(j in 0:(pocet-1))
  {
    #print(as.numeric(substr(kolekcia[i],pocet-j,pocet-j)))
    if(as.numeric(substr(kolekcie[i],pocet-j,pocet-j))==1)
    {
      matica[j+1,i]<-mi[dec[i]]
    }
    else
    {
      matica[j+1,i]<-0
    }
  }
}
