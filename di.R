library(boot)

collection_integral <- function(f, mi, kolekcia)
{
  if(length(kolekcia)==1)
  {
    vysledok <- mi[strtoi(kolekcia,2)]
    print("len jeden prvok")
    return(vysledok)
  }
  
  if(is_chain(kolekcia))
  {
    vysledok<-chain_int(f,mi,kolekcia)
    print("je to chain")
    return(vysledok)
  }
  
  if(is_subpartition(kolekcia))
  {
    vysledok<-subpartition_int(f,mi,kolekcia)
    print("je to subpartition")
    return(vysledok)
  }
  print("ide cez simplex")
  
  pocet<-length(f) #nech viem kolko prvkov je danych
  funkcia<-c()  #to bude funkcia ktora sa optimalizuje
  rhs<-f #prava strana
  dec<-strtoi(kolekcia,2) #kolekcia decimalne
  matica<-matrix(nrow = pocet, ncol = length(kolekcia)) #inicializacia przdnej matice na simplexku
  
  for (i in 1:length(kolekcia)) #precadzam prvkami kolekcie
  {
    funkcia<-c(funkcia,mi[dec[i]]) #pridam do funkcie hodnotu pre prvok kolekcie
    for(j in 0:(pocet-1)) #precadzam jednotlive prvky kolekcie po znakoch
    {
      if(as.numeric(substr(kolekcia[i],pocet-j,pocet-j))==1)
      {
        matica[j+1,i]<-1
      }
      else
      {
        matica[j+1,i]<-0
      }
    }
  }
  #vypocitanie
  vysledok<-simplex(a = funkcia, A1 = matica, b1 = rhs, maxi = TRUE)$value
  return(unname(vysledok))
}

decomposition_integral <- function(f, mi, system)
{
  vysledky<-c()
  for(i in 1:length(system))
  {
    vysledky<-c(vysledky,collection_integral(f,mi,system[[i]]))
  }
  
  maximum<-max(vysledky)
  poradie<-which.max(vysledky)
  
  result <- list(max_value = maximum, coll_num = poradie)
  class(result) <- "custom_result_class"
  print(vysledky)
  return(result)
}

is_chain<-function(kolekcia)
{
  #' usporiadam kolekciu podla jej numerickej hodnoty vzostupne
  #' potom prechadzam po znakoch
  #' ak je znak 0 tak idem na dalsi znak
  #' ak je znak 1, pozriem sa, ze ci aj v dalsom clene je na danom mieste 1
    #' ak nie je, tak break, a return FALSE, lebo uz to neni podmozina
    #' ak to je 1, tak pokracujem na dalsi znak rovnako
  #' nuly mozem vzdy neriesit lebo ak tam je nula, bola nula na tom mieste aj 
  #' doteraz
  pocet<-nchar(kolekcia[1])
  dec<-strtoi(kolekcia,2)
  poradie<-order(dec)
  ordered<-kolekcia[poradie]
  for(i in 1:(length(ordered)-1))
  {
    for(j in 1:pocet)
    {
      if(as.numeric(substr(ordered[i],j,j))==1)
      {
        if(as.numeric(substr(ordered[i+1],j,j))!=1)
        {
          return(FALSE)
        }
        else
        {
          next
        }
      }
      else
      {
        next
      }
    }
  }
  return(TRUE)
}

is_subpartition<-function(kolekcia)
{
  pocet<-nchar(kolekcia[1])
  dec<-strtoi(kolekcia,2)
  poradie<-order(dec,decreasing = TRUE)
  ordered<-kolekcia[poradie]
  
  for(i in 1:(length(ordered)-1))
  {
    for(j in 1:pocet)
    {
      if(as.numeric(substr(ordered[i],j,j))==1)
      {
        if(as.numeric(substr(ordered[i+1],j,j))==1)
        {
          return(FALSE)
        }
      }
      else
      {
        next
      }
    }
  }
  return(TRUE)
}

find_min<-function(f,mnozina)
{
  pocet<-length(f)
  rozd<-unlist(strsplit(mnozina,split=""))
  
  return(min(f[pocet-(which(rozd=="1")-1)]))
}

chain_int<-function(f,mi,kolekcia)
{
  pocet<-length(kolekcia) #kolko mnozin je v kolekcii
  poradie<-order(kolekcia)
  ordered<-kolekcia[poradie]  #usporiadana kolekcia od najvacsej
  dec<-strtoi(ordered,2) #ursporiadana kolekcia decimalne

  vysledok<-find_min(f,ordered[pocet])*mi[dec[pocet]]
  
  for(i in 1:(pocet-1))
  {
    vysledok<-vysledok+(find_min(f,ordered[i])-find_min(f,ordered[i+1]))*mi[dec[i]]
  }
  return(vysledok)
}

subpartition_int<-function(f,mi,kolekcia)
{
  pocet<-length(kolekcia) #kolko mnozin je v kolekcii
  dec<-strtoi(kolekcia,2) #kolekcia decimalne
  
  vysledok<-0
  
  for(i in 1:pocet)
  {
    vysledok<-vysledok+mi[dec[i]]*find_min(f,kolekcia[i])
  }
  return(vysledok)
}

#overridenuta funkcia na vypis vysledku dekompozicneho integralu
print.custom_result_class <- function(x) {
  cat("Max Value:", x$max_value, "\n")
  cat("Found in collection no.:", x$coll_num, "\n")
}

f<-c(1,2,3)
#f<-c(0.2,0.3,0.6)
#mu<-c(0.2,0.4,0.7,0.3,0.6,0.9,1)
mu<-c(1,16,25,81,25,100,196)
k1<-c("111","011","101","001")
k2<-c("011","001")
k3<-c("001")
system<-list(k2,k1,k3)




collection_integral(f,mu,k1)
decomposition_integral(f,mu,system)

