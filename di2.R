library(boot)

collection_integral <- function(f, mi, kolekcia)
{
  string_kolekcia<-sapply(kolekcia, make_string, dlzka = length(f))
  print(string_kolekcia)
  if(length(string_kolekcia)==1)
  {
    vysledok <- mi[strtoi(string_kolekcia,2)]
    print("len jeden prvok")
    return(vysledok)
  }
  
  if(is_chain(string_kolekcia))
  {
    vysledok<-chain_int(f,mi,string_kolekcia)
    print("je to chain")
    return(vysledok)
  }
  
  if(is_subpartition(string_kolekcia))
  {
    vysledok<-subpartition_int(f,mi,string_kolekcia)
    print("je to subpartition")
    return(vysledok)
  }
  print("ide cez simplex")
  
  pocet<-length(f) #nech viem kolko prvkov je danych
  funkcia <- mi[strtoi(string_kolekcia, 2)]  #to bude funkcia ktora sa optimalizuje
  rhs<-f #prava strana
  dec<-strtoi(string_kolekcia,2) #kolekcia decimalne
  matica<-matrix(nrow = pocet, ncol = length(string_kolekcia)) #inicializacia przdnej matice na simplexku
  
  for (i in 1:length(string_kolekcia)) 
  {
    matica[, i] <- as.integer(strsplit(string_kolekcia[i], "")[[1]]) == 1
  }
  
  #vypocitanie
  vysledok<-simplex(a = funkcia, A1 = matica, b1 = rhs, maxi = TRUE)$value
  return(unname(vysledok))
}

obj_size(collection_integral(f,mu,c1))

decomposition_integral <- function(f, mi, system)
{
  vysledky<-c()
  for(i in 1:length(system))
  {
    print(system[[i]])
    vysledky<-c(vysledky,collection_integral(f,mi,system[[i]]))
  }
  
  maximum<-max(vysledky)
  poradie<-which.max(vysledky)
  
  result <- list(max_value = maximum, coll_num = poradie)
  class(result) <- "custom_result_class"
  print(c("vysledky:",vysledky))
  return(result)
}

is_chain<-function(kolekcia)
{
  if(length(kolekcia)==1)
  {
    return(TRUE)
  }
  velkost<-max(sapply(kolekcia, function(x) nchar(as.character(x))))
  string_kolekcia<-sapply(kolekcia, make_string, dlzka = velkost)
  pocet<-nchar(string_kolekcia[1])
  dec<-strtoi(string_kolekcia,2)
  poradie<-order(dec)
  ordered<-string_kolekcia[poradie]
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
  if(length(kolekcia)==1)
  {
    return(TRUE)
  }
  velkost<-max(sapply(kolekcia, function(x) nchar(as.character(x))))
  string_kolekcia<-sapply(kolekcia, make_string, dlzka = velkost)
  
  controlSet <- rep(0, velkost)
  
  for (setIndex in seq_along(string_kolekcia)) 
  {
    set <- strsplit(as.character(string_kolekcia[setIndex]), '')[[1]]
    for (i in seq_along(set)) 
    {
      if (set[i] == "1") 
      {
        if ("1" %in% unlist(controlSet)[i]) 
        {
          return(FALSE)  
        }
        controlSet[i]<-"1"
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

make_string<-function(dlzka,cislo)
{
  kolko_pridat<-dlzka-nchar(as.character(cislo))
  string<-paste0(strrep("0", kolko_pridat), cislo)
  return(string)
}

f<-c(1,2,3)
#f<-c(0.2,0.3,0.6)
#mu<-c(0.2,0.4,0.7,0.3,0.6,0.9,1)
mu<-c(1,16,25,81,25,100,196)
k1<-c("111","011","101","001")
k2<-c("011","001")
k3<-c("001")
system<-list(k2,k1,k3)




c1<-c(111,011,101,001)
c2<-c(011,001)
c3<-c(001)
system2<-list(c1,c2,c3)

is_subpartition(c(11001,00100,00011))

collection_integral(f,mu,k1)
decomposition_integral(f,mu,list(c1))
#' do decomposition interalu MUSI ist list. ak tam pojde len jedna kolekcia tak
#' to nepojde dobre, lebo bude kazdy prvok brat ako zvlast kolekciu

