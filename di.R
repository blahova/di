library(boot)

CollectionIntegral <- setRefClass("CollectionIntegral", 
                                  fields = list(
                                    collection = "character",
                                    chain = "logical",
                                    single = "logical",
                                    subpartition = "logical"
                                  ),
                                  methods = list(
                                    initialize = function(coll) {
                                      .self$collection <- sapply(coll, make_string, dlzka = max(nchar(coll)))
                                      .self$single <- length(.self$collection) == 1
                                      .self$chain <- .self$is_chain()
                                      .self$subpartition <- .self$is_subpartition()
                                    },
                                    show = function() {
                                      cat("Kolekcia: \n")
                                      cat(.self$collection, "\n", sep = " ")
                                      cat("Reťazec  ", .self$chain, "\n", sep = "")
                                      cat("Jeden prvok  ", .self$single, "\n", sep = "")
                                      cat("Podrozklad  ", .self$subpartition, "\n", sep = "")
                                    }
                                  )
)


CollectionIntegral$methods(is_chain = function() {
  if (.self$single) {
    return(TRUE)
  }
  
  ordered <- .self$collection[order(strtoi(.self$collection, 2))]
  
  for (i in 1:(length(ordered) - 1)) {
    for (j in 1:nchar(ordered[1])) {
      current_char <- as.integer(substr(ordered[i], j, j))
      next_char <- as.integer(substr(ordered[i + 1], j, j))
      
      if (current_char > next_char) 
      {
        return(FALSE)
      }
    }
  }
  .self$collection<-ordered
  return(TRUE)
})


CollectionIntegral$methods(is_subpartition = function() {
  if(.self$single) {
    return(TRUE)
  }
  controlSet <- rep(0, nchar(as.character(.self$collection[1])))
  
  for (setIndex in seq_along(.self$collection)) {
    set <- strsplit(as.character(.self$collection[setIndex]), '')[[1]]
    for (i in seq_along(set)) {
      if (set[i] == "1") {
        if ("1" == unlist(controlSet)[i]) {
          return(FALSE)  
        }
        controlSet[i] <- "1"
      }
    }
  }
  
  return(TRUE)
})



CollectionIntegral$methods(compute = function(f, mi) {
  pocet<-length(f) #nech viem kolko prvkov je danych
  .self$collection<-sapply(.self$collection, make_string, dlzka = pocet)
  if(.self$single)
  {
    vysledok <- mi[strtoi(.self$collection,2)]
    print("len jeden prvok")
    return(vysledok)
  }
  if(.self$chain)
  {
    vysledok<-.self$chain_int(f,mi)
    print("je to chain")
    return(vysledok)
  }
  if(.self$subpartition)
  {
    vysledok<-.self$subpartition_int(f,mi)
    print("je to subpartition")
    return(vysledok)
  }
  print("ide cez simplex")
  
  funkcia <- mi[strtoi(.self$collection, 2)]  #to bude funkcia ktora sa optimalizuje
  rhs<-f #prava strana
  dec<-strtoi(.self$collection,2) #kolekcia decimalne
  matica <- matrix(0, nrow = pocet, ncol = length(.self$collection)) #inicializacia przdnej matice na simplexku
  
  for (i in 1:length(.self$collection)) {
    vector <- as.integer(strsplit(.self$collection, "")[[1]]) == 1
    matica[, i] <- rev(vector)  
  }
  #vypocitanie
  vysledok<-simplex(a = funkcia, A1 = matica, b1 = rhs, maxi = TRUE)$value
  return(unname(vysledok))
})

CollectionIntegral$methods(chain_int = function(f, mi) {
  num <- length(.self$collection)
  dec <- strtoi(.self$collection, 2)
  
  result <- find_min(f, .self$collection[num]) * mi[dec[num]]
  
  for (i in (num - 1)) {
    result <- result + (find_min(f, .self$collection[i]) - find_min(f, .self$collection[i + 1])) * mi[dec[i]]
  }
  
  return(result)
})

CollectionIntegral$methods(subpartition_int = function(f, mi) {
  dec <- strtoi(.self$collection, 2)
  result <- sum(mi[dec] * sapply(.self$collection, find_min, f = f))
  return(result)
})

decomposition_integral = function(f, mi,system) {
  vysledky <- sapply(system, function(s) s$compute(f, mi))
  
  maximum <- max(vysledky)
  poradie <- which.max(vysledky)
  
  result <- list(max_value = maximum, coll_num = poradie)
  class(result) <- "custom_result_class"
  print(c("vysledky:", vysledky))
  return(result)
}


find_min<-function(f,set)
{
  num<-length(f)
  split<-unlist(strsplit(set,split=""))
  return(min(f[num-(which(split=="1")-1)]))
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
k2<-c("110","001")
k3<-c("001")
system<-list(k2,k1,k3)


c1<-CollectionIntegral$new(c(111,011,101,001))
c2<-CollectionIntegral$new(c(010,001))
c3<-CollectionIntegral$new(c(001))
c2

c1$compute(f,mu)
c2$compute(f,mu)

c2<-CollectionIntegral$new(c(110,100))
c3<-CollectionIntegral$new(c(001))

system2<-list(c1,c2,c3)
decomposition_integral(f,mu,system2)

c2$compute(f,mu)

system2


c1$integral(f,mu)
c1$integral(f2,mu2)


