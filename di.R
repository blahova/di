library(boot)

CollectionIntegral <- setRefClass("CollectionIntegral", 
                                  fields = list(
                                    collection = "character",
                                    dec="numeric",
                                    chain = "logical",
                                    single = "logical",
                                    subpartition = "logical"
                                  ),
                                  methods = list(
                                    initialize = function(coll) {
                                      .self$collection <- sapply(coll, make_string, length = max(nchar(coll)))
                                      .self$dec<-strtoi(.self$collection,2)
                                      .self$single <- length(.self$collection) == 1
                                      .self$chain <- .self$is_chain()
                                      .self$subpartition <- .self$is_subpartition()
                                    },
                                    show = function() {
                                      cat("Kolekcia: \n")
                                      cat(.self$collection, "\n")
                                      cat(.self$dec, "\n", sep = " ")
                                      cat("Reťazec  ", .self$chain, "\n")
                                      cat("Jeden prvok  ", .self$single, "\n")
                                      cat("Podrozklad  ", .self$subpartition, "\n")
                                    }
                                  )
)

DecompositionIntegral <- setRefClass("DecompositionIntegral", 
                                  fields = list(
                                    collections = "list",
                                    pan = "logical",
                                    choquet = "logical",
                                    shilkret="logical"
                                  ),
                                  methods = list(
                                    initialize = function(coll) {
                                      .self$collections <- coll[order(sapply(coll, function(c) length(c$collection)),decreasing = F)]
                                      delete_subsets()
                                      .self$pan<-all(sapply(.self$collections, function(c) c$subpartition))
                                      .self$choquet<-all(sapply(.self$collections, function(c) c$chain))
                                      .self$shilkret<-all(sapply(.self$collections, function(c) c$single))
                                    },
                                    show = function() {
                                      sapply(.self$collections, function(c) cat(c$collection, "\n"))
                                      cat("PAN  ", .self$pan, "\n")
                                      cat("Choquet  ", .self$choquet, "\n")
                                      cat("Shilkret  ", .self$shilkret, "\n")
                                    },
                                    delete_subsets=function(){
                                      del<-c()
                                      for (i in 1:(length(.self$collections)-1))
                                      {
                                        for (j in (i+1):length(.self$collections))
                                        {
                                          if(all(.self$collections[[i]]$dec%in%.self$collections[[j]]$dec))
                                          {
                                            del<-c(del,i)
                                          }
                                        }
                                      }
                                      if (length(del) > 0) {
                                        .self$collections <- .self$collections[-del]
                                      }
                                    }
                                  )
)


CollectionIntegral$methods(is_chain = function() {
  if (.self$single) {
    return(TRUE)
  }
  
  ordered <- .self$collection[order(.self$dec)]
  
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
  .self$dec<-.self$dec[order(.self$dec)]
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
  if(.self$single)
  {
    result <- mi[.self$dec]
    print("len jeden prvok")
    return(result)
  }
  if(.self$chain)
  {
    result<-.self$chain_int(f,mi)
    print("je to chain")
    return(result)
  }
  if(.self$subpartition)
  {
    result<-.self$subpartition_int(f,mi)
    print("je to subpartition")
    return(result)
  }
  print("ide cez simplex")
  num<-length(f) #nech viem kolko prvkov je danych
  .self$collection<-sapply(.self$collection, make_string, length = num)
  
  fun <- mi[strtoi(.self$collection, 2)]  #to bude funkcia ktora sa optimalizuje
  rhs<-f #prava strana
  matrix <- matrix(0, nrow = num, ncol = length(.self$collection)) #inicializacia przdnej matice na simplexku
  
  for (i in 1:length(.self$collection)) {
    vector <- strsplit(.self$collection, "")[[i]] == 1
    matrix[, i] <- rev(vector)  
  }
  #vypocitanie
  result<-simplex(a = fun, A1 = matrix, b1 = rhs, maxi = TRUE)$value
  return(unname(result))
})

CollectionIntegral$methods(chain_int = function(f, mi) {
  num <- length(.self$collection)
  
  result <- find_min(f, .self$collection[num]) * mi[.self$dec[num]]
  for (i in (num - 1)) {
    result <- result + (find_min(f, .self$collection[i]) - find_min(f, .self$collection[i + 1])) * mi[dec[i]]
  }
  
  return(result)
})


CollectionIntegral$methods(subpartition_int = function(f, mi) {
  result <- sum(mi[.self$dec] * sapply(.self$collection, find_min, f = f))
  return(result)
})

DecompositionIntegral$methods(compute=function(f,mi){
  results <- sapply(.self$collections, function(s) s$compute(f, mi))
  
  maximum <- max(results)
  order <- which.max(results)
  
  result <- list(max_value = maximum, coll_num = order)
  class(result) <- "custom_result_class"
  print(c("vysledky:", results))
  return(result)
  
})



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

make_string<-function(length,number)
{
  to_add<-length-nchar(as.character(number))
  string<-paste0(strrep("0", to_add), number)
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
c2<-CollectionIntegral$new(c(110,100))
c3<-CollectionIntegral$new(c(001))
c2$compute(f,mu)

c2$compute(f,mu)

c1$compute(f,mu)
c3$compute(f,mu)

c2<-CollectionIntegral$new(c(110,100))
c3<-CollectionIntegral$new(c(001))

system2<-list(c1,c2,c3)
decomposition_integral(f,mu,system2)

c2$compute(f,mu)

system2


c1$integral(f,mu)
c1$integral(f2,mu2)



c1<-CollectionIntegral$new(c(111,011,101,001))
c2<-CollectionIntegral$new(c(110,100))
c3<-CollectionIntegral$new(c(001))
syste<-DecompositionIntegral$new(list(c1,c2,c3))
syste$compute(f,mu)
syste$collections[[2]]$compute(f,mu)

all(sapply(syste$collections, function(c) c$chain))


c2$compute(f,mu)
