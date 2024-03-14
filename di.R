library(boot)

Collection <- setRefClass("Collection", 
                          fields = list(
                            set_system = "numeric",
                            is_chain = "logical",
                            is_single = "logical",
                            is_subpartition = "logical"),
                          methods = list(
                            initialize = function(set_system) {
                              .self$set_system <- set_system
                              .self$is_chain <- is_chain(.self$set_system)
                              .self$is_single <- length(.self$set_system) == 1
                              .self$is_subpartition <- is_subpartition(.self$set_system)
                            },
                            show = function() {
                              cat("Kolekcia: \n")
                              cat(.self$set_system, "\n", sep = " ")
                              cat("Reťazec  ", .self$is_chain, "\n", sep = "")
                              cat("Jeden prvok  ", .self$is_single, "\n", sep = "")
                              cat("Podrozklad  ", .self$is_subpartition, "\n", sep = "")
                            }
                          )
)

Collection$methods(is_chain = function() {
  if (length(.self$set_system) == 1) {
    return(TRUE)
  }
  
  string_kolekcia <- sapply(.self$set_system, make_string, dlzka = max(nchar(.self$set_system)))
  ordered <- string_kolekcia[order(strtoi(string_kolekcia, 2))]
  
  pocet <- nchar(ordered[1])
  
  for (i in 1:(length(ordered) - 1)) {
    for (j in 1:pocet) {
      current_char <- as.integer(substr(ordered[i], j, j))
      next_char <- as.integer(substr(ordered[i + 1], j, j))
      
      if (current_char > next_char) 
      {
        return(FALSE)
      }
    }
  }
  
  return(TRUE)
})

Collection$methods(is_subpartition = function() {
  if(length(.self$set_system)==1)
  {
    return(TRUE)
  }
  velkost<-max(sapply(.self$set_system, function(x) nchar(as.character(x))))
  string_kolekcia<-sapply(.self$set_system, make_string, dlzka = velkost)
  
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
})



Collection$methods(integral = function(f, mi) {
  pocet<-length(f) #nech viem kolko prvkov je danych
  string_kolekcia<-sapply(.self$set_system, make_string, dlzka = pocet)
  if(.self$is_single)
  {
    vysledok <- mi[strtoi(string_kolekcia,2)]
    print("len jeden prvok")
    return(vysledok)
  }
  if(.self$is_chain)
  {
    vysledok<-chain_int(f,mi,string_kolekcia)
    print("je to chain")
    return(vysledok)
  }
  if(.self$is_subpartition)
  {
    vysledok<-subpartition_int(f,mi,string_kolekcia)
    print("je to subpartition")
    return(vysledok)
  }
  print("ide cez simplex")
  
  funkcia <- mi[strtoi(.self$set_system, 2)]  #to bude funkcia ktora sa optimalizuje
  rhs<-f #prava strana
  dec<-strtoi(.self$set_system,2) #kolekcia decimalne
  matica <- matrix(0, nrow = pocet, ncol = length(.self$set_system)) #inicializacia przdnej matice na simplexku
  
  for (i in 1:length(string_kolekcia)) {
    vector <- as.integer(strsplit(string_kolekcia, "")[[1]]) == 1
    matica[, i] <- rev(vector)  
  }
  #vypocitanie
  vysledok<-simplex(a = funkcia, A1 = matica, b1 = rhs, maxi = TRUE)$value
  return(unname(vysledok))
})


Decomposition_System <- setRefClass("Decomposition_System", 
                                    fields = list(
                                      system = "list"),
                                    methods = list(
                                      initialize = function(system) {
                                        .self$system <- system
                                      },
                                      show = function() {
                                        cat("Dekompozičný Systém: \n")
                                        for (collection in .self$system) {
                                          collection$show()
                                          cat("\n")# Call the show method of each Collection object
                                        }
                                      }
                                    )
)

Decomposition_System$methods(integral = function(f, mi) {
  vysledky <- sapply(.self$system, function(s) s$integral(f,mi))
  
  maximum <- max(vysledky)
  poradie <- which.max(vysledky)
  
  result <- list(max_value = maximum, coll_num = poradie)
  class(result) <- "custom_result_class"
  print(c("vysledky:", vysledky))
  return(result)
})


system1<-Decomposition_System$new(list(c1,c2,c3))
system1




find_min<-function(f,mnozina)
{
  pocet<-length(f)
  rozd<-unlist(strsplit(mnozina,split=""))
  
  return(min(f[pocet-(which(rozd=="1")-1)]))
}

chain_int <- function(f, mi, kolekcia) {
  pocet <- length(kolekcia)
  ordered <- kolekcia[order(kolekcia)]
  dec <- strtoi(ordered, 2)
  
  vysledok <- find_min(f, ordered[pocet]) * mi[dec[pocet]]
  
  for (i in seq_len(pocet - 1)) {
    vysledok <- vysledok + (find_min(f, ordered[i]) - find_min(f, ordered[i + 1])) * mi[dec[i]]
  }
  
  return(vysledok)
}


subpartition_int <- function(f, mi, kolekcia) {
  dec <- strtoi(kolekcia, 2)
  vysledok <- sum(mi[dec] * sapply(kolekcia, find_min, f = f))
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
k2<-c("110","001")
k3<-c("001")
system<-list(k2,k1,k3)



c1<-Collection$new(c(111,011,101,001))
c2<-Collection$new(c(110,001))
c3<-Collection$new(c(001))


system2<-list(c1,c2,c3)

is_subpartition(c(11001,00100,00011))

collection_integral(f,mu,c("011","001"))
decomposition_integral(f,mu,list(k2))
#' do decomposition interalu MUSI ist list. ak tam pojde len jedna kolekcia tak
#' to nepojde dobre, lebo bude kazdy prvok brat ako zvlast kolekciu

