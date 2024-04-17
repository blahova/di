library(boot)

CollectionIntegral <- setRefClass("CollectionIntegral", 
                                  fields = list(
                                    collection = "list",
                                    dec="numeric",
                                    chain = "logical",
                                    single = "logical",
                                    subpartition = "logical",
                                    pc="logical"
                                  ),
                                  methods = list(
                                    initialize = function(coll) {
                                      .self$collection <- coll[!duplicated(coll)]
                                      .self$dec<-sapply(.self$collection, logical_to_decimal)
                                      .self$collection <- .self$collection[order(.self$dec)]
                                      .self$dec <- .self$dec[order(.self$dec)]
                                      .self$single <- length(.self$collection) == 1
                                      .self$chain <- .self$is_chain()
                                      .self$subpartition <- .self$is_subpartition()
                                      .self$pc<-.self$is_pc()
                                    },
                                    show = function() {
                                      cat("Kolekcia: \n")
                                      invisible(sapply(.self$collection, function(c) { cat(as.integer(c), "\n"); NULL }))
                                      cat(.self$dec, "\n", sep = " ")
                                      cat("Reťazec  ", .self$chain, "\n")
                                      cat("Jeden prvok  ", .self$single, "\n")
                                      cat("Podrozklad  ", .self$subpartition, "\n")
                                      cat("PC ", .self$pc, "\n")
                                    }
                                  )
)

DecompositionIntegral <- setRefClass("DecompositionIntegral", 
                                     fields = list(
                                       collections = "list",
                                       choquet = "logical"
                                     ),
                                     methods = list(
                                       initialize = function(coll) {
                                         .self$collections <- coll[order(sapply(coll, function(c) length(c$collection)),decreasing = F)]
                                         delete_subsets()
                                         .self$choquet<-.self$is_choquet()
                                       },
                                       show = function() {
                                         for (obj in .self$collections) {
                                           cat("Kolekcia:\n")
                                           sapply(obj$collection, function(c) cat(as.integer(c), "\n"))
                                           cat("\n")
                                         }
                                         cat("Choquet  ", .self$choquet, "\n")
                                       }
                                       
                                     )
)

DecompositionIntegral$methods(delete_subsets=function(){
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
})

CollectionIntegral$methods(is_chain = function() {
  if (.self$single) {
    return(TRUE)
  }

  for (i in 1:(length(.self$collection) - 1)) {
    control_element<-.self$collection[[i]]
    compare_element <- .self$collection[[i+1]]
    check_indexes <- which(control_element)
    if (!all(compare_element[check_indexes])) {
      return(FALSE)
    }
  }
  return(TRUE)
})


CollectionIntegral$methods(is_subpartition = function() {
  if (.self$single) {
    return(TRUE)
  }
  
  controlSet <- logical(length(.self$collection[[1]]))
  
  for (setIndex in seq_along(.self$collection)) {
    set <- .self$collection[[setIndex]]
    for (i in seq_along(set)) {
      if (set[i]) {
        if (controlSet[i]) {
          return(FALSE)
        }
        controlSet[i] <- TRUE
      }
    }
  }
  
  return(TRUE)
})

CollectionIntegral$methods(is_pc=function(){
  if(.self$single || .self$chain || .self$subpartition)
  {
    return(TRUE)
  }
  control_coll<-.self$collection
  
  chains <- recursive_function(control_coll)
  #print("chains")
  #print(chains)
  
  
  controlSet <- logical(length(chains[[1]][[1]]))
  
  for (setIndex in seq_along(chains)) {
    set <- chains[[setIndex]][[length(chains[[setIndex]])]]
    for (i in seq_along(set)) {
      if (set[i]) {
        if (controlSet[i]) {
          return(FALSE)
        }
        controlSet[i] <- TRUE
      }
    }
  }
  return(TRUE)
})


DecompositionIntegral$methods(is_choquet = function() {
  n<-length(.self$collections[[1]]$collection[[1]]) #pocet prvkov v mnozine
  chains<-all(sapply(.self$collections, function(c) c$chain)) #ci su vsetky chain
  if (!chains) return(FALSE)
  len<-all(sapply(.self$collections, function(c) length(c$collection))==n) #ci su vsetky kolekcie dlzky ako mnozina
  if (!len) return(FALSE)
  num<-factorial(n)==length(.self$collections)
  if (!num) return(FALSE)

  return(TRUE)
})

DecompositionIntegral$methods(choquet_int = function(f,mi) {
  choquet_coll <- lapply(f, function(x) rev(f >= x))
  choquet_coll<-CollectionIntegral$new(choquet_coll)
  result<-choquet_coll$compute(f,mi)
  result <- list(max_value = result, coll_num = "NA, went through choquet")
  return(result)
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
    print("Je to chain")
    result<-.self$chain_int(f,mi)
    return(result)
  }
  if(.self$subpartition)
  {
    result<-.self$subpartition_int(f,mi)
    print("je to subpartition")
    return(result)
  }
  if(.self$pc)
  {
    print("Je to pc")
    result<-.self$pc_int(f,mi)
    return(result)
  }

  
  print("ide cez simplex")
  
  fun <- mi[.self$dec]  #to bude funkcia ktora sa optimalizuje
  rhs<-f #prava strana
  matrix <- matrix(0, nrow = length(f), ncol = length(.self$collection)) #inicializacia przdnej matice na simplexku
  
  for (i in 1:length(.self$collection)) {
    matrix[, i] <- rev(.self$collection[[i]])
  }
  #vypocitanie
  result<-simplex(a = fun, A1 = matrix, b1 = rhs, maxi = TRUE)$value
  return(unname(result))
})


CollectionIntegral$methods(chain_int = function(f, mi) {
  num <- length(.self$collection)
  result <- find_min(f, .self$collection[[num]]) * mi[.self$dec[num]]
  if(.self$single)
  {#sem to pride len v pripade pc kolekcie
    return(result)
  }
  for (i in 1:(num-1)) {
    result <- result + (find_min(f, .self$collection[[i]]) - find_min(f, .self$collection[[i + 1]])) * mi[.self$dec[i]]
  }
  print(result)
  return(result)
})



CollectionIntegral$methods(subpartition_int = function(f, mi) {
  result <- sum(mi[.self$dec] * sapply(.self$collection, find_min, f = f))
  return(result)
})

CollectionIntegral$methods(pc_int = function(f,mi){
  chains <- recursive_function(.self$collection)
  result <- 0
  print(chains)
  num<-length(chains)
  
  for(chainIndex in 1:num)
  {
    print(chains[[chainIndex]])
    tmp_coll<-CollectionIntegral$new(chains[[chainIndex]])
    tmp_coll$show()
    tmp_result<-tmp_coll$chain_int(f,mi)
    print(tmp_result)
    result<- result + tmp_result
  }
  return(result)
})

c4<-CollectionIntegral$new(list(c(T,F,T),c(F,T,F),c(T,F,F)))
c4$compute(f,mu)

DecompositionIntegral$methods(compute=function(f,mi){
  if(.self$choquet)
  {
    result<-.self$choquet_int(f,mi)
    class(result) <- "custom_result_class"
    return(result)
  }
  results <- sapply(.self$collections, function(s) s$compute(f, mi))
  
  maximum <- max(results)
  order <- which.max(results)
  
  result <- list(max_value = maximum, coll_num = order)
  class(result) <- "custom_result_class"
  print(c("vysledky:", results))
  return(result)
  
})

recursive_function <- function(control_coll, control_element = NULL, result_list = list(),current_chain = list()) {
  # zastavovacie kriterium
  if (length(control_coll) == 0) {
    #print("už je control_coll prazdne")
    if (!is.null(control_element)) {
      result_list<-c(result_list, list(current_chain))
    }
    #print(result_list)
    return(result_list)
  }
  #print(control_coll)
  # prve z control_coll ako control_element
  if (is.null(control_element)) {
    #print("beriem control element")
    control_element <- control_coll[[1]]
    current_chain<-c(current_chain, list(control_element))
    #print("pridal sa control element")
    #print(current_chain)
    #print(control_element)
    control_coll <- control_coll[-1]
    #print("mažem  z control coll, nove je:")
    #print(control_coll)
  }
  
  # hladanie zhody
  match_found <- FALSE
  for (i in seq_along(control_coll)) {
    #print("vyberám compare element")
    compare_element <- control_coll[[i]]
    #print(compare_element)
    # checknutie či je subset
    check_indexes <- which(control_element)
    if (all(compare_element[check_indexes])) {
      #print("našla sa zhoda")
      match_found <- TRUE
      # rekurzia s compare_element ako control_element
      #print("result list vyzerá takto")
      #print(result_list)
      current_chain <- c(current_chain, list(compare_element))
      result_list <- recursive_function(control_coll[-i], compare_element, result_list,current_chain)
      
      return(result_list)
    }
  }
  
  # ak sa nenašiel chain tak rekurzia 
  if (!match_found) {
    result_list<-c(result_list, list(current_chain))
    #print("nenašla sa zhoda")
    #print("result list vyzerá takto:")
    #print(result_list)
    result_list <- recursive_function(control_coll, NULL, result_list)
    return(result_list)
  }
}

find_min <- function(f, set) {
  num <- length(f)
  return(min(f[num - (which(set) - 1)]))
}


#overridenuta funkcia na vypis vysledku dekompozicneho integralu
print.custom_result_class <- function(x) {
  cat("Max Value:", x$max_value, "\n")
  cat("Found in collection no.:", x$coll_num, "\n")
}

logical_to_decimal <- function(logical_vec) {
  decimal_num <- sum(logical_vec * 2^(rev(seq_along(logical_vec)) - 1))
  return(decimal_num)
}

f<-c(1,2,3)
mu<-c(1,16,25,81,100,169,196)



c1<-CollectionIntegral$new(list(c(T,T,T),c(F,T,T),c(T,F,T),c(F,F,T)))
c2<-CollectionIntegral$new(list(c(T,F,F),c(T,T,F)))
c3<-CollectionIntegral$new(list(c(F,F,T)))
c4<-CollectionIntegral$new(list(c(T,F,F),c(F,T,F),c(F,F,T)))
c4$compute(f,mu)
c4$subpartition_int(f,mu)


c1


ch1<-CollectionIntegral$new(list(c(T,T,T),c(F,T,T),c(F,F,T)))
ch2<-CollectionIntegral$new(list(c(T,T,T),c(F,T,T),c(F,T,F)))
ch3<-CollectionIntegral$new(list(c(T,T,T),c(T,F,T),c(F,F,T)))
ch4<-CollectionIntegral$new(list(c(T,T,T),c(T,F,T),c(T,F,F)))
ch5<-CollectionIntegral$new(list(c(T,T,T),c(T,T,F),c(F,T,F)))
ch6<-CollectionIntegral$new(list(c(T,T,T),c(T,T,F),c(T,F,F)))
choquet<-DecompositionIntegral$new(list(ch1,ch2,ch3,ch4,ch5,ch6))
choquet$compute(f,mu)



syste<-DecompositionIntegral$new(list(c1,c2,c3))
syste$compute(f,mu)
syste



test<- list(list(c(T,T,F),c(T,F,F)),list(c(F,F,T)))
test[[2]]
