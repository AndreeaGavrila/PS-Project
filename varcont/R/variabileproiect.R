#cerinta1
#determinarea constantei de normalizare
#' @export
constantaNorm <- function(f)
{

  tryCatch({

    result<-integrate(Vectorize(f), lower = -Inf, upper= Inf)$value

    return(1 / result)
  },

  error = function(err){
    message(paste("Nu se poate calcula constanta de normalizare -> eroare:", err))

  })

}

#Problema 2

#Functia are ca parametri o functie f si suportul ei (lista de vectori de cate 2 elemente), pentru care verificam cele 2 proprietati:
#1.) f(x)>=0 oricare ar fi x din suport
#2.) suma integralelor de pe fiecare suport sa fie egala cu 1
#Prima data functia verifica prima conditie:
#in cazul in care unul dintre capetele intervalului este -Inf sau Inf, imi creez niste copii ale lor pe care le modific in functie de caz
#altfel instructiunea seq de la lim_inf la lim_sup ar ar genera o eroare
#apoi sapply imi creeaza un vector cu elementele secventei peste care aplic functia f
#any imi verifica daca exista cel putin un element negativ in noul vector
#in caz afirmativ, returnez false, inseamna ca prima proprietate nu este indeplinita
#Apoi in try catch incerc sa integrez functia f pe fiecare interval si sa adun rezultatele in valoare_integrala
#daca vreunul dintre capete este -Inf sau Inf, eroare va fi prinsa in catch si se va afisa un mesaj corespunzator
#In final, returnez daca suma acelor integrale este 1
#' @export
verifica_functie <- function(f, intervale) {
  valoare_integrala <- 0
  for (i in intervale) {

    lim_inf <- i[1]
    lim_sup <- i[2]

    if (lim_inf == -Inf && lim_sup == Inf) {
      lim_inf <- -10000
      lim_sup <-  10000
    }
    else if (lim_inf == -Inf)
      lim_inf <- lim_sup - 10000
    else if (lim_sup ==  Inf)
      lim_sup <- lim_inf + 10000

    vector_auxiliar <- seq(lim_inf, lim_sup, length.out = 10000)
    if (any(sapply(vector_auxiliar, f) < 0)) {
      print(paste("Valoare negativa in [", lim_inf, ", ", lim_sup, "]", sep = ""))
      return(FALSE)
    }

    tryCatch(
      valoare_integrala <- valoare_integrala + integrate(Vectorize(f), i[1], i[2])$value,
      error = function(e){
        print(paste("Eroare pe intervalul [" , i[1], ", " , i[2] , "]: integrala divergenta",  sep = ""))
        return(FALSE)
      }
    )

  }

  valoare_integrala == 1
}

#cerinta 3
#' @export
setClass("myclass", slots=list (density ="function", densityX = "function", densityY = "function",
                                lower="function", bidimen="logical", base="list"))

#' @export
myclass <- function( density, densityX = function(x) NULL, densityY = function(x) NULL,
                     lower = function(x) x, bidimen = FALSE, base = list(c(-Inf, Inf)) )

{
  # cand avem v.a bidimensionale, baza va fi formata din listele bazei densitatii lui X si Y.
  # cand avem v.a unidimensionale, baza va fi lista intervalelor enumerate.

  if( length(base) < 2 ) # verific lungimea listei din baza
  { base <- list(base, list()) }

  if ( bidimen ) # pentru v.a bidimensionale
    if ( missing(lower) ) # daca nu e specificat capatul inferior
    { lower = function(x, y) x * y } # completez capatul inferior pentru v.a bidimensionale

  result <- new("myclass", density = density, lower = lower, bidimen = bidimen,
                base = base, densityX = densityX, densityY = densityY)
  #atribui toate valorile necesare in result utlizand new,
  # este un constructor sa generez un obiect nou
  return (result)
}

#' @export
setGeneric("ProbFunc", function(object) standardGeneric("ProbFunc"))
setMethod("ProbFunc", "myclass",
          function (object) {
            return (prob(object))
          })

#' @export
# tratez si cazul probabilitatii conditionate
setMethod("ProbFunc", "numeric",
          function (object) {
            return (object)
          })

#' @export
setGeneric("MediaFunc", function(object) standardGeneric("MediaFunc"))
setMethod("MediaFunc", "myclass",
          function(object){
            return(medieVA(object))
          })

#' @export
setGeneric("DispersiaFunc", function(object) standardGeneric("DispersiaFunc"))
setMethod("DispersiaFunc", "myclass",
          function(object) return(dispersieVA(object)))

#' @export
compoz <- function(f, g)
{
  function(...) f(g(...))
}

#' @export
setGeneric("method", function(object, f) standardGeneric("method"))
setMethod("method", "myclass",
          function(object, f){
            retval <- myclass(object@density, Vectorize(compoz(f, object@lower)),
                              object@bidimen, object@base[[1]])
            # baza densitatii in cazul v.a unidimensionale, adica al lui X
          })

# functia de afisare dupa cum imi trebuie
#' @export
setGeneric("afisare", function(object) standardGeneric("afisare"))
setMethod("afisare", "myclass",
          function (object) {
            cat("Densitatea de probabilitate: ")
            print(body(fun = object@density))

            cat("Variabila Aleatoare Bidimensionala: ", object@bidimen, "\n")

            cat("Baza densitatii: ")

            if ( object@bidimen == TRUE)  # cand avem v.a. bidimensionale

              for (i in object@base[[1]]) # baza densitatii lui X
                for (j in object@base[[2]]) # baza densitatii lui Y
                {
                  if ( i[1] == j[1] & i[2] == j[2]) # baza densitatii lui X si Y cand sunt egale
                    cat("[", i[1], ",", i[2], "] ")
                  else
                  { cat("[", i[1], ",", i[2], "] ")
                    cat( "U")
                    cat(" [", j[1], ",", j[2], "] ") }
                }
            else
              # cand avem v.a. unidimensionale
              for (i in object@base[[1]]) # baza densitatii lui X
                cat("[", i[1], ",", i[2], "] ")

            cat("\n")
            cat("Capatul inferior: ")
            print(body(fun = object@lower))
          })

#cerinta 4
#Reprezentarea grafica a densita??ii ??i a func??iei de reparti??ie pentru diferite valori ale
#parametrilor reparti??iei. ??n cazul ??n care func??ia de reparti??ie nu este data ??ntr-o forma
#explicita(ex. reparti??ia normala) se accepta reprezentarea grafica a unei aproximari a
#acesteia.


#' @export
densitate_n <- function(medie = 0, v = 1)
{
  #de medie Âµ si variantÄ Ï^2

  curve(expr = dnorm(x, medie, v),
        from = medie - 3 * v,
        to   = medie + 3 * v,
        ylab = "fi(x)",
        main = "Densitatea repartitiei normale",
        col = "red",
        lwd =2)

}

#' @export
repartitie_n <- function(medie = 0, v = 1) {

  #de medie Âµ si variantÄ Ï^2

  curve(expr = pnorm(x, medie, v),
        from = medie - 3 * v,
        to   = medie + 3 * v,
        ylab = "FI(x)",
        main = "Functia de repartitie normala",
        col = "blue",
        lwd =2)
}



#DENSITATEA SI FUNCTIA DE REPARTITIE PENTRU REP. UNIFORMA
#X repartizatÄ uniform pe intervalul [a, b]
#' @export
densitate_u <-function(a, b)
{

  curve(expr = dunif(x, a, b),
        from = - 3 * b,
        to   =  3 * b,
        ylab = "fX(x)",
        main = "Densitatea repartitiei uniforme",
        col = "red",
        lwd =2)

}

#' @export
repartitie_u <- function(a, b)
{

  curve(expr = punif(x, a, b),
        from = - 3 * b,
        to   =  3 * b,
        ylab = "FX(x)",
        main = "Functia de repartitie uniforma",
        col = "blue",
        lwd =2)

}


#DENSITATEA SI FUNCTIA DE REPARTITIE BETA
#X este repartizatÄ Beta de parametrii (Î±, Î²), cu Î±, Î² > 0,

#' @export
densitate_b <- function(a, b) {
  curve(expr = dbeta(x, a, b),
        from = 0,
        to   = 1,
        ylab = "f(x)",
        main = "Densitatea in repartitia beta",
        col = "maroon",
        lwd =2)
}
#' @export
repartitie_b <- function(a, b)
{
  curve(expr = pbeta(x, a, b),
        from = 0,
        to   = 1,
        ylab = "F(x)",
        main = "Functia de repartitie beta",
        col = "blue",
        lwd =2)
}




#DENSITATEA SI FUNCTIA DE REPARTITIE GAMMA
# X este repartizatÄ Gama de parametrii (Î±, Î²), cu Î±, Î² > 0,
#' @export
densitate_g <- function(a, b){

  curve(expr = dgamma(x, a, b),
        from = 0,
        to   = 20,
        ylab = "f(x)",
        main = "Densitatea in repartitia gamma",
        col = "maroon",
        lwd =2)


}

#' @export
repartitie_g <- function(a, b){

  curve(expr = pgamma(x, a, b),
        from = 0,
        to   = 20,
        ylab = "F(x)",
        main = "Functia de repartitie gamma",
        col = "red",
        lwd =2)

}


#DENSITATEA SI FUNCTIA DE REPARTITIE EXPONENTIALA

#X este repartizatÄ exponential de parametru Î»
#' @export
densitate_e <- function(lbd){

  curve(expr = dexp(x, lbd),
        from = 0,
        to   = 20,
        ylab = "f(x)",
        main =" Densitatea in repartitie exponentiala",
        col = "red",
        lwd =2)
}
#' @export
repartite_e <- function(lbd){

  curve(expr = pexp(x, lbd),
        from = 0,
        to   = 5,
        ylab = "F(x)",
        main ="Functia de repartitie exponentiala",
        col = "orange",
        lwd =2)

}


#DENSITATEA SI FUNCTIA DE REPARTITIE CAUCHY
#X este repartizatÄ Cauchy de parametrii (0, 1)
#' @export
densitate_c <- function(loc, sc){

  curve(expr = dcauchy(x, loc, sc),
        from = 0,
        to   = 5,
        ylab = "f(x)",
        main ="Densitatea de repartitie Cauchy",
        col = "orange",
        lwd =2)

}

#' @export
repartitie_c <- function(loc, sc){

  curve(expr = pcauchy(x, loc, sc),
        from = 0,
        to   = 5,
        ylab = "F(x)",
        main ="Functia de repartitie Cauchy",
        col = "blue",
        lwd =2)

}

#cerinta 5
#calcularea mediei
#' @export
medieVA <- function(f)
{
  tryCatch({
    fcalcul<- function(x) { x*f(x)}
    result<-integrate(Vectorize(fcalcul), lower = -Inf, upper= Inf)$value
    return (result)
  },
  error = function(err){
    message(paste("Nu se poate calcula media -> eroare:", err))
  })
}

#calcularea dispersiei
#' @export
dispersieVA <- function(f)
{
  tryCatch({
    fcalcul<- function(x) { ((x-medieVA(f))^2)*f(x)}
    result<-integrate(Vectorize(fcalcul), lower = -Inf, upper= Inf)$value
    return (result)
  },
  error = function(err){
    message(paste("Nu se poate calcula dispersia -> eroare:", err))
  })

}

#calcularea momentelor initiale
#' @export
momenteInitVA <- function(f)
{
  lista<- list()

  tryCatch({
    for(i in 1:4)
    {
      tryCatch({
        fcalcul<- function(x) { ((x)^i)*f(x)}
        result<-integrate(Vectorize(fcalcul), lower = -Inf, upper= Inf)$value
        ##print(result)
        lista<-append(lista, result)
      },
      error = function(err){
        message(paste("Nu se poate calcula momentul initial de ordin", i))

      })
    }
    return(lista)
  },
  error = function(err){
    message(paste("Eroare:", err))
  })
}

#calcularea momentelor centrate
#' @export
momenteCentVA <- function(f)
{
  lista<- list()

  tryCatch({
    for(i in 1:4)
    {
      tryCatch({
        t<-medieVA(f)
        fcalcul<- function(x) { ((x-t)^i)*f(x)}
        result<-integrate(Vectorize(fcalcul), lower = -Inf, upper= Inf)$value
        ##print(result)
        lista<-append(lista, result)
      },
      error = function(err){
        message(paste("Nu se poate calcula momentul centrat de ordin", i))

      })
    }
    return(lista)
  },
  error = function(err){
    message(paste("Eroare:", err))
  })

}
#cerinta 6
#calcularea mediei g
#' @export
mediefuncVA <- function(f,g)
{
  tryCatch({
    fcalcul<- function(x) { g(x)*f(x)}
    result<-integrate(Vectorize(fcalcul), lower = -Inf, upper= Inf)$value
    return (result)
  },
  error = function(err){
    message(paste("Nu se poate calcula media cu functia g -> eroare:", err))
  })
}

#calcularea dispersiei g
#' @export
dispersiefuncVA <- function(f,g)
{
  tryCatch({
    fcalcul<- function(x) { ((g(x)-mediefuncVA(f,g))^2)*f(x)}
    result<-integrate(Vectorize(fcalcul), lower = -Inf, upper= Inf)$value
    return (result)
  },
  error = function(err){
    message(paste("Nu se poate calcula dispersia cu functia g-> eroare:", err))
  })

}

#cerinta 8
#' @export
fisa_sinteza <-function(val){

  #Afisarea unei fise  de  sintezs care sa contina informatii de baza despre respectiva
  #repartitie(cu precizarea sursei informatiei!). Relevant aici ar fi sa precizati pentru
  #ce e folosita in mod uzual acea repartitie, semnificatia parametrilor, media, dispersia etc.




  normala <- c(
    "    REPARTITIE NORMALA :       ",
    " -> Definitie : Spunem ca o variabila aleatoare X este repartizata normal sau Gaussian de medie ? s, i varianta ?? ^ 2, daca densitatea ei are forma f(x) = ??(x) = (1 / ???(2 * ?? * ??)) * (e ^ ((- (x ??? ?) ^ 2) / ((2 * ??) ^ 2)) , x ??? R.",
    " -> Notatie : X ??? N (?, ??2)",
    " -> Media : E[X] = ?",
    " -> Varianta : Var(X) = ??^2",
    " -> Sursa : https://moodle.unibuc.ro/pluginfile.php/121742/mod_resource/content/1/Lab6.pdf"

  )


  lognorm  <- c("    REPARTITIE LOGNORMALA :       ",
                " -> Definitie : Spune ca o variabila aleatoare X este repartizata log-normal de parametrii ? si ??^2, daca ln(X) este repartizata normal de parametrii ? si ??^2. Cu alte cuvinte daca Y ??? N (?, ??2) atunci X = e^Y ??? LN(?, ??^2). ",
                " -> Notatie : X ??? LN(?, ??^2)",
                " -> Media : E[X] = e ^(? + (??^2)/2)",
                " -> Varianta : Var(X) = (e ^ (??^2)??? 1) * (e ^ (2? + ??^2))",
                " -> Sursa : Curs: Probabilitat, i s, i Statistica (2020-2021), A. Amarioarei"
  )


  uniforma <- c("    REPARTITIE UNIFORMA :       ",
                " -> Definitie : O variabila aleatoare X repartizata uniform pe intervalul [a, b], are densitatea data de f(x) = { 1/(b ??? a), x ??? [a, b] si  0, altfel}",
                " -> Notatie : X ??? U[a, b]",
                " -> Media :  E[X] = (a + b) / 2",
                " -> Varianta : Var(X) = [(a ??? b) ^ 2] / 12 ",
                " -> Sursa : https://moodle.unibuc.ro/pluginfile.php/121742/mod_resource/content/1/Lab6.pdf"

  )


  beta <- c("    REPARTITIE BETA :       ",
            " -> Definitie : Spunem ca o variabila aleatoare X este repartizata Beta de parametrii (??, ??), cu ??, ?? > 0, daca densitatea ei are forma f(x) = (1/B(??, ??))* (x ^ (?????1))*(1 ??? x)^(?????1), 0 ??? x ??? 1, unde B(??, ??) este functia (Beta, numita s, i integrala Euler de primul tip) definita prin B(??, ??) = integrala de la 0 la infinit din ((x ^ (?????1)) *(1 ??? x)^(?????1) dx, ?????, ?? > 0.",
            " -> Notatie : X ??? B(??, ??)",
            " -> Media : E[X] = ??/(?? + ??)",
            " -> Varianta :  Var(X) = (?? * ??)/(((?? + ??) ^ 2) * (?? + ?? + 1))",
            " -> Sursa : https://moodle.unibuc.ro/pluginfile.php/121742/mod_resource/content/1/Lab6.pdf"

  )



  gama <- c(
    "    REPARTITIE GAMMA :       ",
    " -> Definitie : Spunem ca o variabila aleatoare X este repartizata Gama de parametrii (??, ??), cu ??, ?? > 0, daca densitatea ei are forma f(x) = ((?? ^ ??) / ??(??)) * (x ^(?????1)) * (e^ (?????x)), ???x > 0, unde ??(??) este funct, ia (Gama, numita s, i integrala Euler de al doilea tip) definita prin ??(??) = integrala de la 0 la infint din ((x ^ (?????1)) * (e ^(???x)) dx, ????? > 0.",
    " -> Notatie : X ??? ??(??, ??)",
    " -> Media :  E[X] = ?? / ??",
    " -> Varianta : Var(X) =  ?? / (?? ^ 2)",
    " -> Sursa : https://moodle.unibuc.ro/pluginfile.php/121742/mod_resource/content/1/Lab6.pdf"

  )


  exp <- c(
    "    REPARTITIE EXPONENTIALA :       ",
    " -> Definitie : Spunem ca o variabila aleatoare X este repartizata exponential de parametru ??, daca densitatea ei are forma f(x) = (??e) ^ (?????x), ???x ??? R.",
    " -> Notatie : X ??? E(??)",
    " -> Media : E[X] = 1/??",
    " -> Varianta : Var(X) = 1/(??^2)",
    " -> Sursa : https://moodle.unibuc.ro/pluginfile.php/121742/mod_resource/content/1/Lab6.pdf"

  )



  cauchy <- c(
    "    REPARTITIE CAUCHY :       ",
    " -> Definitie : Spunem ca o variabila aleatoare X este repartizata Cauchy de parametrii (0, 1), daca densitatea ei are forma  fX(x) = (1/??) * (1/(1 + x^2)), ???x ??? R.",
    " -> Notatie : X ??? C(0, 1)",
    " -> Media : NU EXISTA",
    " -> Varianta : NU EXISTA",
    " -> Sursa : https://moodle.unibuc.ro/pluginfile.php/121742/mod_resource/content/1/Lab6.pdf"

  )




  student <- c(
    "    REPARTITIE T/STUDENT :       ",
    " -> Definitie : Spunem ca o variabila aleatoare X este repartizata Student( are distributie Student) cu n grade de libertate daca poate fi scrisa sub forma X=Y/???(Z/n), unde Y ~ N(0,1) este o variabila normala standard iar Z ~ X^2(n) este o variabila aleatoare X^2 cu n grade de libertate independenta de Y.",
    " -> Notatie : X ~ T(n), n = nr de grade de libertate",
    " -> Media : E[X] = integrala de la -inf la + inf din x*f(x) dx = 0, n > 1",
    " -> Varianta : Var(X) = E[X^2]-E[X]^2 = n/(n-2)",
    " -> Sursa : http://cs.unitbv.ro/~pascu/stat/Distributii%20continue%20clasice.pdf"
  )

  x2 <- c(
    "    REPARTITIE X-PATRAT :       ",
    " -> Definitie : O variabila aleatoare X este distribuita X-patrat cu n grade de libertate daca are densitatea de probabilitate de forma: f(x) = (1/(2^(n/2) * gamma(n/2))) * x^((n/2)-1) * e^(-x/2), pentru x > 0",
    " -> Notatie : X ~ X^2(n), n = nr de grade de libertate",
    " -> Media : E[X] = integrala de la -inf la inf din x*f(x) dx = n",
    " -> Varianta : Var(X) = E[X^2]-E[X]^2 = 2n",
    " -> Sursa : http://math.etc.tuiasi.ro/rstrugariu/cursuri/SPD2015/c7.pdf"
  )


  if(val == "beta")

    print(noquote(beta), justify = "right")



  else if(val == "norm")

    print(noquote(normala), justify = "right")

  else if(val =="lognorm")
    print(noquote(lognorm), justify = "right")

  else if(val == "unif")

    print(noquote(uniforma), justify = "right")



  else if(val == "exp")

    print(noquote(exp), justify = "right")


  else if(val =="gama")

    print(noquote(gama), justify = "right")



  else if(val == "student")

    print(noquote(student), justify = "right")


  else if(val == "x2")

    print(noquote(x2), justify = "right")

  else if(val =="cauchy")
    print(noquote(cauchy), justify = "right")

  else if(val == "all")
  {

    print(noquote(normala))
    cat("\n\n")
    print(noquote(lognorm))
    cat("\n\n")
    print(noquote(uniforma))
    cat("\n\n")
    print(noquote(exp))
    cat("\n\n")
    print(noquote(beta))
    cat("\n\n")
    print(noquote(gama))
    cat("\n\n")
    print(noquote(student))
    cat("\n\n")
    print(noquote(x2))
    cat("\n\n")
    print(noquote(cauchy))

  }
  else
    print(noquote("Nu exista o astfel de repartitie!"))

}
#Problema 10
#Covarianta si coeficientul de corelatie

#functia va primi ca argumente densitatea comuna a variabilelor aleatoare continue X si Y si suportul functiei pentru x, respectiv pentru y
#initial, calculeaza densitatile marginale si mediile lui X si Y conform formulelor:
#fX(x) = integrala pe suportul lui y din fX,Y(x,y) dy, analog pentru fY(y)
#E[X] = integrala pe suportul lui X din x * f(x) dx, analog pentru E[Y]
#apoi construieste o functie noua necesara formulei de covarianta, adica (X-E[x])(Y-E[y])
#cov(X,y) = E[(X-E[x])(Y-E[y])]
#afla si variantele lui X si Y necesare coeficientului de corelatie
#Var(x) = E[(X-E[x])^2], analog pentru Var(Y)
#coeficientul de corelatie = cov(X,y) / sqrt (Var(X)) * sqrt(Var(Y))
#' @export
covarianta_coeficientCorelatie <- function(f, suportX, suportY){

  if(suportX[1] == -Inf || suportX[2] == Inf || suportY[1] == -Inf || suportY[2] == Inf){
    print("Imposibil de calculat: un capat al unui interval este -Inf sau Inf")
    return(FALSE)
  }

  densitateMarginalaX <- Vectorize(function(x){
    integrate(function(y){
      f(x,y)
    }, suportY[1], suportY[2])$value
  })

  medieX <- integrate(function(x){
    return (x * densitateMarginalaX(x))
  }, suportX[1], suportX[2])$value

  densitateMarginalaY <- Vectorize(function(y){
    integrate(function(x){
      f(x,y)
    }, suportX[1], suportX[2])$value
  })

  medieY <- integrate(function(y){
    return (y * densitateMarginalaY(y))
  }, suportY[1], suportY[2])$value

  functie_noua <- function(x,y){
    return ((x - medieX) * (y - medieY) * f(x, y))
  }

  covarianta <- integrate(Vectorize(function (y) {
    integrate(function (x) {
      functie_noua(x, y) }, suportX[1], suportX[2]) $ value
  }), suportY[1], suportY[2]) $ value

  variantaX <- integrate(
    function(x) {
      return ((x - medieX) ^ 2 * densitateMarginalaX(x))
    }, suportX[1], suportX[2]) $ value

  variantaY <- integrate(
    function(y) {
      return ((y - medieY) ^ 2 * densitateMarginalaY(y))
    }, suportY[1], suportY[2]) $ value

  coeficient_corelatie <- covarianta / sqrt(variantaX * variantaY)

  print(paste("Covarianta: ", covarianta))
  print(paste("Coeficient de corelatie: ", coeficient_corelatie))
}

# 12) Construirea sumei ??i diferen??ei a doua variabile aleatoare
# continue independente (folosi??i formula de convolu??ie)


##############################################################################


# f este o funct densitatea de probabilitate unei variabile aleatoare X
# g este o funct densitatea de probabilitate unei variabile aleatoare Y
# convolutionSum va returna o funct densitatea de probabilitate unei variabile
# Z = X + Y
#' @export
convolutionSum <- function(f, g) {
  function(z) (integrate (function(x) (f(x) * g(z - x)), -Inf, +Inf)$value)
}

# f este o funct densitatea de probabilitate unei variabile aleatoare X
# g este o funct densitatea de probabilitate unei variabile aleatoare Y
# convolutionDif va returna o funct densitatea de probabilitate unei variabile
# Z = X - Y
#' @export
convolutionDif <- function(f, g) {
  function(z) (integrate (function(x) (f(x) * g(x - z)), -Inf, +Inf)$value)
}
