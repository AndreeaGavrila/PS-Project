
#APELURI FUNCTII
ls.str()
rm(list = ls())


#EX1
constantaNorm(function(x) {exp(-(x^2)/2)})
constantaNorm(function(x) {exp(x)/((1+exp(x))^2)})


#EX2

verifica_functie(function(x) 3*x^2, list(c(0, 1))) #True
verifica_functie(function(x) x, list(c(-1, 0), c(1, 3))) #valoare negativa in [-1.00, 0.00]
verifica_functie(function(x) x, list(c(-Inf, 0)))  # integrala divergenta in [0.00, Inf]



#EX3

# Teste
X <- myclass(density = function(x,y) exp(-(x^2)*y/2),
             bidimen = TRUE,
             base = list(list(c(-1, 1)), list(c(2, 3))))
afisare(X)

Y <- myclass(density = function (x) 1/((x+1)*sqrt(x)),
             bidimen = FALSE,
             base = list(c(0, 1)))
afisare(Y)

Z <- myclass(density = function(x,y) sqrt(exp(-(x^2)*y/2)),
             bidimen = TRUE,
             base = list(list(c(0, 2)), list(c(0, 2))))
afisare(Z)


#EX4



#REPARTITIA NORMALA
densitate_n(0,1)
repartitie_n(0,1)

#REPARTITIA UNIFORMA
densitate_u(1, 4)
repartitie_u(1, 4)

#REPARTITIA EXPONENTIALA
densitate_e(5)
repartite_e(5)

#REPARTITIA GAMMA
densitate_g(0.5, 0.5)
repartitie_g(1, 0.5)

#REPARTITIA BETA
densitate_b(0.5, 2)
repartitie_b(1, 5)

#REPARTITIA CAUCHY
densitate_c(0, 0.5)
repartitie_c(3, 0.5)




#EX5

medieVA(function(x) {exp(-(x^2)/2)})
medieVA(function(x) {exp(x)/((1+exp(x))^2)})
momenteInitVA(function(x) {exp(-(x^2)/2)})
momenteCentVA(function(x) {exp(x)/((1+exp(x))^2)})
momenteCentVA(function(x) {exp(-(x^2)/2)})

#Ex6

mediefuncVA(function(x) {exp(-(x^2)/2)},function(x) {exp(-(x^2)/2)})
dispersiefuncVA(function(x) {exp(-(x^2)/2)},function(x) {exp(-(x^2)/2)})



#EX8

fisa_sinteza("all")
fisa_sinteza("x2")
fisa_sinteza("unif")


#EX10

covarianta_coeficientCorelatie(function(x,y){3/2*(x^2+y^2)}, c(0,1), c(0,1))
#Covarianta: -0.015625 si Coeficientul de corelatie: -0.2054794...
covarianta_coeficientCorelatie(function(x,y){x+y+1}, c(1,2), c(5,7))
#Covarianta: 39681.666... si coeficientul de corelatie: 0.9999107...


#EX12

# densitatea unei variabile aleatoare repartizate normal
# primesc ca parametrii media si abaterea standard
f.X <- function(X) dnorm(X,1,0.5)        # normal (mu=1, sigma=0.5)
f.Y <- function(Y) dlnorm(Y,1.5, 0.75)   # log-normal (mu=1.5, sigma=0.75)


# Construirea sumei si diferentei (folositi formula de convolutie)
f.Z <- convolutionSum(f.X, f.Y)
f.Z <- Vectorize(f.Z)   # este necesar sa Vectorizez rezultatul


f.Q <- convolutionSum(f.X, f.Y)
f.Q <- Vectorize(f.Q)

set.seed(1) # pentru a crea obiecte aleatorii pentru a reproduce exemplele

# repartitia normala
X <- rnorm(1000,1,0.5)
# generam 1000 observatii independente din repartitia normala
# primesc ca parametrii media si abaterea standard


# repartitia log-normala
Y <- rlnorm(1000,1.5,0.75)
# generam 1000 observatii independente din repartitia log-normala
# primesc ca parametrii media si abaterea standard

Z <- X + Y
Q <- X - Y

# compar metodele folosite:

# am generat 1000 de observatii din repartitia data
# si am trasat histograma acestora si am suprapus
# densitatea repartitiei date

hist(Z,freq=F,breaks=50, xlim=c(0,30))
z <- seq(0,50,0.01)
lines(z,f.Z(z),lty=2,col="red")


hist(Q,freq=F,breaks=50, xlim=c(0,30))
q <- seq(0,50,0.01)
lines(q,f.Q(q),lty=2,col="blue")













