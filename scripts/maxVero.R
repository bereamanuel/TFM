estimacionMV <- function(datos){
  ##La siguiente función nos devuelve mu y sigma obtenidos por el método de máxima verosimilitud,
  ##Este método se basa en maximizar la función de verosimilitud.
  ##Input:
  ##    - datos(DataFrame)      : Datos a analizar (t,Xt)
  ##Output:
  ##    - List[Number]          : Lista con los parámetros que nos devuelve el modelo.


n <- dim(datos)[1]

dt <- datos$t[2]- datos$t[1]

muE <- (1/(n*dt))*sum((datos$value/lag(datos$value)) -1, na.rm = T)
siE <- sqrt((1/(n*dt))*sum(((datos$value/lag(datos$value)) -1- muE*dt)**2 , na.rm = T))
list(muE = muE, siE = siE)
}


media <- function(datos,mu){
  ##La siguiente función nos devuelve el media del modelo Log-Normal en el instate t:
  ##      Input:
  ##          - datos(List[Number])   : Datos a analizar.
  ##          - mu(Number)            : Media muestral.
  ##          - t (Number)            : Vector de tiempos o el instante t. (Valor opcional, si no se rellena, devuelve en cada instante de t)
  ##      Output:
  ##          - eS(List[Number])      : Vector de medias de la solución en todos los instantes o media de la solución en el instante t.


s0 <- datos$value[1]
eS <- rep(0,dim(datos)[1])


for(i in seq(1,dim(datos)[1])){
  eS[i] <- s0*exp(mu*(i-1))
}
eS
}

desvTipica <- function(datos ,mu, sigma, t = None){

  ## La siguiente función nos devuelve la desviación típica del modelo Log-Normal en el instate t:
  ## Input:
  ##     - datos(List[Number])   : Datos a analizar.
  ##     - mu(Number)            : Media muestral.
  ##     - sigma(Number)         : Desviación típica muestral.
  ##     - t (Number)            : Vector de tiempos o el instante t. (Valor opcional, si no se rellena, devuelve en cada instante de t)
  ## Output:
  ##     - eS(List[Number])       : Vector desviación típica la solución en todos los instantes o desviación típica de la solución en el instante t.


s0 <- datos$value[1]
eS <- rep(0,dim(datos)[1])

for(i in seq(1,dim(datos)[1])){
  eS[i] <- sqrt((s0^2)*exp(2*mu*(i-1))*(exp((i-1)*sigma^2)-1) )
}

eS
}

validacion <- function(est,datos){
    ##La siguiente función nos devuleve los errores cometidos al utilizar cierto estimador. Se complementa con la función error.
    ##Input:
    ##    - estimacion(Dict[Number]) : Diccionario que contiene la media y la sigma estimada con cierta función de estimación.
    ##    - datos(List[Number])   : Datos a analizar.
    ##Output:
    ##  - Dict                  : Diccionario que nos devuelve los valores de error.
    
  mu <- est$muE
  sigma <- est$siE
  
  mm <- media(datos,mu)
  dsm <- desvTipica(datos,mu,sigma)
  list(mm = mm, dsm = dsm)
}



  
