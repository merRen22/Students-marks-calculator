install.packages("reshape")
install.packages("caret")

library(shiny)
library(reshape)
library(caret)
library(bnlearn)

shinyServer(function(input, output) {
  
  Grafo = empty.graph(c("sex","age","address","famsize","Medu","Fedu","reason","guardian","failures","romantic","freetime","Pstatus","Mjob","Fjob","traveltime","studytime","schoolsup","famsup","paid","activities","higher","internet","famrel","goout","Dalc","Walc","health","absences","G1","G2","G3"))
  
  #Opciones de cada variable
  Etiquetas = c("OP1","OP2")
  
  
  #matriz 1*1 (11)    
  
  #OP1 = F, OP2 = M
  MatrizSexo = matrix(c(0.524,0.476), ncol = 2,nrow = 1, dimnames = list("value","sex" = Etiquetas))
  #OP1 = MAS 18, OP2 = MENOR = 18
  MatrizEdad = matrix(c(0.221,0.779), ncol = 2,nrow = 1, dimnames = list("value","age" = Etiquetas))
  #OP1 = R, OP2 = U
  MatrizZona = matrix(c(0.181,0.819), ncol = 2,nrow = 1, dimnames = list("value","address" = Etiquetas))
  #OP1 = GT3, OP2 = LE3
  MatrizTamaniofamilia = matrix(c(0.721,0.279), ncol = 2,nrow = 1, dimnames = list("value","famsize" = Etiquetas))
  #OP1 = MAYOR3, OP2 = MENOR3
  Matrizeducacionmadres = matrix(c(0.596,0.404), ncol = 2,nrow = 1, dimnames = list("value","Medu" = Etiquetas))
  #OP1 = MAYOR3, OP2 = MENOR3
  Matrizeducacionpadres = matrix(c(0.507,0.493), ncol = 2,nrow = 1, dimnames = list("value","Fedu" = Etiquetas))
  #OP1 = R1, OP2 = R2
  MatrizRazonparaescogercolegio = matrix(c(0.567,0.433), ncol = 2,nrow = 1, dimnames = list("value","reason" = Etiquetas))
  #OP1 = PADRES, OP2 = OTHERS
  MatrizGuardinaestudiantil = matrix(c(0.924,0.076), ncol = 2,nrow = 1, dimnames = list("value","guardian" = Etiquetas))
  #OP1 = FALLA, OP2 = NOFALLA
  MatrizCursosjalados = matrix(c(0.199,0.801), ncol = 2,nrow = 1, dimnames = list("value","failures" = Etiquetas))
  #OP1 = NO, OP2 = YES
  MatrizRomance = matrix(c(0.676,0.324), ncol = 2,nrow = 1, dimnames = list("value","romantic" = Etiquetas))
  #OP1 = FT1, OP2 = FT2
  MatrizTiempolibre = matrix(c(0.616,0.384), ncol = 2,nrow = 1, dimnames = list("value","freetime" = Etiquetas))
  
  #matriz 2*2 (6)
  MatriztrabajoMama = matrix(c(0.328,0.672,0.729,0.271), ncol = 2,nrow = 2, dimnames = list("Mjob"=Etiquetas,"Medu"= Etiquetas))
  MatriztrabajoPapa = matrix(c(0.492,0.508,0.725,0.275), ncol = 2,nrow = 2, dimnames = list("Fjob"=Etiquetas,"Fedu"= Etiquetas))
  MatrizApoyoestudiantilextra = matrix(c(0.893,0.107,0.829,0.171), ncol = 2,nrow = 2, dimnames = list("schoolsup"=Etiquetas,"famsup"= Etiquetas))
  MatrizActividadextra = matrix(c(0.477,0.523,0.433,0.567), ncol = 2,nrow = 2, dimnames = list("activities"=Etiquetas,"schoolsup"= Etiquetas))
  MatrizClasespagasextra = matrix(c(0.533,0.467,0.567,0.433), ncol = 2,nrow = 2, dimnames = list("paid"=Etiquetas,"schoolsup"= Etiquetas))
  MatrizTiempodeviaje  = matrix(c(0.836,0.164,0.956,0.044), ncol = 2,nrow = 2, dimnames = list("traveltime"=Etiquetas,"address"= Etiquetas))
  
  #matriz 2*2*2 (6)    
  valuesConsumodealcholenfindesemana = c(0.964,0.036,0.635,0.365,0.892,0.108,0.676,0.324)
  dim(valuesConsumodealcholenfindesemana) = c(2,2,2)
  dimnames(valuesConsumodealcholenfindesemana) = list("Walc"=Etiquetas, "sex"=Etiquetas, "age"=Etiquetas)
  
  valuesConsumodealcholensemana = c(0.988,0.012,0.983,0.017,0.905,0.095,0.927,0.073)
  dim(valuesConsumodealcholensemana) = c(2,2,2)
  dimnames(valuesConsumodealcholensemana) = list("Dalc"=Etiquetas, "sex"=Etiquetas, "age"=Etiquetas)
  
  valuesconvivenciaconpadres = c(0.087,0.913,0.165,0.835,0.183,0.817,0.077,0.923)
  dim(valuesconvivenciaconpadres) = c(2,2,2)
  dimnames(valuesconvivenciaconpadres) = list("Pstatus"=Etiquetas, "Mjob"=Etiquetas, "Fjob"=Etiquetas)
  
  valuesAusencias = c(0.384,0.616,0.391,0.609,0.583,0.417,0.650,0.350)
  dim(valuesAusencias) = c(2,2,2)
  dimnames(valuesAusencias) = list("absences"=Etiquetas, "famsup"=Etiquetas, "Dalc"=Etiquetas)
  
  valuesInternet = c(0.269,0.731,0.591,0.409,0.115,0.885,0.115,0.885)
  dim(valuesInternet) = c(2,2,2)
  dimnames(valuesInternet) = list("internet"=Etiquetas, "traveltime"=Etiquetas, "address"=Etiquetas)
  
  valuesG3 = c(0.966,0.034,0.881,0.119,0.342,0.658,0.070,0.930)
  dim(valuesG3) = c(2,2,2)
  dimnames(valuesG3) = list("G3"=Etiquetas, "G1"=Etiquetas, "G2"=Etiquetas)
  
  #matriz 2*2*2*2 (7)        
  valuesCalidadderelacionfamiliar = c(0.259,0.741,0.50,0.50,0.278,0.722,0.50,0.50,0.209,0.791,0.50,0.50,0.350,0.650,0.115,0.885)
  dim(valuesCalidadderelacionfamiliar) = c(2,2,2,2)
  dimnames(valuesCalidadderelacionfamiliar) = list("famrel"=Etiquetas, "Dalc"=Etiquetas, "Walc"=Etiquetas, "Pstatus"=Etiquetas)
  
  valuesEstadodesaludactual = c(0.419,0.581,0.553,0.447,0.477,0.523,0.582,0.418,0.50,0.50,0.550,0.450,0.50,0.50,0.875,0.125)
  dim(valuesEstadodesaludactual) = c(2,2,2,2)
  dimnames(valuesEstadodesaludactual) = list("health"=Etiquetas, "famrel"=Etiquetas,"absences"=Etiquetas, "Dalc"=Etiquetas)
  
  valuesG1 = c(0.10,0.90,0.621,0.379,0.5,0.5,0.569,0.431,0.071,0.929,0.359,0.641,0.125,0.875,0.50,0.50)
  dim(valuesG1) = c(2,2,2,2)
  dimnames(valuesG1) = list("G1"=Etiquetas, "higher"=Etiquetas, "health"=Etiquetas, "goout"=Etiquetas)
  
  valuesSalirconamigos = c(0.744,0.256,0.537,0.463,0.583,0.417,0.439,0.561,0.805,0.195,0.458,0.542,0.812,0.188,0.464,0.536)
  dim(valuesSalirconamigos) = c(2,2,2,2)
  dimnames(valuesSalirconamigos) = list("goout"=Etiquetas, "freetime"=Etiquetas, "activities"=Etiquetas, "absences"=Etiquetas)
  
  valuesApoyoeduaciondefamilia = c(0.361,0.639,0.50,0.50,0.417,0.583,0.50,0.50,0.331,0.669,0.324,0.676,0.433,0.567,0.357,0.643)
  dim(valuesApoyoeduaciondefamilia) = c(2,2,2,2)
  dimnames(valuesApoyoeduaciondefamilia) = list("famsup"=Etiquetas,"guardian"=Etiquetas, "famsize"=Etiquetas, "Pstatus"=Etiquetas )
  
  valuesTiempodeestudio = c(0.786,0.214,0.786,0.214,0.819,0.181,0.917,0.083,0.922,0.078,0.759,0.241,0.73,0.27,0.682,0.318)
  dim(valuesTiempodeestudio) = c(2,2,2,2)
  dimnames(valuesTiempodeestudio) = list("studytime"=Etiquetas, "romantic"=Etiquetas, "failures"=Etiquetas, "internet"=Etiquetas)
  
  valuesContinuareduacionsuperior = c(0.073,0.927,0.024,0.976,0.007,0.993,0.016,0.984,0.160,0.840,0.028,0.972,0.012,0.988,0.028,0.972)
  dim(valuesContinuareduacionsuperior) = c(2,2,2,2)
  dimnames(valuesContinuareduacionsuperior) = list("higher"=Etiquetas, "studytime"=Etiquetas, "paid"=Etiquetas, "reason"=Etiquetas)
  
  #matriz 2*2*2*2*2 (1)    
  valuesG2 = c(0.50,0.50,0.10,0.90,0.50,0.50,0.071,0.929,0.833,0.167,0.167,0.833,0.5,0.5,0.125,0.875,0.936,0.064,0.244,0.756,0.865,0.135,0.12,0.88,0.863,0.137,0.074,0.926,0.848,0.152,0.109,0.891)
  dim(valuesG2) = c(2,2,2,2,2)
  dimnames(valuesG2) = list("G2"=Etiquetas, "G1"=Etiquetas, "goout"=Etiquetas, "health"=Etiquetas, "higher"=Etiquetas)
  
  
  #GRAFO
  matrizU1 =  matrix(0,ncol = 31, nrow = 31, dimnames = list(c("sex","age","address","famsize","Medu","Fedu","reason","guardian","failures","romantic","freetime","Pstatus","Mjob","Fjob","traveltime","studytime","schoolsup","famsup","paid","activities","higher","internet","famrel","goout","Dalc","Walc","health","absences","G1","G2","G3"),c("sex","age","address","famsize","Medu","Fedu","reason","guardian","failures","romantic","freetime","Pstatus","Mjob","Fjob","traveltime","studytime","schoolsup","famsup","paid","activities","higher","internet","famrel","goout","Dalc","Walc","health","absences","G1","G2","G3")))
  matrizU1["sex","Walc"]=1
  matrizU1["sex","Dalc"]=1
  matrizU1["age","Walc"]=1
  matrizU1["age","Dalc"]=1
  matrizU1["address","traveltime"]=1
  matrizU1["address","internet"]=1
  matrizU1["famsize","famsup"]=1
  matrizU1["Medu","Mjob"]=1
  matrizU1["Fedu","Fjob"]=1
  matrizU1["reason","higher"]=1
  matrizU1["guardian","famsup"]=1
  matrizU1["failures","studytime"]=1
  matrizU1["romantic","studytime"]=1
  matrizU1["freetime","goout"]=1
  matrizU1["Walc","famrel"]=1
  matrizU1["traveltime","internet"]=1
  matrizU1["internet","studytime"]=1
  matrizU1["studytime","higher"]=1
  matrizU1["Mjob","Pstatus"]=1
  matrizU1["Fjob","Pstatus"]=1
  matrizU1["Dalc","famrel"]=1
  matrizU1["Dalc","health"]=1
  matrizU1["Dalc","absences"]=1
  matrizU1["Pstatus","famrel"]=1
  matrizU1["Pstatus","famsup"]=1
  matrizU1["famsup","absences"]=1
  matrizU1["famsup","schoolsup"]=1
  matrizU1["famrel","health"]=1
  matrizU1["absences","health"]=1
  matrizU1["absences","goout"]=1
  matrizU1["schoolsup","activities"]=1
  matrizU1["schoolsup","paid"]=1
  matrizU1["activities","goout"]=1
  matrizU1["paid","higher"]=1
  matrizU1["higher","G1"]=1
  matrizU1["higher","G2"]=1
  matrizU1["goout","G1"]=1
  matrizU1["goout","G2"]=1
  matrizU1["health","G1"]=1
  matrizU1["health","G2"]=1
  matrizU1["G1","G3"]=1
  matrizU1["G2","G3"]=1
  matrizU1["G1","G2"]=1
  
  amat(Grafo) = matrizU1
  Modelo = custom.fit(Grafo,dist = list(sex=MatrizSexo,age=MatrizEdad,address=MatrizZona,famsize=MatrizTamaniofamilia,Medu=Matrizeducacionmadres,Fedu=Matrizeducacionpadres,reason=MatrizRazonparaescogercolegio,guardian=MatrizGuardinaestudiantil,failures=MatrizCursosjalados,romantic=MatrizRomance,freetime=MatrizTiempolibre,Pstatus=valuesconvivenciaconpadres,Mjob=MatriztrabajoMama,Fjob=MatriztrabajoPapa,traveltime=MatrizTiempodeviaje,studytime=valuesTiempodeestudio,schoolsup=MatrizApoyoestudiantilextra,famsup=valuesApoyoeduaciondefamilia,paid=MatrizClasespagasextra,activities=MatrizActividadextra,higher=valuesContinuareduacionsuperior,internet=valuesInternet,famrel=valuesCalidadderelacionfamiliar,goout=valuesSalirconamigos,Dalc=valuesConsumodealcholensemana,Walc=valuesConsumodealcholenfindesemana,health=valuesEstadodesaludactual,absences=valuesAusencias,G1=valuesG1,G2=valuesG2,G3=valuesG3))
  probG3alumno1 = cpquery(Modelo,G3=="OP2", (sex =="OP2" & age =="OP2" & address=="OP2"&famsize=="OP2" & Medu=="OP1" & Fedu=="OP2"&reason=="OP2" & guardian=="OP1" & failures=="OP2"&romantic =="OP1" & freetime =="OP2"), n=1000000)
  probG3alumno2 = cpquery(Modelo,G3=="OP1", (sex =="OP1" & age =="OP1" & address=="OP2"&famsize=="OP1" & Medu=="OP1" & Fedu=="OP1"&reason=="OP2" & guardian=="OP1" & failures=="OP2"&romantic =="OP1" & freetime =="OP1"), n=1000000)
  
  
  probG3alumno1 = cpquery(Modelo,G3=="OP1", (sex =="OP1" & age =="OP2" & address=="OP2"&famsize=="OP1" & Medu=="OP1" & Fedu=="OP1"&reason=="OP1" & guardian=="OP1" & failures=="OP2"&romantic =="OP1" & freetime =="OP1"), n=1000000)
  probG3alumno2 = cpquery(Modelo,G3=="OP1", (sex =="OP1" & age =="OP2" & address=="OP2"&famsize=="OP1" & Medu=="OP1" & Fedu=="OP1"&reason=="OP1" & guardian=="OP1" & failures=="OP2"&romantic =="OP1" & freetime =="OP1"), n=1000000)
  probG3alumno3 = cpquery(Modelo,G3=="OP1", (sex =="OP1" & age =="OP2" & address=="OP2"&famsize=="OP1" & Medu=="OP1" & Fedu=="OP1"&reason=="OP1" & guardian=="OP1" & failures=="OP2"&romantic =="OP1" & freetime =="OP1"), n=1000000)
  probG3alumno4 = cpquery(Modelo,G3=="OP1", (sex =="OP2" & age =="OP2" & address=="OP2"&famsize=="OP1" & Medu=="OP1" & Fedu=="OP2"&reason=="OP2" & guardian=="OP1" & failures=="OP1"&romantic =="OP1" & freetime =="OP2"), n=1000000)
  probG3alumno5 = cpquery(Modelo,G3=="OP1", (sex =="OP2" & age =="OP2" & address=="OP2"&famsize=="OP2" & Medu=="OP1" & Fedu=="OP1"&reason=="OP1" & guardian=="OP1" & failures=="OP2"&romantic =="OP1" & freetime =="OP1"), n=1000000)
  probG3alumno6 = cpquery(Modelo,G3=="OP1", (sex =="OP1" & age =="OP2" & address=="OP1"&famsize=="OP1" & Medu=="OP2" & Fedu=="OP1"&reason=="OP2" & guardian=="OP1" & failures=="OP2"&romantic =="OP1" & freetime =="OP1"), n=1000000)
  probG3alumno7 = cpquery(Modelo,G3=="OP1", (sex =="OP1" & age =="OP2" & address=="OP2"&famsize=="OP1" & Medu=="OP2" & Fedu=="OP2"&reason=="OP1" & guardian=="OP1" & failures=="OP1"&romantic =="OP1" & freetime =="OP1"), n=1000000)
  probG3alumno8 = cpquery(Modelo,G3=="OP1", (sex =="OP1" & age =="OP2" & address=="OP2"&famsize=="OP1" & Medu=="OP2" & Fedu=="OP1"&reason=="OP2" & guardian=="OP1" & failures=="OP2"&romantic =="OP1" & freetime =="OP2"), n=1000000)
  probG3alumno9 = cpquery(Modelo,G3=="OP1", (sex =="OP1" & age =="OP2" & address=="OP2"&famsize=="OP1" & Medu=="OP2" & Fedu=="OP2"&reason=="OP2" & guardian=="OP1" & failures=="OP1"&romantic =="OP1" & freetime =="OP1"), n=1000000)
  probG3alumno10 = cpquery(Modelo,G3=="OP1", (sex =="OP1" & age =="OP2" & address=="OP2"&famsize=="OP1" & Medu=="OP1" & Fedu=="OP1"&reason=="OP2" & guardian=="OP1" & failures=="OP2"&romantic =="OP2" & freetime =="OP1"), n=1000000)
  
  
  
  #Evento btn
  observeEvent(input$go,{
    
    #MOdelo y grafo
    Modelo
    Grafo
    plot(Grafo)
    
    if(input$sex == "OP1"){
    variableS = "OP1"
    }else{
      variableS = "OP2"
      }
    #varS = (sex == "OP1")
    
    
    variableAge = input$age
    variableAddress = input$address
    variableFailures = input$failures
    variableDFamsize = input$famsize
    variableRomantic = input$romantic
    variableMedu = input$Medu
    variableFedu = input$Fedu
    variableGuardian = input$guardian
    variableReason = input$reason
    variableFreetime = input$freetime
    variableGuardian = input$guardian

    
    
    #consulta
    #MatrizSexo
    #OP1 = F, OP2 = M
    
    #MatrizEdad
    #OP1 = MAS 18, OP2 = MENOR = 18
    
    #MatrizZona
    #OP1 = R, OP2 = U
    
    #MatrizTamaniofamilia
    #OP1 = GT3, OP2 = LE3
    
    ##Matrizeducacionmadres
    #OP1 = MAYOR3, OP2 = MENOR3
    
    #Matrizeducacionpadres
    #OP1 = MAYOR3, OP2 = MENOR3
    
    #MatrizRazonparaescogercolegio
    #OP1 = R1, OP2 = R2
    
    #MatrizGuardinaestudiantil
    #OP1 = PADRES, OP2 = OTHERS
    
    #MatrizCursosjalados
    #OP1 = FALLA, OP2 = NOFALLA
    
    #MatrizRomance
    #OP1 = NO, OP2 = YES
    
    #MatrizTiempolibre 
    #OP1 = FT1, OP2 = FT2


    
      #Porbabilidad de aporebar
    probAlumnoG3 = cpquery(Modelo,G3=="OP1", (sex =="OP2" & age =="OP1" & address=="OP1"&famsize=="OP2" & Medu=="OP2" & Fedu=="OP2"&reason=="OP2" & guardian=="OP1" & failures=="OP1"&romantic =="OP2" & freetime =="OP1"), n=10000000)
    probAlumnoG1 = cpquery(Modelo,G1=="OP1", (sex =="OP2" & age =="OP1" & address=="OP1"&famsize=="OP2" & Medu=="OP2" & Fedu=="OP2"&reason=="OP2" & guardian=="OP1" & failures=="OP1"&romantic =="OP2" & freetime =="OP1"), n=10000000)
    probAlumnoG2 = cpquery(Modelo,G2=="OP1", (sex =="OP2" & age =="OP1" & address=="OP1"&famsize=="OP2" & Medu=="OP2" & Fedu=="OP2"&reason=="OP2" & guardian=="OP1" & failures=="OP1"&romantic =="OP2" & freetime =="OP1"), n=10000000)
      
      
      
      
    output$selected_var1 <- renderText({paste("Tus probabilidades de aprobar este anio escolar (G3) son de  : ", probAlumnoG3)})
    output$selected_var2 <- renderText({paste("Tus probabilidades de aprobar este anio escolar (G1) son de  : ", probAlumnoG1)})
    output$selected_var3 <- renderText({paste("Tus probabilidades de aprobar este anio escolar (G2) son de  : ", probAlumnoG2)})
  })
  
})