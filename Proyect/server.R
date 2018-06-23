#install.packages("reshape")
#install.packages("caret")

library(shiny)
library(reshape)
library(caret)
library(bnlearn)

shinyServer(function(input, output) {
  
  Grafo = empty.graph(c("sex","age","address","famsize","Medu","Fedu","reason","guardian","failures","romantic","freetime","Pstatus","Mjob","Fjob","traveltime","studytime","schoolsup","famsup","paid","activities","higher","internet","famrel","goout","Dalc","Walc","health","absences","G1","G2","G3"))

  #Opciones de cada variable
  Etiquetas = c("OP1","OP2")
  

    #matriz 1*1 (11)    
    MatrizSexo = matrix(c(0.50,0.50), ncol = 2,nrow = 1, dimnames = list("value","sex" = Etiquetas))
    MatrizEdad = matrix(c(0.50,0.50), ncol = 2,nrow = 1, dimnames = list("value","age" = Etiquetas))
    MatrizZona = matrix(c(0.50,0.50), ncol = 2,nrow = 1, dimnames = list("value","address" = Etiquetas))
    MatrizTamaniofamilia = matrix(c(0.50,0.50), ncol = 2,nrow = 1, dimnames = list("value","famsize" = Etiquetas))
    Matrizeducacionmadres = matrix(c(0.50,0.50), ncol = 2,nrow = 1, dimnames = list("value","Medu" = Etiquetas))
    Matrizeducacionpadres = matrix(c(0.50,0.50), ncol = 2,nrow = 1, dimnames = list("value","Fedu" = Etiquetas))
    MatrizRazonparaescogercolegio = matrix(c(0.50,0.50), ncol = 2,nrow = 1, dimnames = list("value","reason" = Etiquetas))
    MatrizGuardinaestudiantil = matrix(c(0.50,0.50), ncol = 2,nrow = 1, dimnames = list("value","guardian" = Etiquetas))
    MatrizCursosjalados = matrix(c(0.50,0.50), ncol = 2,nrow = 1, dimnames = list("value","failures" = Etiquetas))
    MatrizRomance = matrix(c(0.50,0.50), ncol = 2,nrow = 1, dimnames = list("value","romantic" = Etiquetas))
    MatrizTiempolibre = matrix(c(0.50,0.50), ncol = 2,nrow = 1, dimnames = list("value","freetime" = Etiquetas))
    
    #matriz 2*2 (6)
    MatriztrabajoMama = matrix(c(0.50,0.50,0.50,0.50), ncol = 2,nrow = 2, dimnames = list("Mjob"=Etiquetas,"Medu"= Etiquetas))
    MatriztrabajoPapa = matrix(c(0.50,0.50,0.50,0.50), ncol = 2,nrow = 2, dimnames = list("Fjob"=Etiquetas,"Fedu"= Etiquetas))
    MatrizApoyoestudiantilextra = matrix(c(0.50,0.50,0.50,0.50), ncol = 2,nrow = 2, dimnames = list("schoolsup"=Etiquetas,"famsup"= Etiquetas))
    MatrizActividadextra = matrix(c(0.50,0.50,0.50,0.50), ncol = 2,nrow = 2, dimnames = list("activities"=Etiquetas,"schoolsup"= Etiquetas))
    MatrizClasespagasextra = matrix(c(0.50,0.50,0.50,0.50), ncol = 2,nrow = 2, dimnames = list("paid"=Etiquetas,"schoolsup"= Etiquetas))
    MatrizTiempodeviaje  = matrix(c(0.50,0.50,0.50,0.50), ncol = 2,nrow = 2, dimnames = list("traveltime"=Etiquetas,"address"= Etiquetas))
    
    #matriz 2*2*2 (6)    
    valuesConsumodealcholenfindesemana = c(0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50)
    dim(valuesConsumodealcholenfindesemana) = c(2,2,2)
    dimnames(valuesConsumodealcholenfindesemana) = list("Walc"=Etiquetas, "sex"=Etiquetas, "age"=Etiquetas)
    
    valuesConsumodealcholensemana = c(0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50)
    dim(valuesConsumodealcholensemana) = c(2,2,2)
    dimnames(valuesConsumodealcholensemana) = list("Dalc"=Etiquetas, "sex"=Etiquetas, "age"=Etiquetas)
    
    valuesconvivenciaconpadres = c(0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50)
    dim(valuesconvivenciaconpadres) = c(2,2,2)
    dimnames(valuesconvivenciaconpadres) = list("Pstatus"=Etiquetas, "Mjob"=Etiquetas, "Fjob"=Etiquetas)
    
    valuesAusencias = c(0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50)
    dim(valuesAusencias) = c(2,2,2)
    dimnames(valuesAusencias) = list("absences"=Etiquetas, "Dalc"=Etiquetas, "famsup"=Etiquetas)
    
    valuesInternet = c(0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50)
    dim(valuesInternet) = c(2,2,2)
    dimnames(valuesInternet) = list("internet"=Etiquetas, "traveltime"=Etiquetas, "address"=Etiquetas)
    
    valuesG3 = c(0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50)
    dim(valuesG3) = c(2,2,2)
    dimnames(valuesG3) = list("G3"=Etiquetas, "G1"=Etiquetas, "G2"=Etiquetas)
    
    #matriz 2*2*2*2 (7)        
    valuesCalidadderelacionfamiliar = c(0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50)
    dim(valuesCalidadderelacionfamiliar) = c(2,2,2,2)
    dimnames(valuesCalidadderelacionfamiliar) = list("famrel"=Etiquetas, "Walc"=Etiquetas, "Dalc"=Etiquetas, "Pstatus"=Etiquetas)
    
    valuesEstadodesaludactual = c(0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50)
    dim(valuesEstadodesaludactual) = c(2,2,2,2)
    dimnames(valuesEstadodesaludactual) = list("health"=Etiquetas, "famrel"=Etiquetas, "Dalc"=Etiquetas, "absences"=Etiquetas)
    
    valuesG1 = c(0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50)
    dim(valuesG1) = c(2,2,2,2)
    dimnames(valuesG1) = list("G1"=Etiquetas, "health"=Etiquetas, "goout"=Etiquetas, "higher"=Etiquetas)
    
    valuesSalirconamigos = c(0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50)
    dim(valuesSalirconamigos) = c(2,2,2,2)
    dimnames(valuesSalirconamigos) = list("goout"=Etiquetas, "absences"=Etiquetas, "activities"=Etiquetas, "freetime"=Etiquetas)
    
    valuesApoyoeduaciondefamilia = c(0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50)
    dim(valuesApoyoeduaciondefamilia) = c(2,2,2,2)
    dimnames(valuesApoyoeduaciondefamilia) = list("famsup"=Etiquetas, "Pstatus"=Etiquetas, "famsize"=Etiquetas, "guardian"=Etiquetas)
    
    valuesTiempodeestudio = c(0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50)
    dim(valuesTiempodeestudio) = c(2,2,2,2)
    dimnames(valuesTiempodeestudio) = list("studytime"=Etiquetas, "internet"=Etiquetas, "failures"=Etiquetas, "romantic"=Etiquetas)
    
    valuesContinuareduacionsuperior = c(0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50)
    dim(valuesContinuareduacionsuperior) = c(2,2,2,2)
    dimnames(valuesContinuareduacionsuperior) = list("higher"=Etiquetas, "paid"=Etiquetas, "studytime"=Etiquetas, "reason"=Etiquetas)
    
    #matriz 2*2*2*2*2 (1)    
    valuesG2 = c(0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50)
    dim(valuesG2) = c(2,2,2,2,2)
    dimnames(valuesG2) = list("G2"=Etiquetas, "G1"=Etiquetas, "health"=Etiquetas, "goout"=Etiquetas, "higher"=Etiquetas)
    
  
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
      #Porbabilidad de aporebar
      probAlumno = cpquery(Modelo,G3=="OP1", (sex =="OP1" & age =="OP1" & address=="OP1"  & famsize=="OP1" & Medu=="OP1" & Fedu=="OP1" & reason=="OP1" & guardian=="OP1" & failures=="OP1" & romantic =="OP1" & freetime =="OP1"), n=10000000)
      #probAlumno = cpquery(Modelo, event = (G3=="OP1"), evidence = list(sex ="OP1" , age ="OP1" , address="OP1"  , famsize="OP1" , Medu="OP1" , Fedu="OP1" , reason="OP1" , guardian="OP1" , failures="OP1" , romantic ="OP1" , freetime ="OP1"), n=10000000)
      #probAlumno = cpquery(Modelo,G3=="OP1", (c(varS) & age ==variableAge & address==variableAddress  & famsize==variableDFamsize & Medu==variableMedu & Fedu==variableFedu & reason==variableReason & guardian==variableGuardian & failures==variableFailures & romantic ==variableRomantic & freetime ==variableFreetime), n=10000000)
      
    output$selected_var <- renderText({paste("Tus probabilidades de aprobar son  ", probAlumno)})
  })
  
})