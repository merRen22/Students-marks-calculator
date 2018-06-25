library(shiny)

# Elemento de la interfaz
shinyUI(pageWithSidebar(
  
  # Titulo
  headerPanel("Desempeño escolar con teorema de Bayes"),
  
  sidebarPanel(    
    #1
    selectInput("sex", "Sexo:",
                list("Mujer" = "OP1",   #F 
                     "Hombre" = "OP2"   #M
                )),
    
    #2
    selectInput("age", "Edad:",
                list("Mayor de 18" = "OP1", #"mature", 
                     "Menor de 18" = "OP2"  #"notMature"
                )),
    
    #3
    selectInput("address ", "Zona",
                list("urbano" = "OP1", #"1", 
                     "rural" = "OP2"  #"0"
                )),
    
    #4
    selectInput("famsize", "Tamaño familia",
                list("3 o mas personas " = "OP1", #"GT3", 
                     "menor o igual que 3" = "OP2"  #"LE3"
                )),
    
    #5
    selectInput("failures", "Cursos jalados",
                list("Ninguno" = "OP1", #"0", 
                     "Al menos 1" = "OP2"  #"1"
                )),
    
    #6
    selectInput("romantic", "Relacion sentimental",
                list("Si" = "OP1", #"y", 
                     "No" = "OP2"  #"n"
                )),
    
    #7
    selectInput("Medu", "educacion madre:",
                list("Educacion superior" = "OP1", #"1", 
                     "Solo escolar" = "OP2"  #"0"
                )),
    
    #8
    selectInput("Fedu", "educacion padre",
                list("Educacion superior" = "OP1", #"1", 
                     "Solo escolar" = "OP2"  #"0"
                )),
    
    #9
    selectInput("guardian", "Guardina estudiantil",
                list("padres" = "OP1", #"parents", 
                     "otra persona" = "OP2"  #"other"
                )),
    
    #10
    selectInput("reason", "Razon para escoger colegio",
                list("Reputacion y proximidad" = "OP1", #"reputation", 
                     "Oferta de cursos o otro" = "OP2"  #"otras"
                )),
    
    #11
    selectInput("freetime", "Tiempo libre",
                list("alto - medio" = "OP1", #"high", 
                     "bajo" = "OP2"  #"low"
                )),
    
    
    actionButton("go", "Hallar Probabilidad")
    
  ),
  
  mainPanel(
    textOutput("selected_var1"),
    textOutput("selected_var2"),
    textOutput("selected_var3")
  )
))