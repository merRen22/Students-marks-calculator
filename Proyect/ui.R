library(shiny)

# Elemento de la interfaz
shinyUI(pageWithSidebar(
  
  # Titulo
  headerPanel("Desempeño escolar con teorema de Bayes"),
  
  sidebarPanel(    
    #1
    selectInput("sex", "Sexo:",
                c("Mujer" = "OP1",   #F 
                     "Hombre" = "OP2"   #M
                )),
    
    #2
    selectInput("age", "Edad:",
                c("Mayor de 18" = "OP1", #"mature", 
                     "Menor de 18" = "OP2"  #"notMature"
                )),
    
    #3
    selectInput("address ", "Zona",
                c("urbano" = "OP2", #"1", 
                     "rural" = "OP1"  #"0"
                )),
    
    #4
    selectInput("famsize", "Tamaño familia",
                list("3 o mas personas " = "OP1", #"GT3", 
                     "menor o igual que 3" = "OP2"  #"LE3"
                )),
    
    #5
    selectInput("failures", "Cursos jalados",
                list("Ninguno" = "OP2", #"0", 
                     "Al menos 1" = "OP1"  #"1"
                )),
    
    #6
    selectInput("romantic", "Relacion sentimental",
                list("Si" = "OP2", #"y", 
                     "No" = "OP1"  #"n"
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
    
    textOutput("Aselected_var1"),
    textOutput("Aselected_var2"),
    textOutput("Aselected_var3"),
    textOutput("Aselected_var4"),
    textOutput("Aselected_var5"),
    textOutput("Aselected_var6"),
    textOutput("Aselected_var7"),
    textOutput("Aselected_var8"),
    textOutput("Aselected_var9"),
    textOutput("Aselected_var10"),
    
    
    
    textOutput("selected_var1"),
    textOutput("selected_var2"),
    textOutput("selected_var3"),
    textOutput("selected_var4"),
    textOutput("selected_var5"),
    textOutput("selected_var6"),
    textOutput("selected_var7"),
    textOutput("selected_var8"),
    textOutput("selected_var9"),
    textOutput("selected_var10")
  )
))