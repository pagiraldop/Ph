library(shiny)
library(markdown)
shinyUI(fluidPage(
  titlePanel("Pruebas de Hipótesis"),
  h5(p(em("Esta aplicación realiza pruebas de hipótesis para datos resumidos")),
     align="left"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'Prueba',
                  label = "Elija la prueba que quiere realizar",
                  choices=c("PH. para la media",
                            "PH. para la varianza",
                            #"PH. para proporciones",
                            "PH. para la diferencia de proporciones",
                            "PH. para la diferencia de medias",
                            # "PH. para la diferencia de medias pareadas",
                            "PH. para el cociente de varianzas"
                            ),
                  selected="PH. para la media"),
      
      
  
      conditionalPanel(condition="input.Prueba =='PH. para la media'",
                       
                       selectInput(inputId='header',
                                   label = "la varianza de la población es conocida?",
                                   choices = list("Si",
                                                  "No"),
                                   selected = "Si"),

                       
                       conditionalPanel(condition="input.header =='Si'",
                                        
                                        numericInput(inputId="varT",
                                                     label = "Ingrese el valor de la varianza POBLACIONAL",
                                                     value = "0.0256",
                                                     min = 1)
                                        
                                        ),
                       
                       conditionalPanel(condition="input.header == 'No'",
                                        
                                        numericInput(inputId="varZ",
                                                     label = "Ingrese el valor de la varianza MUESTRAL",
                                                     value = "1",
                                                     min = 1)
                                        
                                       ),
                       
                       
                       
                       numericInput(inputId="media",
                                    label = "Ingrese el valor de la media muestral",
                                    value = "8.091"),
                       

                       
                       
                       numericInput(inputId="nx",
                                    label = "Ingrese el número de observaciones",
                                    value = "25",
                                    min = 1),
                       
                       
                       numericInput(inputId='mu0m', 
                                    label=HTML("Ingrese el valor de referencia
                                               &mu;<sub>0</sub> para probar
                                               H<sub>0</sub>: &mu; = &mu;<sub>0</sub>"), 
                                    value="8"),
                       
                       selectInput(inputId="h0m", 
                                   label=HTML("Elija la hipótesis alternativa
                                              < , &ne; o >"), 
                                   choices=list("Menor" = "less", 
                                                "Differente" = "two.sided",
                                                "Mayor" = "greater"),
                                   selected = "two.sided"),
                       
                       sliderInput(inputId='alfa',
                                   label = HTML("Opcional: elija un nivel de confianza para 
                                                construir el intervalo de confianza para 
                                                la media &mu;"),
                                   min=0.90, max=0.99,
                                   value=0.95, step=0.01)
      ),
      
      conditionalPanel(condition="input.Prueba =='PH. para la varianza'",
                       numericInput(inputId="varianza",
                                    label = "Ingrese la varianza muestral",
                                    value = "0.391868",
                                    min = 0),
                       
                       numericInput(inputId="nxv",
                                    label = "Ingrese el número de observaciones",
                                    value = "12",
                                    min = 0),
                       
                       
                       numericInput(inputId='sigma0', 
                                    label=HTML("Ingrese el valor de referencia
                                               &sigma;<sub>0</sub> para probar
                                               H<sub>0</sub>: &sigma;<sup>2</sup> = &sigma;<sub>0</sub>"), 
                                    value=1),
                       
                       selectInput(inputId="h0v", 
                                   label=HTML("Elija la hipótesis alternativa
                                              < , &ne; o >"), 
                                   choices=list("Menor" = "less", 
                                                "Differente" = "two.sided",
                                                "Mayor" = "greater"),
                                   selected = "two.sided"),
                       
                       
                       sliderInput(inputId='alfa2',
                                   label = HTML("Opcional: elija un nivel de confianza
                                                para construir el intervalo de 
                                                confianza para la varianza &sigma;<sup>2</sup>"),
                                   min=0.90, max=0.99,
                                   value=0.95, step=0.01)

      ),
      
      conditionalPanel(condition="input.Prueba =='PH. para la diferencia de medias'",
                       
                       selectInput(inputId='iguales',
                                   label = "como son las varianzas de las poblaciones?",
                                   choices = list(#"Conocidas",
                                                  "Desconocidas y diferentes"="FALSE",
                                                  "Desconocidas y iguales"="TRUE"),
                                   selected = "FALSE"),
                       
                       numericInput(inputId="mediap1",
                                    label = "Ingrese el valor de la media muestral de la 
                                    primera población",
                                    value = "546"),
                       
                       numericInput(inputId="mediap2",
                                    label = "Ingrese el valor de la media muestral de la segunda
                                    población",
                                    value = "492"),
                       
                       conditionalPanel(condition="input.iguales == 'Conocidas'",
                                        
                                        numericInput(inputId="varcono",
                                                     label = "Ingrese el valor de la varianza de la primera población",
                                                     value = "0",
                                                     min = 0),
                                        
                                        numericInput(inputId="varcono2",
                                                     label = "Ingrese el valor de la varianza de la segunda población",
                                                     value = "0",
                                                     min = 0)
                                        
                                        ),
                       
                       conditionalPanel(condition=c("input.iguales == 'FALSE'","input.iguales == 'TRUE'"),
                                        
                                        numericInput(inputId="varp22",
                                                     label = "Ingrese el valor de la varianza muestral de la primera 
                                                     población",
                                                     value = "961",
                                                     min = 0),
                                        
                                        numericInput(inputId="varp32",
                                                     label = "Ingrese el valor de la varianza muestral de la segunda
                                                     población",
                                                     value = "676",
                                                     min = 0)
                                        
                                        ),
                       
                       
                       
                       numericInput(inputId="nxv1",
                                    label = "Ingrese el número de observaciones de la primera
                                    muestra",
                                    value = "4",
                                    min = 0),
                       
                       
                       numericInput(inputId="nxv2",
                                    label = "Ingrese el número de observaciones de la segunda
                                    muestra",
                                    value = "4",
                                    min = 0),
                       
                       
                       numericInput(inputId='delta0', 
                                    label=HTML("Ingrese el valor de referencia 
                                               &delta;<sub>0</sub> para la probar
                                               H<sub>0</sub>: &mu;<sub>1</sub> 
                                               - &mu;<sub>2</sub> = &delta;<sub>0</sub>"), 
                                    value=0),
                       
                       selectInput(inputId="h0v", 
                                   label=HTML("La hipótesis nula de la prueba es Ho:
                                              &mu;<sub>1</sub> - 
                                              &mu;<sub>2</sub> = &delta;<sub>0</sub>, 
                                              elija el tipo de hipótesis alterna
                                              < , &ne; o >"), 
                                   choices=list("Menor" = "less", 
                                                "Diferente" = "two.sided",
                                                "Mayor" = "greater"),
                                   selected = "two.sided"),
                       
                       sliderInput(inputId='alfa3', 
                                   label=HTML("Opcional: elija un nivel de confianza para
                                              construir el intervalo de confianza para la
                                              diferencia de medias &mu;<sub>1</sub> - 
                                              &mu;<sub>2</sub>"),
                                   min=0.90, max=0.99,
                                   value=0.95, step=0.01)
                       

                       ),
      
      conditionalPanel(condition="input.Prueba =='PH. para la diferencia de medias pareadas'",
                       
                       
                       numericInput(inputId="mediapa1",
                                    label = "Ingrese el valor de la media muestral de la 
                                    primera población",
                                    value = "0"),
                       
                       numericInput(inputId="varp1",
                                    label = "Ingrese el valor de la varianza muestral de la primera
                                    población",
                                    value = "0"),
                       
                       
                       numericInput(inputId="nxvp1",
                                    label = "Ingrese el numero de observaciones para la primera
                                    muestra",
                                    value = "0"),
                       
                       numericInput(inputId="mediapp2",
                                    label = "Ingrese el valor de la media muestral de la segunda
                                    muestra",
                                    value = "0"),
                       
                       numericInput(inputId="varpp2",
                                    label = "Ingrese el valor de la varianza muestral de la segunda
                                    población",
                                    value = "0"),
                       
                       numericInput(inputId="nxvp2",
                                    label = "Ingrese el numero de observaciones para la segunda
                                    población",
                                    value = "0"),
                       
                       
                       numericInput(inputId='deltap0', 
                                    label=HTML("Ingrese el valor de referencia 
                                               &delta;<sub>0</sub> para la probar
                                               H<sub>0</sub>: &mu;<sub>1</sub> 
                                               - &mu;<sub>2</sub> = &delta;<sub>0</sub>"), 
                                    value=0),
                       
                       selectInput(inputId="h0vp", 
                                   label=HTML("La hipótesis nula de la prueba es Ho:
                                              &mu;<sub>1</sub> - 
                                              &mu;<sub>2</sub> = &delta;<sub>0</sub>, 
                                              elija el tipo de hipótesis alterna
                                              < , &ne; o >"), 
                                   choices=list("Menor" = "less", 
                                                "Diferente" = "two.sided",
                                                "Mayor" = "greater"),
                                   selected = "two.sided"),
                       
                       sliderInput(inputId='alfa6', 
                                   label=HTML("Opcional: elija un nivel de confianza para
                                              construir el intervalo de confianza para la
                                              diferencia de medias &mu;<sub>1</sub> - 
                                              &mu;<sub>2</sub>"),
                                   min=0.90, max=0.99,
                                   value=0.95, step=0.01)
                       
                       
                       ),
      
      conditionalPanel(condition="input.Prueba =='PH. para el cociente de varianzas'",
                       
                       numericInput(inputId="varc1",
                                    label = "Ingrese el valor de la varianza muestral de la primera población",
                                    value = "3.84"),
                       
                       numericInput(inputId="nxvar",
                                    label = "Ingrese el numero de observaciones para la primera
                                    población",
                                    value = "20"),
                       
                       numericInput(inputId="varc2",
                                    label = "Ingrese el valor de la varianza muestral de la segunda
                                    población",
                                    value = "4.54"),
                       
                       numericInput(inputId="nxvar2",
                                    label = "Ingrese el numero de observaciones para la segunda
                                    población",
                                    value = "20"),
                       
                       selectInput(inputId="pi", 
                                   label=HTML("La hipÃ³tesis nula de la prueba es Ho:
                                              &sigma;<sup>2</sup><sub>1</sub> / 
                                              &sigma;<sup>2</sup><sub>2</sub> = 1, 
                                              elija el tipo de hipótesis alterna
                                              < , &ne; o >"), 
                                   choices=list("Menor" = "less", 
                                                "Diferente" = "two.sided",
                                                "Mayor" = "greater"),
                                   selected = "two.sided"),
                       
                       
                       sliderInput(inputId='alfa4', 
                                   label=HTML("Opcional: elija un nivel de confianza para 
                                              construir el intervalo de confianza para 
                                              el cociente
                                              &sigma;<sup>2</sup><sub>1</sub> / 
                                              &sigma;<sup>2</sup><sub>2</sub>"),
                                   min=0.90, max=0.99,
                                   value=0.95, step=0.01)
                       
                     
                       
      ),
      
      conditionalPanel(condition="input.Prueba =='PH. para proporciones'",
                       
                       
                       fileInput(inputId='file1',
                                 label="Use el boton siguiente para cargar su base de datos.",
                                 accept = c(
                                   'text/csv',
                                   'text/comma-separated-values',
                                   'text/tab-separated-values',
                                   'text/plain',
                                   '.csv',
                                   '.tsv'
                                 )),
                       
                       checkboxInput(inputId='header3',
                                     label="Tiene encabezado la base de datos?", 
                                     value=TRUE),
                       
                       selectInput(inputId="sep",
                                   label = "Cual es la separaciÃ³n de los datos?", 
                                   choices = list(Tab='\t', Comma=',',
                                                  Semicolon=';', 'space'=' '),
                                   selected = ';'),
                       
                       
                       selectInput(inputId="variable1",
                                   label="Elija la variable cuantitativa para realizar
                                   la prueba de hipÃ³tesis.",
                                   choices=""),
                       
                       numericInput(inputId='p0', 
                                    label=HTML("Ingrese el valor de referencia
                                               P<sub>0</sub> para probar
                                               H<sub>0</sub>: P = P<sub>0</sub>"), 
                                    value=0),
                       
                       selectInput(inputId="P0", 
                                   label=HTML("Elija la hipÃ³tesis alternativa
                          < , &ne; o >"), 
                                   choices=list("Menor" = "less", 
                                                "Differente" = "two.sided",
                                                "Mayor" = "greater"),
                                   selected = "two.sided"),
                       
                       
                       sliderInput(inputId='alfa5',
                                   label=HTML("Opcional: elija un nivel de confianza para 
                construir el intervalo de confianza para la proporcion P"),
                                   min=0.90, max=0.99,
                                   value=0.95, step=0.01)
                       
                       
                       ),
      
      conditionalPanel(condition="input.Prueba =='PH. para la diferencia de proporciones'",

                       
                       numericInput(inputId="primpobla",
                                   label="Ingrese el número de sucesos en la primera muestra",
                                   value= 18),
                       
                       numericInput(inputId="segpobla",
                                    label="Ingrese el número de sucesos en la segunda muestra",
                                    value= 14),
                       
                       numericInput(inputId="primnpobla",
                                    label="Ingrese el número de observaciones en la primera muestra",
                                    value= 20),
                       
                       numericInput(inputId="segnpobla",
                                    label="Ingrese el número de observaciones en la segunda muestra",
                                    value= 20),
                       
                       numericInput(inputId="pp",
                                    label="Ingrese la probabilidad en que ocurre un suceso en la primera muestra",
                                    max = 1,
                                    min = 0,
                                    value= 0.5),
                       
                       numericInput(inputId="ps",
                                    label="Ingrese la probabilidad en que ocurre un suceso en la segunda muestra",
                                    max = 1,
                                    min = 0,
                                    value= 0.5),
                       
                       selectInput(inputId="P0", 
                                   label=HTML("Elija la hipótesis alternativa
                          < , &ne; o >"), 
                                   choices=list("Menor" = "less", 
                                                "Differente" = "two.sided",
                                                "Mayor" = "greater"),
                                   selected = "two.sided"),
                       
                       
                       sliderInput(inputId='alfa5',
                                   label=HTML("Opcional: elija un nivel de confianza para 
                construir el intervalo de confianza para la diferencia de proporciones P<sub>1</sub> -
                                               P<sub>2</sub>"),
                                   min=0.90, max=0.99,
                                   value=0.95, step=0.01),
                       
                       selectInput(inputId='headerpp',
                                   label = "Desea aplicar factor de corrección",
                                   choices = list("FALSE",
                                                  "TRUE"),
                                   selected = "TRUE")
                       
                       
                       ),
      img(src="unal3.png", height = 60, width = 300),
      br(),
      br(),
      tags$a(href="https://srunal.github.io", "https://srunal.github.io"),
      br(),
      h6("Paula Andrea Giraldo P."),
      h6("Freddy Hernández B.")
      

      
      ),
    
    
      mainPanel(
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Resultados",
                            h5('A continuación los resultados de la prueba de hipótesis'),
                            
                            textOutput("resul1"),
                            
                            plotOutput(outputId="miplot"),
                          
                            textOutput("resul2")
                          
                            
                            ),
                    
                    tabPanel(HTML("Teoría PH. media"), 
                             includeHTML("media.html")
                             ),
                    
                    
                    
                    tabPanel(HTML("Teoría PH. varianza"), 
                             includeHTML("varianza.html")
                             ),
                    
                    
                    tabPanel(HTML("Teoría PH. diferencia de medias"), 
                             includeHTML("diferenciamedias.html")
                             ),
                    
                    tabPanel(HTML("Teoría PH. cociente de varianzas"), 
                             includeHTML("diferenciamedias.html")
                    )
                    
                    
                    )
      )
      )
  )
)