## ui ##

ui <- fluidPage(
  titlePanel('Comparing detection range metrics'),
  
  h3('Step 1: choose distribution and detection function'),
  
  column(6,
         
         h4('Distribution of available cues:'),
         
         numericInput(inputId = "n_total", 
                      label = 'Number of cues', 
                      min=1, 
                      max = 1e6,
                      value = 1e3, 
                      step = 10),
         
         numericInput(inputId = "max_radius", 
                      label = 'Maximum detection radius (km)', 
                      min=1, 
                      max = 250,
                      value = 40, 
                      step = 5),
         
         selectInput(inputId = "distribution",
                     label = 'Distribution', 
                     choices = c('uniform', 'clustered'), 
                     selected = 'uniform', 
                     multiple = FALSE),
         
         conditionalPanel(
           condition = "input.distribution == 'clustered'",
   
         numericInput(inputId = "n_cluster", 
                      label = 'Number of clusters', 
                      min=1, 
                      max = 50,
                      value = 10, 
                      step = 1),
         
         numericInput(inputId = "r_cluster", 
                      label = 'Radius of clusters (km)', 
                      min=1, 
                      max = 50,
                      value = 1, 
                      step = 1)
         )
  ),
  
  column(6,
         
         h4('Adjust detection function:'),
         
         numericInput(inputId = "L", 
                      label = 'Y intercept', 
                      min=0, 
                      max = 2,
                      value = 0.9, 
                      step = 0.05),
         
         numericInput(inputId = "x0", 
                      label = 'Inflection range (km)', 
                      min=0, 
                      max = 100,
                      value = 15, 
                      step = 1),
         
         numericInput(inputId = "k", 
                      label = 'Slope', 
                      min=-10, 
                      max = 0,
                      value = -0.25, 
                      step = 0.05),
         
  ),
  column(12,
         # leave blank to preserve alignment
         ),
  column(6,
         plotOutput("distribution")
  ),
  column(6,
         plotOutput("detection_function"),
  ),
  column(12,
         h3('Step 2: run simulation and compare metrics'),
         actionButton(inputId = "go", label = 'Run simulation', width = '100%')
         ),
  column(6,
         plotOutput("detections")
  ),
  column(6,
         plotOutput("metrics")
  ),
  column(8,offset = 2,
         tableOutput("met")
  ),
)