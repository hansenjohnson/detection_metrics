## server ##

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # initialize reactive values
  v <- reactiveValues(detections = NULL,
                      metrics = NULL,
                      met = NULL)
  
  # compute real detection function
  rl <- reactive({
    plot_df(max_radius = input$max_radius,
            L = input$L,
            x0 = input$x0,
            k = input$k,
            return_data = TRUE)
  })
  
  # compute distribution
  df <- reactive({
    
    # convert inputs
    n_total = input$n_total
    max_radius = input$max_radius
    n_cluster = input$n_cluster
    r_cluster = input$r_cluster
    
    if(input$distribution == 'uniform'){
      
      # output
      df = sprinkle(n = n_total, max_radius = max_radius)
      
    } else {
      
      # find center of each cluster
      center_cluster = sprinkle(n = n_cluster, max_radius = max_radius)
      
      # determine number per cluster
      n_group = n_total/n_cluster
      
      # loop through and build clusters
      cl = list()
      for(icl in 1:n_group){
        
        # build cluster
        tmp = sprinkle(n = n_group, max_radius = r_cluster)
        
        # shift based on center point
        tmp$x = tmp$x+center_cluster$x[icl]
        tmp$y = tmp$y+center_cluster$y[icl]
        
        # recompute range
        tmp$r = sqrt(tmp$x^2 + tmp$y^2)
        
        # store
        cl[[icl]] = tmp
      }
      
      # output
      df = bind_rows(cl) %>%
        filter(r<=max_radius)
    }
    
    # return output
    df
  })
  
  
  # compute detections
  det <- reactive({
    # compute distribution
    det = df()
    L = input$L
    x0 = input$x0
    k = input$k
    
    # apply detection function
    det$detected = NA
    for(ii in 1:nrow(det)){
      det$detected[ii] = rbinom(1, size = 1,prob = logistic_df(det$r[ii],L=L,x0=x0,k=k))
    }
    
    # return variable
    det
    
  })
  
  # plot detection function
  output$detection_function <- renderPlot({
    
    ggplot(rl())+
      geom_path(aes(x=r,y=p))+
      lims(y = c(0,1))+
      labs(x = 'Range (km)', y = 'Probability')+
      theme_bw()+
      theme(plot.margin = margin(40, 2, 2, 2, unit = "pt"))
    
  })
  
  # plot distribution
  output$distribution <- renderPlot({
    
    ggplot()+
      geom_point(data=df(),aes(x=x,y=y),shape=1)+
      coord_equal()+
      labs(x = 'X (km)', y = 'Y (km)')+
      theme_bw()
    
  })
  
  # plot detections
  observeEvent(input$go,{
    
    v$detections = ggplot()+
      geom_point(data=det(),aes(x=x,y=y,shape=as.character(detected),color = as.character(detected)), show.legend = FALSE)+
      scale_shape_manual(values = det_shapes)+
      scale_color_manual(values = det_cols)+
      coord_equal()+
      labs(x = 'X (km)', y = 'Y (km)')+
      theme_bw()
    
  })
  
  output$detections <- renderPlot({
    v$detections
  })
  
  # compute and plot metrics
  
  observeEvent(input$go,{
    # convert reactive output
    det = det()
    rl = rl()
    
    # compute empirical detection function
    edf = empirical_df(det)
    lg = edf$mod
    df_lg = edf$data
    
    # calc edr
    rho = calc_edr(lg, input$max_radius)
    
    # # detections only
    det_only = det %>% filter(detected==1)
    
    # descriptive stats
    mean_r = mean(det_only$r, na.rm = T) %>% round(.,3)
    median_r = median(det_only$r, na.rm = T) %>% round(.,3)
    max_r = max(det_only$r, na.rm = T) %>% round(.,3)
    q95 = quantile(det_only$r, probs = 0.95, na.rm = T) %>% round(.,3)
    
    # logistic regression stats
    r50 = df_lg$r[which.min(abs(df_lg$p - 0.5))]
    r33 = df_lg$r[which.min(abs(df_lg$p - 0.33))]
    
    # make metrics table
    met = tibble(
      metric = c('EDR', 'mean', 'median', 'max', '50%', '33%','q95'),
      r = c(rho, mean_r,median_r, max_r, r50, r33, q95),
      definition = c('Effective detection radius', 
                     'Mean detection distance', 
                     'Median detection distance',
                     'Maximum detection distance',
                     'Distance to 50% detection probability',
                     'Distance to 33% detection probability',
                     'Distance to 95% detection quantile')
    ) %>%
      arrange(r)
    
    # plot metrics
    metrics = ggplot()+
      geom_point(data = det, aes(x=r,y=detected,shape=as.character(detected),color=as.character(detected)),show.legend = FALSE)+
      scale_shape_manual(values = det_shapes)+
      scale_color_manual(values = det_cols)+
      geom_path(data = rl, aes(x=r,y=p))+
      geom_ribbon(data = df_lg, aes(x = r, ymin = lower, ymax = upper), alpha=0.1, color = NA)+
      geom_path(data = df_lg, aes(x=r,y=p), color = 'blue')+
      geom_vline(data = met, aes(xintercept = r), linetype = 2)+
      geom_text(data = met, aes(x = r, y = Inf, label = metric), angle = 90, hjust = -0.1)+
      coord_cartesian(clip = "off") +
      labs(x = 'Range (km)', y = 'Probability')+
      theme_bw()+
      theme(plot.margin = margin(40, 2, 2, 2, unit = "pt"))
    
    # return plot object
    v$metrics = metrics
    v$met = met
    
  })
  
  output$metrics <- renderPlot({
    v$metrics
  })
  
  output$met <- renderTable({
    if(is.null(v$met)){return()}
    
    colnames(v$met) = c('Metric', 'Range (km)', 'Definition')
    
    v$met
    
  }, width = '100%')
  
}