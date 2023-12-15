library(shiny)
library(ggplot2)
library(png)
library(dplyr)

plane <- readPNG('plane.png')
red <- readPNG("red.png")
accept = readPNG('accept.png') %>% rowSums(dims = 2)

extract_anchor_img <- function(x, i){
  
  sigma <- rowSums(x, dims = 2)
  y <- x
  red_layer = x[,,1]
  mask <- (sigma > 1) & (red_layer > 0.5) | (red_layer < 0.5)
  
  for (i in 1:3) {
    layer = y[, , i]
    layer[mask] = 1
    y[, , i] = layer
  }
  return(y)
}

anchor_img <- extract_anchor_img(red, 1)

get_anchor_points <- function(anchor_img){
  
  m = dim(anchor_img)[1]
  n = dim(anchor_img)[2]
  
  I = matrix(rep(matrix(seq(m), ncol = 1),n), ncol = n)
  J = t(matrix(rep(matrix(seq(n), ncol = 1),m), ncol = m))
  
  mask = rowSums(anchor_img, dims = 2) != 3
  
  return(list(I[mask], J[mask]))
  
}

anchor_points = get_anchor_points(anchor_img)

plot_sampled_survivorship <- function(plane,
                                      anchor_points,
                                      accept,
                                      colour = 'black',
                                      dot_size = 1.5,
                                      marker = 16,
                                      jitter = 20){
  
  h = dim(plane)[1]
  w = dim(plane)[2]
  
  x = list()
  y = list()
  
  while (length(x) < 120) {
    index = sample(1:length(anchor_points[[1]]),1)
    i = anchor_points[[1]][index] + sample(seq(-jitter, jitter + 1), 1)
    j = anchor_points[[2]][index] + sample(seq(-jitter, jitter + 1), 1)
    
    if (accept[i,j] > 0){
      x <- append(x,j)
      y <- append(y,i)
    }
  }
  
  df <- do.call(rbind, Map(data.frame, x=x, y=y))
  plot(ggplot(df, aes(x,y)) + 
         annotation_custom(grid::rasterGrob(plane, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0, -h) + # minus for reversed y scale
         scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
         scale_y_reverse(expand=c(0,0),limits=c(h,0)) + 
         geom_point(colour = colour, size = dot_size, shape = marker) +
         theme(axis.title.x = element_blank(),
               axis.text.x = element_blank(),
               axis.ticks.x = element_blank(),
               axis.title.y = element_blank(),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank())
  )
}

ui <- fluidPage(

    titlePanel("Build your own survivorship bias plane!"),

    sidebarLayout(
        sidebarPanel(
            selectInput("marker",
                        "Pick a marker",
                        choice = list(`Marker: o` = 16,
                                      `Marker: +` = 3,
                                      `Marker: X` = 4,
                                      `Marker: <>` = 5),
                        selected = 16),
            selectInput("colour",
                        "Pick a colour",
                        choice = list('darkgreen', 
                                      'blue',  
                                      'purple', 
                                      'yellow',
                                      'magenta',
                                      'orange'),
                        selected = 'darkgreen'),
            sliderInput("dotsize",
                        "Dot size:",
                        min = 1,
                        max = 20,
                        value = 2)
        ),

        mainPanel(
           plotOutput("survivor_plot")
        )
    )
)

server <- function(input, output) {

    output$survivor_plot <- renderPlot({
      plot_sampled_survivorship(plane, 
                                anchor_points, 
                                accept, 
                                marker = as.numeric(input$marker),
                                colour = input$colour, 
                                dot_size = input$dotsize,
                                jitter = 5)
        
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
