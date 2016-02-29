library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gtable)
library(grid)

# read raw sunspot data
path_to_data <- './data'
getSunspots <- function(year, path='./data/Sunspots/by.year'){
  if(!file.exists(paste(path, '/g', as.character(year), '.txt', sep=""))){
    return(NULL)
  }
  sunspot <- read.fwf(file = paste(path, '/g', as.character(year), '.txt', sep=""),
                      widths = c(4, 2, 2, 4, 8, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 5,
                                 1, 5, 1, 5, 1, 5, 1, 5))[,-c(7,9,11,13,15,17,19,21,23)]
  colnames(sunspot) <- c('year', 'month', 'day', 'time', 'SPG', 'SPGT', 'umbral.area',
                         'spot.area','', 'Corrected.area', 'dist.center', 'angle.hn', 'Carr.Long', 'Lat', 'Central.mer.dist')
  sunspot <- sunspot[,-9]
  sunspot <- sunspot[,] %>% 
    mutate(datetime = ymd(paste(year, month, day, sep="-")) + minutes(round(24*60*time))) %>%
    select(-(year:time))
  sunspot
}

sunspots.monthly <-  read.csv(paste(path_to_data, '/Sunspots/cleaned/sunspots.montlhy.csv',sep=""))
temperature <- read.csv(paste(path_to_data, '/Temperature/cleaned/temperature.csv',sep=""))
shinyServer(function(input, output) {
  
  step <- reactive({
    as.numeric(input$step)
  })
  dateview <- reactive({
    as.Date(input$date.sun.view)
  })
  date.begin <- reactive({
    as.Date(input$date.begin)
  })
  
  date.end <- reactive({
    as.Date(input$date.end)
  })
  
  output$date.v <- renderText({as.character(dateview())})
  
  output$sun.view.plot = renderPlot({
    q.year <- year(dateview())
    sunspot <- getSunspots(q.year)
    if(is.null(sunspot)){
      p<-ggplot(data.frame()) + geom_point() +
        ggtitle(paste("No sunspots data available for", dateview()))
    }else{
      sunspots.date <- sunspot %>%
        filter(as.Date(datetime)==dateview(), 
               !is.na(Central.mer.dist),  
               !is.na(dist.center),  
               !is.na(Corrected.area))
      if(nrow(sunspots.date) == 0){
        p<-ggplot(data.frame()) + geom_point() +
          ggtitle(paste("No sunspots data available for", dateview()))
      }else{
        p<-ggplot(sunspots.date,
               aes(x=Central.mer.dist, y=dist.center, size=Corrected.area)) + geom_point() +
          ggtitle(paste("Sunspots on", dateview()))
      }
    }
    print(p + xlab("Distance to the central meridian") + 
            ylab("Distance from the center of the solar disk"))
      })
  
  output$sp.time.plot = renderPlot({
    p1<-ggplot(sunspots.monthly %>% 
             filter(as.Date(datetime) >= date.begin(),
                    as.Date(datetime) <= date.end()) %>%
               group_by(datetime=as.Date(floor(as.numeric(as.Date(datetime))/step())*step(),
                                         origin="1979-01-01")) %>%
               summarise_each(funs(mean(.,na.rm=T))), 
           aes(x=as.Date(datetime), y=mean.cnt * 10)) + geom_line(colour="blue") + 
      geom_smooth(method=lm) + ylab("Count") + xlab("Date") + ggtitle("Sunspot count and LOTI  over time") + theme_bw()
    p2 <- ggplot(temperature %>% 
                   filter(as.Date(datetime) >= date.begin(),
                          as.Date(datetime) <= date.end()) %>%
                   group_by(datetime=as.Date(floor(as.numeric(as.Date(datetime))/step())*step(),
                                             origin="1979-01-01")) %>%
                   summarise_each(funs(mean(.,na.rm=T))),
                 aes(x=as.Date(datetime), y=LOTI)) + geom_line(colour='red') +  
      geom_smooth(method=lm) + ylab("LOTI") + theme_bw() %+replace% 
      theme(panel.background = element_rect(fill = NA))
    g1 <- ggplot_gtable(ggplot_build(p1))
    g2 <- ggplot_gtable(ggplot_build(p2))
    pp <- c(subset(g1$layout, name == "panel", se = t:r))
    g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                         pp$l, pp$b, pp$l)
    ia <- which(g2$layout$name == "axis-l")
    ga <- g2$grobs[[ia]]
    ax <- ga$children[[2]]
    ax$widths <- rev(ax$widths)
    ax$grobs <- rev(ax$grobs)
    ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
    g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
    grid.draw(g) 
  })
  
  output$sp.hist.plot <- renderPlot({
    ggplot(sunspots.monthly %>% 
             filter(as.Date(datetime) >= date.begin(),
                    as.Date(datetime) <= date.end()) %>%
             group_by(datetime=as.Date(floor(as.numeric(as.Date(datetime))/step())*step(),
                                       origin="1979-01-01")) %>%
             summarise_each(funs(mean(.,na.rm=T))), 
           aes(x=mean.cnt)) + geom_histogram() + 
      xlab("Sunspot Count") + ylab("Frequency") + ggtitle("Sunspot Count Histogram")
  })
  output$temp.hist <- renderPlot({
    ggplot(temperature %>%
             filter(as.Date(datetime) >= date.begin(),
                    as.Date(datetime) <= date.end()) %>%
             group_by(datetime=as.Date(floor(as.numeric(as.Date(datetime))/step())*step(),
                                       origin="1979-01-01")) %>%
             summarise_each(funs(mean(.,na.rm=T))),
           aes(x=LOTI)) + geom_histogram() + ggtitle("LOTI Histogram")
  })
  output$temp.vs.sp <- renderPlot({
    ggplot(sunspots.monthly %>%
             filter(as.Date(datetime) >= date.begin(),
                    as.Date(datetime) <= date.end()) %>%
             group_by(datetime=as.Date(floor(as.numeric(as.Date(datetime))/step())*step(),
                                       origin="1979-01-01")) %>%
             summarise_each(funs(mean(.,na.rm=T))) %>%
             inner_join(temperature  %>%
                          group_by(datetime=as.Date(floor(as.numeric(as.Date(datetime))/step())*step(),
                                                    origin="1979-01-01")) %>%
                          summarise_each(funs(mean(.,na.rm=T))), by='datetime'),
           aes(x=mean.cnt, y=LOTI, colour=mean.mean.area)) + xlab("Sunspot Count") +
      ylab("LOTI") + scale_colour_continuous(name="Sunspot\nMean Area") + 
      ggtitle("LOTI vs. Sunspots Count and Mean Sunspot Area") + geom_point(alpha=0.5) + geom_smooth(method=lm)
  })
})