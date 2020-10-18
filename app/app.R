rm(list=ls())
library(tidyverse)
library(lubridate)
library(scales)
library(png)
library(grid)

interpolate_colors <- function(c1,c2,n=100) {
  r1 <- paste0("0x",substr(c1,2,3)) %>% strtoi()
  g1 <- paste0("0x",substr(c1,4,5)) %>% strtoi()
  b1 <- paste0("0x",substr(c1,6,7)) %>% strtoi()
  r2 <- paste0("0x",substr(c2,2,3)) %>% strtoi()
  g2 <- paste0("0x",substr(c2,4,5)) %>% strtoi()
  b2 <- paste0("0x",substr(c2,6,7)) %>% strtoi()
  data.frame(r=seq(r1,r2,(r2-r1)/(n-1))%>%round()%>%as.hexmode()%>%as.character(),
             g=seq(g1,g2,(g2-g1)/(n-1))%>%round()%>%as.hexmode()%>%as.character(),
             b=seq(b1,b2,(b2-b1)/(n-1))%>%round()%>%as.hexmode()%>%as.character()
  ) %>% mutate(s=paste0("#",r,g,b)) %>% pull(s)
}


wvr_black <- "#000000"
wvr_blue  <- "#0038ff"
wvr_green <- "#99ff1a"
wvr_white <- "#ffffff"
wvr_grad <- interpolate_colors(wvr_green,wvr_blue)

g1 <- readPNG("washmachine.png") %>% rasterGrob(interpolate=TRUE)
g2 <- readPNG("toilet.png") %>% rasterGrob(interpolate=TRUE)
g3 <- readPNG("shower.png") %>% rasterGrob(interpolate=TRUE)
g <- list(g1,g2,g3,g3,g3)

load("water.RData")
x <- x %>%
  mutate(day=day(t),month=month(t),hour=hour(t),minute=minute(t),year=year(t)) %>% 
  mutate(month=as.integer(month))
my_now <- now()
my_month  <- month(my_now)
my_day    <- day(my_now)
my_hour   <- hour(my_now) - 4
my_minute <- minute(my_now)
x2 <- x %>% filter(month<my_month|
                     (month==my_month&day<my_day)|
                     (month==my_month&day==my_day&hour<my_hour)|
                     (month==my_month&day==my_day&hour==my_hour&minute<my_minute))

ui <- fluidPage(
  plotOutput("plt1",height=200),
  plotOutput("plt2",height=200),
  plotOutput("plt3",height=1000),
  plotOutput("plt4",height=200),
  plotOutput("plt5",height=200),
  textOutput("txt")
)

server <- function(input, output) {
  output$txt <- renderText({
    "Data from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/FIE0S4"
  })
  
  my_timeplot <- function(d) {
    plt <- d %>% ggplot(aes(t,avg_rate)) +
      stat_smooth(method = "loess",span=0.1,se=F,col=wvr_blue) +
      scale_x_datetime(breaks=date_breaks("3 hour"),labels = date_format("%H")) +
      theme(text= element_text(size=20)) + xlab("") + 
      ylab("Liter / Minute") + coord_cartesian(ylim=c(0,4),expand = F)
    fit_x <- ggplot_build(plt)$data[[1]]$x
    fit_y <- ggplot_build(plt)$data[[1]]$y
    df <- data.frame(x=as_datetime(fit_x),y=fit_y,is_peak=ggpmisc:::find_peaks(fit_y,span = 7)) %>% filter(is_peak) %>%
      mutate(xmin=x-minutes(45),xmax=x+minutes(45),ymin=ifelse(y<3,y-1,y-2),ymax=ifelse(y<3,y+1,y)) 
    for(i in seq(nrow(df))) {
      plt <- plt + annotation_custom(sample(g,1)%>%.[[1]],xmin=df$xmin[i],xmax=df$xmax[i],ymin=df$ymin[i],ymax=df$ymax[i])
    }
    plt
  }
  
  output$plt1 <- renderPlot({
    x2 %>% filter(t>max(x2$t)-days(2),t<max(x2$t)-days(1)) %>% mutate(avg_rate=avg_rate/2) %>% my_timeplot()
  })
  output$plt2 <- renderPlot({
    x2 %>% filter(t>max(x2$t)-days(1)) %>% my_timeplot()
  })
  output$plt3 <- renderPlot({
    val_y <- x2 %>% pull(avg_rate) %>% sum()
    val_m <- x2 %>% filter(month==my_month) %>% pull(avg_rate) %>% sum()
    val_d <- x2 %>% filter(month==my_month,day==my_day) %>% pull(avg_rate) %>% sum()
    df <- data.frame(val=c(val_d,val_m,val_y),
                     maxval=c(550,16700,200000),
                     title=c("Heute","Monat","Jahr"), stringsAsFactors = F
    ) %>% mutate(percentage=val/maxval,label=paste0(round(val)," / ",as.integer(maxval), " Liter"))
    
    df$title <- factor(df$title,levels=c("Heute","Monat","Jahr"))
    
    ggplot(df, aes(fill = percentage, col=percentage, ymax = percentage, ymin = 0, xmax = 2, xmin = 1)) +
      geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="#ece8bd") +
      geom_rect() +
      coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
      geom_text(aes(x=0.5,y=1.5, label = label, colour=percentage),size=10) +
      facet_wrap(~title, ncol = 1) +
      scale_fill_gradientn(colors=wvr_grad,aesthetics = c("fill","color")) +
      theme_void() +
      theme(text = element_text(size=40)) +
      guides(fill=F,col=F)
  },height=1000)
  
  my_barplot <- function(d) {
    d %>% ggplot(aes(day,Verbrauch,fill=Verbrauch)) + geom_bar(stat="identity") +
      geom_hline(yintercept = 500,lty="dashed") + geom_hline(yintercept=750) +
      ylab("Verbrauch / l") + xlab("Tag") + 
      scale_fill_gradientn(colors=wvr_grad,limits=c(500,750),oob=scales::squish) + 
      theme_bw() +
      scale_x_continuous(breaks=seq(0,31,3)) + guides(fill=F) +
      scale_y_continuous(breaks=c(500,750)) +
      theme(text = element_text(size=20))
  }
  
  output$plt4 <- renderPlot({
    x2 %>% filter(month==10) %>% group_by(day) %>% summarize(Verbrauch=sum(avg_rate)) %>% ungroup() %>%
      mutate(gr=ifelse(Verbrauch>750,0,ifelse(Verbrauch>500,1,2))) %>% my_barplot()
  })
  output$plt5 <- renderPlot({
    x2 %>% filter(month==10) %>% group_by(day) %>% summarize(Verbrauch=sum(avg_rate)/2) %>% ungroup() %>%
      mutate(gr=ifelse(Verbrauch>750,0,ifelse(Verbrauch>500,1,2))) %>% my_barplot()
  })
}
shinyApp(ui,server)
