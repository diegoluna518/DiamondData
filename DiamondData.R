library(ggplot2)
qplot(data = diamonds, x = price, bins = 30)+
  facet_wrap(~diamonds$cut)

qplot(data = diamonds, x = price, bins = 30,
      xlim = c(5000,10000))+
  facet_wrap(~diamonds$cut)
by(diamonds$price, diamonds$cut, summary)

qplot(x = price, data = diamonds) + 
  facet_wrap(~cut, scales = "free")

qplot(x = price, data = diamonds, binwidth = .1) +
  scale_x_log10()+ 
  facet_wrap(~cut, scales = "free")

ggplot(diamonds, aes(x = price, y = color)) +
  geom_boxplot()
by(diamonds$price, diamonds$color, summary)

by(diamonds$price, diamonds$color, IQR)

# Investigate the price per carat of diamonds across
# the different colors of diamonds using boxplots.

ggplot(aes(x = color, y = price/carat), data = diamonds,
       binwidth = 25)+
  scale_y_continuous(breaks = seq(2000,6000,100))+
  coord_cartesian(ylim = c(2000,6000))+
  geom_boxplot(fill = c("gainsboro","gold","goldenrod","khaki", "lightseagreen","lightslateblue","lightslategrey"))+
  labs(title = "Price by carat by color")+
  theme(plot.title = element_text(size = rel(2)))+
  theme(plot.title = element_text(face = "bold"))
library(gridExtra)
library(grid)

p1 <- qplot(x = cut, y= price, data = diamonds, geom = "boxplot", fill = cut) +
  coord_cartesian(ylim = c(0,7000))
p2 <- qplot(x = color, y= price, data = diamonds, geom = "boxplot"
            , fill = color) +
  coord_cartesian(ylim = c(0,8000))
p3 <- qplot(x = clarity, y= price, data = diamonds, geom = "boxplot", fill = clarity) +
  coord_cartesian(ylim = c(0,6500))
grid <- grid.arrange(p1, p2, p3, ncol = 1)

#Investigate frequency polygon
ggplot( data = diamonds, aes(x = carat), binwidth = 10) +
  scale_y_continuous(breaks = 2000,15000,100) +
  geom_freqpoly()
qplot(x = carat,
      data = diamonds,
      binwidth = 0.1,
      xlab = 'Carats',
      ylab = 'Count',
      geom = 'freqpoly') +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, 0.1)) +
  scale_y_continuous(breaks = seq(0,12000,1000))

qplot(x = carat, data = diamonds, binwidth = 0.1,
      geom = "freqpoly")+
  scale_x_continuous(limits=c(0,5), breaks = seq(0,5,0.1))+
  scale_y_continuous(breaks = seq(0,12000,1000))
table(diamonds$carat)