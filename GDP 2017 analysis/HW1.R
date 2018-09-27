library(dplyr)
library(ggplot2)

GDP_data <- read.csv("http://databank.worldbank.org/data/download/GDP.csv", stringsAsFactors = FALSE)
GDP_data <- GDP_data[, -c(3,6:8)]
colnames(GDP_data) <- c("Country.code","Ranking","Economy", "GDP.2017.in.millions.of.US.dollars")
GDP_data <- filter(GDP_data, GDP.2017.in.millions.of.US.dollars != "")
GDP_data <- GDP_data[-c(1,2),]

GDP_data$GDP.2017.in.millions.of.US.dollars <- gsub(",","",GDP_data$GDP.2017.in.millions.of.US.dollars)
GDP_data[,c(2,4)] <- sapply(GDP_data[,c(2,4)], as.numeric)

World_data <- filter(GDP_data,  is.na(Ranking))
GDP_data <- filter(GDP_data, !is.na(Ranking))

round(mean(GDP_data$GDP.2017.in.millions.of.US.dollars), digits = 0)
number_of_countries = tally(GDP_data)
summarise(GDP_data, Mean = round(mean(GDP.2017.in.millions.of.US.dollars), digits = 0), 
          Median = median(GDP.2017.in.millions.of.US.dollars),
          sd = sd(GDP.2017.in.millions.of.US.dollars), 
          iqr = round(IQR(GDP.2017.in.millions.of.US.dollars),digits = 0), 
          Maximum =  max(GDP.2017.in.millions.of.US.dollars),
          Range = max(GDP.2017.in.millions.of.US.dollars)-min(GDP.2017.in.millions.of.US.dollars))

#default hist
library(scales)
breaks = seq(20,20000000, by = 500000)
hist1 <- ggplot(data = GDP_data, aes(x=GDP.2017.in.millions.of.US.dollars)) + 
        geom_histogram(breaks = breaks,color = 'black', fill = '#ed2d2d')
hist1 + geom_vline(aes(xintercept=mean(GDP.2017.in.millions.of.US.dollars)), 
                  color="blue", linetype="dashed", size=1) + 
        labs(title = 'Histogram of GDP', x = "GDP 2017 in millions of US dollars", y = "Frequency") + 
        scale_x_continuous(expand =c(0.01, 0), labels = comma) +
        scale_y_continuous(expand = c(0, 0)) + theme_bw()+
    theme(axis.text.x = element_text(colour ="black"), axis.text.y = element_text(colour="black"))
      
# histogram with density plot, not shown in report
breaks = seq(20,20000000, by = 500000)
hist <- ggplot(data = GDP_data, aes(x=GDP.2017.in.millions.of.US.dollars)) + 
        geom_histogram(aes(y = ..density..),breaks = breaks,
                       color = 'black', fill = 'white')
hist + geom_vline(aes(xintercept=mean(GDP.2017.in.millions.of.US.dollars)), 
                  color="blue", linetype="dashed", size=1) +
        geom_density(adjust = 2, alpha=.6, fill="#FF6666")
# default (adjust*)bw = nrd0; Silverman's rule of thumb

#default data box
boxplt <- ggplot(data = GDP_data, aes(x = '', y=GDP.2017.in.millions.of.US.dollars)) + geom_boxplot()
boxplt + coord_cartesian(ylim = c(0, 20000000))

#data with log.GDP
GDP_data2<- mutate(GDP_data, log.GDP = log(GDP.2017.in.millions.of.US.dollars))
hist2 <- ggplot(data = GDP_data2, aes(x=log.GDP)) + 
        geom_histogram(color = 'black', fill = '#ed2d2d', binwidth = 1.25)
hist2+ labs(title = 'Histogram of log(GDP)', x = "log(GDP) 2017 in millions of US dollars", y = "Frequency")+
        scale_x_continuous(expand =c(0.01, 0)) +
        scale_y_continuous(limits = c(0, 45), expand =c(0,0)) + theme_bw()+
  theme(axis.text.x = element_text(colour ="black"), axis.text.y = element_text(colour="black"))

qqplt<- ggplot(data = GDP_data2, aes(sample = log.GDP)) + geom_qq()+ geom_qq_line()
qqplt + labs(title = 'Q-Q plot of log(GDP)', x = "Theoretical distribution", y = "Sample distribution")+
  theme_bw()+
  theme(axis.text.x = element_text(colour ="black"), axis.text.y = element_text(colour="black"))

#percentiles
per <- round(quantile(GDP_data$GDP.2017.in.millions.of.US.dollars, probs = seq(0, 1, by= 0.25)), digits = 0) 
per

# data without top 7% percentile countries (top 14 removed)
GDP_data3 <- filter(GDP_data, GDP.2017.in.millions.of.US.dollars <=1161217)
GDP3sum <- sum(GDP_data3$GDP.2017.in.millions.of.US.dollars)


hist3 <- ggplot(data = GDP_data3, aes(x=GDP.2017.in.millions.of.US.dollars)) + 
  geom_histogram(color = 'black', fill = '#ed2d2d', binwidth = 75000)     
hist3 + labs(title = 'Histogram of GDP (upto 93rd percentile)', x = "GDP 2017 in millions of US dollars", y = "Frequency")+
  scale_x_continuous(limits = c(0,1200000), expand =c(0.09, 10), labels = comma) +
  scale_y_continuous(limits = c(0, 40), expand =c(0,0)) + theme_bw()+
  theme(axis.text.x = element_text(colour ="black"), axis.text.y = element_text(colour="black"))

boxplt3 <- ggplot(data = GDP_data3, aes(x = '', y=GDP.2017.in.millions.of.US.dollars)) + 
          geom_boxplot(fill = '#ed2d2d' )
boxplt3 + labs(title = 'Boxplot of GDP (upto 93rd percentile)', x = "Countries", 
               y = "GDP 2017 in millions of US dollars")+
  theme_bw()+ scale_y_continuous(labels = comma)+  scale_x_discrete(expand=c(0,0.5))+
  theme(axis.text.x = element_text(colour ="black"), axis.text.y = element_text(colour="black"))


#75 percentile
GDP_data4 <- filter(GDP_data, GDP.2017.in.millions.of.US.dollars <=201679)
hist4 <- ggplot(data = GDP_data4, aes(x=GDP.2017.in.millions.of.US.dollars)) + 
  geom_histogram(color = 'black', fill = '#ed2d2d', binwidth = 20000)     
hist4

#world data
World_data <- filter(World_data,  !is.na(GDP.2017.in.millions.of.US.dollars))

#regions
barplt1 <- ggplot(data = World_data[c(2:8),]) + 
           geom_bar(aes(x=Economy, y=GDP.2017.in.millions.of.US.dollars), stat='identity',
                    color ='black', fill = '#ed2d2d')
barplt1 + theme_bw()+theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  scale_y_continuous(limits = c(0,25000000),expand = c(0,0),labels = comma) + 
  labs(title = 'GDP by geographic regions', x='Region', y = 'GDP 2017 in millions of US dollars')+
  theme(axis.text.x = element_text(colour ="black"), axis.text.y = element_text(colour="black"))+
  coord_flip()
#income
data1 <-arrange(World_data[c(9:12),],(GDP.2017.in.millions.of.US.dollars))
barplt2 <- ggplot(data1) + 
  geom_bar(aes(x=Economy, y=GDP.2017.in.millions.of.US.dollars), stat='identity',
           color ='black', fill = '#ed2d2d')
barplt2 + theme_bw()+theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  scale_y_continuous(labels = comma) + labs(title = 'GDP by income groups', x='Income group',
                                            y = 'GDP 2017 in millions of US dollars')+
  theme(axis.text.x = element_text(colour ="black"), axis.text.y = element_text(colour="black"))
