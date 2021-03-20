#Packages#
library(tidyverse)
library(plyr)
library(skimr)
library(ggplot2)
library(tibbletime)
library(ggthemes)
library(cowplot)
library(gridExtra)
library(grid)
library(fpp3)
library(forecast)
library(smooth)
library(nortsTest)
library(tseries)
#Data Access#
df <- read.csv("avocado.csv")

#Data Preprocessing#
#Convert data
df$Date <- as.Date(as.character(df$Date, format="%Y-%m-%d"))
df$type <- as.factor(df$type)
#Rename Column
df <- rename(df, c("X4046" = "Small Hass", "X4225" = "Large Hass", "X4770" = "XLarge Hass"))
#Add new column convert to month.abb
df$month <- format(as.Date(df$Date), "%m")
df$Month <- sapply(df$month, function(x) month.abb[as.numeric(x)])
df$Month <- factor(df$Month, levels = month.abb)
df$year <- factor(df$year)
#Delete Column
df <- select(df, -X)
#Sorting data
df <- arrange(df, Date)
#Checking duplicated data and missing value
sum(duplicated(df))
sum(is.na(df))
head(df)

#Exploratory Data Analysis#
#Density plots of different types of avocado
options(repr.plot.width = 8, repr.plot.height = 4)
ggplot(df, aes(x=AveragePrice, fill=type)) + geom_density(alpha=0.5) + facet_wrap(~type) + theme_minimal() +
theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + labs(title = "Avocado Price by Type") + scale_fill_brewer(palette = "Set2")
#Price Trends
price_trend <- df %>% select(Date, AveragePrice, type) %>% 
  ggplot(aes(x=Date, y=AveragePrice,color=type, fill=type)) + geom_area(alpha=0.3) + 
  theme_bw() + labs(title = "Average Price (2015-2018)") + scale_fill_brewer(palette = "Dark2")
price_trend         
#Create a facet wrap for each product
ggplot(df, aes(x=Date, y=AveragePrice, col=type)) + geom_line() + facet_wrap(~type) + 
  theme_minimal() + theme(legend.position = "bottom")
#Filter by type
organic <- df %>% select(Date, AveragePrice, type, Total.Volume) %>% filter(type=="organic")
conventional <- df %>% select(Date, AveragePrice, type, Total.Volume) %>% filter(type=="conventional")
#Organic Avocado
organic <- as_tbl_time(organic, index=Date)
organic <- as_period(organic, 'month')
#Conventional Avocado
conventional <- as_tbl_time(conventional, index = Date)
conventional <- as_period(conventional, 'month')
#Create yearly avocados price by type
options(repr.plot.width = 8, repr.plot.height = 6)
conventional_monthly <- conventional %>% 
  ggplot(aes(x=Date, y=AveragePrice)) + geom_line(color = "#0072B2") + theme_economist() +
  theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "grey")) + 
  labs(title = "Conventional Avocados") + geom_hline(yintercept = max(conventional$AveragePrice), linetype="dashed", color="red") + 
  geom_hline(yintercept = min(conventional$AveragePrice), linetype="dashed", color = "blue")

conventional_volume <- conventional %>% 
  ggplot(aes(x=Date, y=Total.Volume)) + geom_bar(stat = 'identity', fill="#0072B2", color="black") + theme_economist() + 
  theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "grey")) + 
  geom_smooth(method = "loess", color="red")

organic_monthly <- organic %>% 
  ggplot(aes(x=Date, y=AveragePrice)) + geom_line(color = "#009E73") + theme_economist() + 
  theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "grey")) + 
  labs(title = "Organic Avocados") + geom_hline(yintercept = max(organic$AveragePrice), linetype="dashed", color="red") + 
  geom_hline(yintercept = min(organic$AveragePrice), linetype="dashed", color="blue")

organic_volume <- organic %>% 
  ggplot(aes(x=Date, y=Total.Volume)) + geom_bar(stat = 'identity', fill="#009E73", color="black") + 
  theme_economist() + theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "grey")) + 
  geom_smooth(method = "loess", color="red")

plot_grid(conventional_monthly, organic_monthly, conventional_volume, organic_volume, nrow=2, ncol=2)

#Distribution Prices of Avocados by Year
ggplot(df, aes(x=AveragePrice, fill=as.factor(year))) + geom_density(alpha = .5) +
  theme_economist() + facet_wrap(~year) + theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill="bisque2")) + 
  guides(fill=FALSE) + labs(title = "Distribution of Prices by year", x='Averege Price', y='Density') + 
  scale_fill_manual(values = c("#D55E00","#0072B2","#F0E442","#009E73"))

#Detecting seasonality patterns
avg <- aggregate(AveragePrice ~ Month + type, df, mean)

conv_patterns <- avg %>% filter(type == "conventional") %>% arrange(Month) %>% 
  ggplot(aes(x=Month, y=AveragePrice)) + geom_point(color = "#FC4E07", aes(size=AveragePrice)) + geom_line(group=1, color = "#0072B2") +
  theme_economist() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#E7B800")) + 
  labs(title = "Conventional Avocados", x="Month", y="Average Price")

org_patterns <- avg %>% filter(type == "organic") %>% arrange(Month) %>% 
  ggplot(aes(x=Month, y=AveragePrice)) + geom_point(color = "#FC4E07", aes(size=AveragePrice)) + geom_line(group=1, color = "#0072B2") + 
  theme_economist() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#E7B800")) + 
  labs(title = "Organic Avocados", x="Month", y="Average Price")

plot_grid(conv_patterns, org_patterns, nrow = 2)

#Seasonality fluctuatuions pattern each year
avg2 <- aggregate(AveragePrice~Month + type + year, df, mean)
avg2 <- filter(avg2, year %in% c("2015", "2016", "2017"))

ggplot(avg2, aes(x=Month, y=AveragePrice, group = type, color = type)) + geom_point(color = "#FC4E07") + geom_line(size = 1) + 
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "lightgrey")) + labs(title = "Seasonal Fluctuation of Avocados", x="Month", y="Average Price") + 
  facet_grid(~ year)

#Measuring Standard Deviation per month through each year by type of avocado
std_avo <- aggregate(AveragePrice~Month + type + year, df, sd)
std_avo <- std_avo %>% rename( c("AveragePrice"="std")) %>% filter(year %in% c("2015", "2016", "2017"))

std_conv <- std_avo %>% filter(type == "conventional") %>% ggplot(aes(x=Month, y=std)) + geom_point(aes(size=std), color = "#0072B2") +
  geom_segment(aes(x=Month, xend=Month, y=min(std), yend=max(std)), show.legend = TRUE, linetype="dashed", size=0.1) + coord_flip() + 
  facet_grid(~ year) + theme_tufte() + theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "lightgrey"), legend.position = "none") + 
  labs(title = "Conventional Avocados \n Price Volatility", x="Month", y="Standard Deviation")

std_org <- std_avo %>% filter(type == "organic") %>% ggplot(aes(x=Month, y=std)) + geom_point(aes(size=std), color = "#009E73") +
  geom_segment(aes(x=Month, xend=Month, y=min(std), yend=max(std)), show.legend = TRUE, linetype="dashed", size=0.1) + coord_flip() + 
  facet_grid(~ year) + theme_tufte() + theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "lightgrey"), legend.position = "none") + 
  labs(title = "Organic Avocados \n Price Volatility", x="Month", y="Standard Deviation")

plot_grid(std_conv, std_org, nrow = 2)

#How the price changes per month, filter by type and year
options(repr.plot.width = 10, repr.plot.height = 8)
se <- function(x) sqrt(var(x)/length(x))

conv <- df %>% select(year, Month, AveragePrice, type) %>% filter(type == "conventional", year %in% c("2015", "2016", "2017")) %>% group_by(year, Month) %>% 
  ggplot(aes(x=Month, y=AveragePrice, fill=Month), color="white") + geom_bar(width = 1, stat = 'identity') + 
  geom_errorbar(aes(ymin = AveragePrice - se(AveragePrice),
                    ymax = AveragePrice + se(AveragePrice),
                    color = Month), width = .2) + scale_y_continuous(breaks = 0:nlevels(df$Month)) + 
  facet_wrap(~ year) + theme_minimal() + theme(axis.ticks = element_blank(),
                                               axis.text.y = element_blank(),
                                               axis.title = element_blank(),
                                               axis.line = element_blank(),
                                               plot.background = element_rect(fill = "lightgrey"),
                                               legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  coord_polar() + labs(title = "Seasonal cycle \n Conventional Avocados") + 
  scale_fill_manual(values = c('#E74C3C','#F7DC6F','#58D68D','#5DADE2','#8E44AD','#E67E22','#5D6D7E','#229954','#5499C7','#873600','#512E5F','#D4AC0D'))

org <- df %>% select(year, Month, AveragePrice, type) %>% filter(type == "organic", year %in% c("2015", "2016", "2017")) %>% group_by(year, Month) %>% 
  ggplot(aes(x=Month, y=AveragePrice, fill=Month), color="white") + geom_bar(width = 1, stat = 'identity') + 
  geom_errorbar(aes(ymin = AveragePrice - se(AveragePrice),
                    ymax = AveragePrice + se(AveragePrice),
                    color = Month), width = .2) + scale_y_continuous(breaks = 0:nlevels(df$Month)) + 
  facet_wrap(~ year) + theme_minimal() + theme(axis.ticks = element_blank(),
                                               axis.text.y = element_blank(),
                                               axis.title = element_blank(),
                                               axis.line = element_blank(),
                                               plot.background = element_rect(fill = "lightgrey"),
                                               legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  coord_polar() + labs(title = "Seasonal cycle \n Conventional Avocados") + 
  scale_fill_manual(values = c('#E74C3C','#F7DC6F','#58D68D','#5DADE2','#8E44AD','#E67E22','#5D6D7E','#229954','#5499C7','#873600','#512E5F','#D4AC0D'))

grid.arrange(conv, org, nrow = 2)

#Avocados Price Changes
options(repr.plot.width = 10, repr.plot.height = 7)
structured_data <- avg2 %>% group_by(year, Month) %>%  spread_(key="year", value="AveragePrice")
colnames(structured_data) <- c("Months","Type","First_year","Second_year", "Third_year")
structured_data$first_pct <- NA
structured_data$second_pct <- NA

structured_data$first_pct <- (structured_data$Second_year - structured_data$First_year)/structured_data$First_year
structured_data$second_pct <- (structured_data$Third_year - structured_data$Second_year)/structured_data$Second_year

#Conventional Avocados
structured_data_conv <- structured_data %>% filter(Type == "conventional") %>% 
  mutate(first_cond=ifelse(first_pct > 0, "Positive","Negative"), 
         second_cond=ifelse(second_pct > 0, "Positive","Negative"))  

first_change <- ggplot(structured_data_conv) + 
  geom_segment(aes(x=Months, xend=Months, y=First_year, yend=Second_year), color="#1B2631", size = 1) + 
  geom_point(aes(x=Months, y=First_year), color="#F4D03F", size=3) + 
  geom_point(aes(x=Months, y=Second_year), color="#3498DB", size=3) + 
  coord_flip() + theme_economist() + theme(legend.position = "top", plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "lightgrey")) + 
  labs(title = "Conventional Avocado Price changes \n (2015 - 2016)", x="Months", y="Price", caption = "Blue: Year of 2016, Green: Year of 2017")
second_change <- ggplot(structured_data_conv) + 
  geom_segment(aes(x=Months, xend=Months, y=Second_year, yend=Third_year), color="#1B2631", size = 1) + 
  geom_point(aes(x=Months, y=Second_year), color="#3498DB", size=3) + 
  geom_point(aes(x=Months, y=Third_year), color="#D35400", size=3) + 
  coord_flip() + theme_economist() + theme(legend.position = "top", plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "lightgrey")) + 
  labs(title = "Conventional Avocado Price changes \n (2016 - 2017)", x="Months", y="Price", caption = "Blue: Year of 2016, Green: Year of 2017")

first_dif <- structured_data_conv %>% select(Months, first_pct, first_cond) %>% 
  ggplot(aes(fill=first_cond)) + geom_bar(stat = "identity", alpha = 0.5, aes(x=Months, y=round(first_pct,2) * 100), color="black") + 
  theme_economist() + theme(axis.text.x = element_text(angle=90), plot.background = element_rect(fill = "lightgrey"), legend.position = "bottom") + 
  labs(x="Month", y="% Difference") + guides(fill = guide_legend(title = "Diff Status")) + scale_fill_manual(values = c("#C0392B", "#F4D03F"))
second_dif <- structured_data_conv %>% select(Months, second_pct, second_cond) %>% 
  ggplot(aes(fill=second_cond)) + geom_bar(stat = "identity", alpha = 0.5, aes(x=Months, y=round(second_pct,2) * 100), color="black") + 
  theme_economist() + theme(axis.text.x = element_text(angle=90), plot.background = element_rect(fill = "lightgrey"), legend.position = "bottom") + 
  labs(x="Month", y="% Difference") + guides(fill = guide_legend(title = "Diff Status")) + scale_fill_manual(values = c("#C0392B", "#F4D03F"))

plot_grid(first_change, second_change, first_dif, second_dif, nrow = 2, ncol = 2)

#Organic Avocados
structured_data_org <- structured_data %>% filter(Type == "organic") %>% 
  mutate(first_cond=ifelse(first_pct > 0, "Positive","Negative"), 
         second_cond=ifelse(second_pct > 0, "Positive","Negative"))
first_change_org <- ggplot(structured_data_org) + 
  geom_segment(aes(x=Months, xend=Months, y=First_year, yend=Second_year), color="#17202A", size = 1) + 
  geom_point(aes(x=Months, y=First_year), color="#F4D03F", size=3) + 
  geom_point(aes(x=Months, y=Second_year), color="#3498DB", size=3) + 
  coord_flip() + theme_economist() + theme(legend.position = "top", plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#76D7C4")) + 
  labs(title = "Conventional Avocado Price changes \n (2015 - 2016)", x="Months", y="Price", caption = "Blue: Year of 2016, Green: Year of 2017")
second_change_org <- ggplot(structured_data_org) + 
  geom_segment(aes(x=Months, xend=Months, y=Second_year, yend=Third_year), color="#17202A", size = 1) + 
  geom_point(aes(x=Months, y=Second_year), color="#3498DB", size=3) + 
  geom_point(aes(x=Months, y=Third_year), color="#D35400", size=3) + 
  coord_flip() + theme_economist() + theme(legend.position = "top", plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#76D7C4")) + 
  labs(title = "Conventional Avocado Price changes \n (2016 - 2017)", x="Months", y="Price", caption = "Blue: Year of 2016, Green: Year of 2017")

first_dif_org <- structured_data_org %>% select(Months, first_pct, first_cond) %>% 
  ggplot(aes(fill=first_cond)) + geom_bar(stat = "identity", alpha = 0.5, aes(x=Months, y=round(first_pct,2) * 100), color="black") + 
  theme_economist() + theme(axis.text.x = element_text(angle=90), plot.background = element_rect(fill = "#76D7C4"), legend.position = "bottom") + 
  labs(x="Month", y="% Difference") + guides(fill = guide_legend(title = "Diff Status")) + scale_fill_manual(values = c("#C0392B", "#F4D03F"))
second_dif_org <- structured_data_org %>% select(Months, second_pct, second_cond) %>% 
  ggplot(aes(fill=second_cond)) + geom_bar(stat = "identity", alpha = 0.5, aes(x=Months, y=round(second_pct,2) * 100), color="black") + 
  theme_economist() + theme(axis.text.x = element_text(angle=90), plot.background = element_rect(fill = "#76D7C4"), legend.position = "bottom") + 
  labs(x="Month", y="% Difference") + guides(fill = guide_legend(title = "Diff Status")) + scale_fill_manual(values = c("#C0392B", "#F4D03F"))

plot_grid(first_change_org, second_change_org, first_dif_org, second_dif_org, nrow = 2, ncol = 2)

#Distribution of Average Price Avocados by Season
options(repr.plot.width = 8, repr.plot.height = 6)
#Create seasonal column and plot a point line chart by each year
df$season <- ifelse(df$month %in% c("03","04","05"), "Spring",
                    ifelse(df$month %in% c("06","07","08"), "Summer",
                           ifelse(df$month %in% c("09","10","11"), "Fall", "Winter")))
avg3 <- aggregate(AveragePrice~season + type + year, df, mean)

sc <- avg3 %>% filter(type == "conventional", year %in% c("2015", "2016", "2017")) %>% 
  group_by(season, year) %>%
  ggplot(aes(x=season, y=AveragePrice, color=season)) + geom_point(size=3) + 
  geom_segment(aes(x=season, xend=season,
                   y=0, yend=AveragePrice)) +
  coord_flip() + facet_wrap(~year) + theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill="#E8DAEF")) +
  labs(title = "Conventional Avocados by Season", x="Season", y="Average Price") + 
  geom_text(aes(x=season, y=0.01, label=paste0("$", round(AveragePrice, 2))), hjust=-0.5, vjust=-0.5, size=4, colour="black", fontface="italic", angle=360)

sa <- avg3 %>% filter(type == "organic", year %in% c("2015", "2016", "2017")) %>% 
  group_by(season, year) %>%
  ggplot(aes(x=season, y=AveragePrice, color=season)) + geom_point(size=3) + 
  geom_segment(aes(x=season, xend=season,
                   y=0, yend=AveragePrice)) +
  coord_flip() + facet_wrap(~year) + theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill="#E8DAEF")) +
  labs(title = "Organic Avocados by Season", x="Season", y="Average Price") + 
  geom_text(aes(x=season, y=0.01, label=paste0("$", round(AveragePrice, 2))), hjust=-0.5, vjust=-0.5, size=4, colour="black", fontface="italic", angle=360)

plot_grid(sc, sa, nrow = 2)

#Different of Avocados by Month
options(repr.plot.width = 8, repr.plot.height = 7)
avg4 <- aggregate(Total.Volume~Month, df, mean)
df$Volume.Price.Diff <- (df$Total.Volume - df$AveragePrice)
avg5 <- aggregate(Volume.Price.Diff~Month + year + type, df, mean)
avg5 <- avg5 %>% group_by(year, Month) %>% rename(c("Volume.Price.Diff"="avg_diff")) %>% 
  filter(year %in% c("2015", "2016", "2017"))


vol_month <- avg4 %>% rename( c("Total.Volume"="avg.volume")) %>% 
  ggplot(aes(x=Month, y=avg.volume)) +
  geom_bar(stat = "identity", fill = "#D35400", alpha = 0.5) + 
  coord_flip() + theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#E8DAEF"))
price_month <- avg %>% ggplot(aes(x=Month, y=AveragePrice)) + 
  geom_bar(stat = "identity", fill = "#28B463", alpha = 0.5) + 
  coord_flip() + theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#E8DAEF"))
diff_conv <- avg5 %>% filter(type == "conventional") %>% 
  ggplot(aes(x=Month, y=avg_diff, group=year, color=year)) +
  geom_area(aes(fill=year), alpha=0.5) + theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#E8DAEF"), 
        legend.position = "bottom", legend.background = element_rect(fill = "#FDF2E9",
                                                                     size = 0.5,
                                                                     linetype = "solid",
                                                                     colour = "black")) + 
  labs(title = "Differesial of Conventional Avocados by Month")
diff_org <- avg5 %>% filter(type == "organic") %>% 
  ggplot(aes(x=Month, y=avg_diff, group=year, color=year)) +
  geom_area(aes(fill=year), alpha=0.5) + theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#E8DAEF"), 
        legend.position = "bottom", legend.background = element_rect(fill = "#FDF2E9",
                                                                     size = 0.5,
                                                                     linetype = "solid",
                                                                     colour = "black")) + 
  labs(title = "Differesial of Organic Avocados by Month")

pushViewport(viewport(layout = grid.layout(2,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

print(vol_month, vp = vplayout(1,1))
print(price_month, vp = vplayout(1,2))
print(diff_conv, vp = vplayout(2,1))
print(diff_org, vp =vplayout(2,2))

#Market Volume of Avocados
avg1 <- aggregate(Total.Volume~Month + year + region + type, df, mean)
avg1 <- avg1 %>% rename(c("Total.Volume"="avg.vol")) %>% filter(year %in% c("2015", "2016", "2017"), region == c("Northeast","SouthCentral","MidSouth","Southeast","West")) %>% 
  group_by(year, Month, region)

vol_conv <- avg1 %>% filter(type == "conventional") %>% 
  ggplot(aes(x=Month, y=avg.vol)) + 
  geom_point(color="#E74C3C", size=2.5) + geom_line(group=1, color="#2E4053", size=1) + 
  facet_grid(region ~ year) + theme_economist() + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90)) + 
  labs(title = "Market Volume \n Conventional Avocados", x="Month", y="Average Volume")
vol_conv
vol_org <- avg1 %>% filter(type == "organic") %>% 
  ggplot(aes(x=Month, y=avg.vol)) + 
  geom_point(color="#52BE80", size=2.5) + geom_line(group=1, color="#2E4053", size=1) + 
  facet_grid(region ~ year) + theme_economist() + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90)) + 
  labs(title = "Market Volume \n Organic Avocados", x="Month", y="Average Volume")
vol_org

###Time Series Analysis###
##Primelinery Analysis##
#Time Plot
options(repr.plot.width = 10, repr.plot.height = 8)

tsv <- ts(conventional[,2], start = c(2015,1), frequency = 12)
tso <- ts(organic[,2], start = c(2015,1), frequency = 12)

conv.plot <- autoplot(tsv, color= "#935116", size = 1) + theme_economist() +
  labs(title = "Time Plot : Average Price of Avocado Conventional", y = "Average Price")
org.plot <- autoplot(tso, color= "#239B56", size = 1) + theme_economist() +
  labs(title = "Time Plot : Average Price of Avocado Organic", y = "Average Price")

plot_grid(conv.plot, org.plot, nrow = 2)

#Polar Plot of Avocados
polar.conv <- ggseasonplot(tsv, polar = TRUE, color=year) + 
  ylab("Average Avocado Price") +
  ggtitle("Conventional Avocados \n Polar Plot") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#A3E4D7", size = 0.5, linetype = "solid", colour = "black"))
polar.org <- ggseasonplot(tso, polar = TRUE, color=year) + 
  ylab("Average Avocado Price") +
  ggtitle("Organic Avocados \n Polar Plot") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#A3E4D7", size = 0.5, linetype = "solid", colour = "black"))
plot_grid(polar.conv, polar.org, ncol = 2)

#Plot Decomposition of Avocados
options(repr.plot.width = 8, repr.plot.height = 10)
dc <- stl(tsv[,1], s.window = "periodic")
do <- stl(tso[,1], s.window = "periodic")

plot.dc <- autoplot(dc) +
  ggtitle("Decomposition of Avocados Conventional") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
plot.do <- autoplot(do) + 
  ggtitle("Decomposition of Avocados Organic") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

plot_grid(plot.dc, plot.do, ncol = 2)

#Trens of Avocados by Months
options(repr.plot.width = 8, repr.plot.height = 6)

month_conv <- ggsubseriesplot(tsv) +
  labs(title = "Conventional Avocados", x="Month", y="Average Price") + theme_economist() +
  theme(plot.title = element_text(hjust = 0.5))
month_org <- ggsubseriesplot(tso) + 
  labs(title = "Organic Avocados", x="Months", y="Average Price") +
  theme_economist() + theme(plot.title = element_text(hjust = 0.5))
plot_grid(month_conv, month_org, nrow = 2)

#Relationship between trend and autocorrelation of avocados
options(repr.plot.width = 10, repr.plot.height = 6)

trends_conv <- window(conv.price, start=2015)
conv_plot_trends <- autoplot(trends_conv) + theme_economist() +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(x="Year", y="Average Price", title = "Conventional Avocados")
autocorr_conv <- ggpacf(conv.price) + theme_economist() +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(title = "Autocorrelation for \n Conventional Avocados")

trends_org <- window(org.price, start=2015)
org_plot_trends <- autoplot(trends_org) + theme_economist() +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(x="Year", y="Average Price", title = "Organic Avocados")
autocorr_org <- ggpacf(org.price) + theme_economist() +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) + 
  labs(title = "Autocorrelation for \n Organic Avocados")
plot_grid(conv_plot_trends, autocorr_conv, org_plot_trends, autocorr_org, ncol = 2, nrow = 2)

#Avocados Conventional data has a trend
#Take the first difference of data to remove trend
diff.c <- diff(tsv)
time.plot.diffc <- autoplot(diff.c) + theme_economist() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs("Time Plot of Avocados Conventional Differenced Data", y="Average Price")
season.diff.c <- ggseasonplot(diff.c, color=year) + 
  ylab("Average Price") +
  ggtitle("Conventional Avocados \n Seasonality Differenced Plot") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#A3E4D7", size = 0.5, linetype = "solid", colour = "black"))
subseries.diff.c <- ggsubseriesplot(diff.c) +
  labs(title = "Conventional Avocados", x="Month", y="Average Price") + theme_economist() +
  theme(plot.title = element_text(hjust = 0.5))
plot_grid(time.plot.diffc, season.diff.c, subseries.diff.c, nrow = 3)


##Forecasting with Various Methods##

#Using ETS Method#
fit_ets_conv <- ets(tsv) #Residual SD = 0.1893
print(summary(fit_ets_conv)) #MAPE = 11.64674
checkresiduals(fit_ets_conv)

fit_ets_org <- ets(tso) #Residual SD = 0.1533
print(summary(fit_ets_org)) #MAPE = 6.787826
checkresiduals(fit_ets_org)

#Using ARIMA Model#
fit_arima_conv <- auto.arima(tsv, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE) #Residual SD = 0.2313871
print(summary(fit_arima_conv)) #MAPE = 9.331712
checkresiduals(fit_arima_conv)

fit_arima_org <- auto.arima(tso, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE) #Residual SD = 0.1906568
print(summary(fit_arima_org)) #MAPE = 5.739336
checkresiduals(fit_arima_org)


##Forecast with ARIMA Model##
arima_conv <- forecast(fit_arima_conv, h=48)

plot.arima.conv <- autoplot(arima_conv) +
  ggtitle("Forecast of Avocados Conventional") + 
  theme_economist() + theme(plot.title = element_text(hjust = 0.5))
  
arima_org <- forecast(fit_arima_org, h=48)

plot.arima.org <- autoplot(arima_org) +
  ggtitle("Forecast of Avocados Organic") + 
  theme_economist() + theme(plot.title = element_text(hjust = 0.5))

plot_grid(plot.arima.conv, plot.arima.org, nrow = 2)
  
