library(reshape2)
library(ggplot2)

# Load data
clades <- read.csv("year_data.csv", na.strings = 0)
clades[is.na(clades)] <- 0

# Convert to long format
longClades <- melt(clades, id = c("year"))


ggplot(data = longClades, aes(x=year, y=value, color=variable, fill=variable))+
  geom_smooth()
  
  
ggplot(data = longClades, aes(x=year, y=value, color=variable, fill=variable, ymin=0, ymax=value, alpha=0.7))+
  geom_ribbon() + 
  geom_line()
