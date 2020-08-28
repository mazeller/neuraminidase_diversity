library(reshape2)
library(ggplot2)
library(viridis)

# Load data
clades <- read.csv("year_data.csv", na.strings = 0)
clades[is.na(clades)] <- 0

# Convert to long format
longClades <- melt(clades, id = c("year"))


ggplot(data = longClades, aes(x=year, y=value, color=variable, fill=variable))+
  #geom_smooth(se = FALSE)
  geom_line(size=1.5)
  
  
ggplot(data = longClades, aes(x=year, y=value, color=variable, fill=variable, ymin=0, ymax=value, alpha=0.7))+
  geom_ribbon() + 
  geom_line()

ggplot(data = longClades, aes(x=year, y=value, color=variable, fill=variable))+
  geom_area()

#normalize
normClades <- clades/rowSums(clades[2:11])
normClades[1] <- clades[1]
normLongClades <- melt(normClades, id = c("year"))

ggplot(data = normLongClades, aes(x=year, y=value, color=variable, fill=variable))+
  geom_area(alpha=1, size=.5, colour="white") +
  scale_fill_viridis(discrete = T, option="inferno", name="HA-NA clade pair") + 
  labs(y = "Percent of N2 detection", x = "Year", color="test") +
  scale_x_continuous(limits = c(2009,2018), expand = c(0, 0.2)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
        legend.title = element_text(size=12,),
        legend.text = element_text(size=12),
  )

########################
###State based Graphs###
########################
states <- read.csv("state_data.csv")

#Drop out clades not in top 10
topClades <- list("delta1b-02A","cluster_iva-02B","delta1a-02B","delta2-98B","cluster_ivb-02B","delta1a-02A","2010.1-02B","cluster_iva-02A","2010.1-02A","alpha-02B")
topStates <- list("Iowa","Illinois","North Carolina","Minnesota","Indiana","Nebraska")
states <-states [states$merged_clade %in% topClades,]
states <-states [states$State.Province %in% topStates,]

#State facet
stateList <- dcast(states, State.Province + collection_date ~ merged_clade)
stateItem <- melt(stateList, id = c("collection_date","State.Province"))

ggplot(data = stateItem, aes(x=collection_date, y=value, color=variable, fill=variable))+
  geom_area(alpha=1, size=.5, colour="white") +
  scale_fill_viridis(discrete = T, option="inferno", name="HA-NA clade pair") + 
  labs(y = "Percent of N2 detection", x = "Year", color="test") +
  #scale_x_continuous(limits = c(2009,2018), expand = c(0, 0.2)) +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
        legend.title = element_text(size=12,),
        legend.text = element_text(size=12),
  ) +
  facet_wrap(~State.Province, ncol = 2)
