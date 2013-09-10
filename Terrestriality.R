##---- workspace ----
library(plyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(RColorBrewer)
library(lubridate)
library(rCharts)

##---- data_manip ----
ps <- read.csv("AllPointSamples.csv")
ps <- ps[, -1]
names(ps) <- c("focal_group", 
               "focal_animal", 
               "time_stamp", 
               "seq_num",
               "state_behavior",
               "forest_level",
               "height",
               "centrality",
               "sex",
               "dominance")

ps$time_stamp <- as.POSIXct(ps$time_stamp)
ps$focal_group <- revalue(ps$focal_group, c("XX"="CP"))
ps <- subset(ps, forest_level != "" & sex != "")
ps$is_terrestrial <- "No"
ps[ps$forest_level=="G",]$is_terrestrial <- "Yes"
ps$is_terrestrial <- factor(ps$is_terrestrial)

##---- prop_scans_terr ----
temp <- dcast(ps, focal_animal + sex ~ is_terrestrial, fun.aggregate=length)
names(temp)[3] <- "Not Terrestrial"
names(temp)[4] <- "Terrestrial"
temp <- melt(temp)
temp$variable <- factor(temp$variable,levels=c("Terrestrial","Not Terrestrial"))
temp <- ddply(temp, .(focal_animal, sex), transform, prop=value/sum(value), total=sum(value))
temp$sex <- as.character(temp$sex)
temp[temp$sex=="M",]$sex <- "Male"
temp[temp$sex=="F",]$sex <- "Female"
temp$sex <- factor(temp$sex)

temp_df <- data.frame(sex=NA,mean_value=NA)
temp_df[1,] <- c("Male",mean(subset(temp, sex=="Male" & variable=="Terrestrial")$prop))
temp_df[2,] <- c("Female",mean(subset(temp, sex=="Female" & variable=="Terrestrial")$prop))
temp_df$mean_value <- as.numeric(temp_df$mean_value)
temp_df$sex <- factor(temp_df$sex)

temp <- subset(temp,variable=="Terrestrial")
temp$focal_animal <- reorder(temp$focal_animal, -temp$prop)

##---- scan_plot ----
ggplot(temp,aes(x=focal_animal, y=prop, fill=prop)) + geom_bar(stat="identity", color="black", position="dodge", width=0.8) + theme_classic() + theme(legend.position="bottom", panel.border = element_rect(fill = NA, colour="grey50")) + geom_hline(data=temp_df,aes(yintercept=mean_value),color="grey20", linetype=2, size=0.7, alpha=0.7) + scale_fill_gradientn(guide=FALSE, colours=colorRampPalette(colors=brewer.pal(9,"Blues"))(50)) + geom_text(aes(label=total, y = prop + .005), color = "black", size = 2.5, stat = "identity") + facet_grid(. ~ sex, scales="free", space="free", drop=TRUE) + labs(x="Focal Animal", y="Proportion Of Scans On Ground")

##---- terr_activity_budget ----
t <- subset(ps, state_behavior != "0")
t <- subset(t, year(time_stamp) == 2011)
t$activity <- substr(as.character(t$state_behavior),1,1)
t[t$activity=="D" | t$activity=="U", ]$activity <- "O"
t$activity <- factor(t$activity)
t$activity <- revalue(t$activity, 
                      c("A" = "Self-directed",
                        "E" = "Ingest food",
                        "F" = "Search for food",
                        "O" = "Other",
                        "R" = "Rest",
                        "S" = "Social",
                        "T" = "Travel rapidly",
                        "V" = "Vigilant",
                        "X" = "Extractive forage"))

# ggplot(t, aes(x=is_terrestrial, fill=activity)) + geom_bar(position="fill", color="black") + scale_fill_brewer(palette="Set1") + facet_grid(. ~ focal_group) + theme_bw()
t1 <- dcast(t, focal_group + focal_animal + sex + activity ~ is_terrestrial, fun.aggregate=length)
t2 <- melt(t1)

names(t2) <- c("Focal Group", "Focal Animal", "Sex", "Activity", "Is Terrestrial", "Scans")


##---- activity_plot ---
d7 <- dPlot(
  x = c("Focal Group","Is Terrestrial"),
  y = "Scans",
  groups = "Activity",
  data = t2,
  type = "bar",
  height = 600,
  width = 900
)
d7$yAxis(type = "addPctAxis")
d7$legend(
  x = 0,
  y = 10,
  width = 800,
  height = 20,
  horizontalAlign = "center"
)
# d7
d7$publish('Activites', host = 'gist')

##---- activity_table ----
dt <- dTable(
  t2,
  bScrollInfinite = T,
  bScrollCollapse = T,
  sScrollY = "200px",
  width = "800px"
)
dt







temp <- dcast(ps, focal_group + focal_animal + sex ~ is_terrestrial, fun.aggregate=length)
names(temp)[4] <- "Not Terrestrial"
names(temp)[5] <- "Terrestrial"
temp <- melt(temp)
temp$variable <- factor(temp$variable,levels=c("Terrestrial","Not Terrestrial"))
temp <- ddply(temp, .(focal_group, focal_animal, sex), transform, prop=value/sum(value), total=sum(value))
temp$sex <- as.character(temp$sex)
temp[temp$sex=="M",]$sex <- "Male"
temp[temp$sex=="F",]$sex <- "Female"
temp$sex <- factor(temp$sex)

temp_df2 <- ddply(temp, .(focal_group, sex), summarize, mean(prop))
names(temp_df2)[3] <- "mean_prop"

temp <- subset(temp,variable=="Terrestrial")
temp$focal_animal <- reorder(temp$focal_animal, -temp$prop)

##---- scan_plot2 ----
ggplot(temp,aes(x=focal_animal, y=prop)) + geom_bar(aes(fill=sex),stat="identity", position="dodge", width=0.8) + theme_classic() + theme(legend.position="bottom", panel.border = element_rect(fill = NA, colour="grey50")) + geom_hline(data=temp_df,aes(yintercept=mean_value),color="grey20", linetype=2, size=0.7, alpha=0.7) + geom_text(aes(label=total, y = prop + .005), color = "black", size = 2.5, stat = "identity") + facet_grid(. ~ focal_group, scales="free", space="free", drop=TRUE) + labs(x="Focal Animal", y="Proportion Of Scans On Ground") + scale_fill_brewer(palette="Set2")

ggplot() + 
  geom_bar(data = temp, 
           aes(x = focal_animal, y = prop, fill = sex),
           color = "black",
           stat = "identity", position = "dodge", 
           width = 0.8, alpha = 0.5) + 
  geom_hline(data = temp_df2,
             aes(yintercept = mean_prop, color = sex),
             linetype = 2, size = 0.9) + 
  geom_text(data = temp_df2,
            aes(x = 4,
                label = round(mean_prop, 4), 
                y = mean_prop + .005,
#                 y = 0.08 + as.numeric(sex)*.01,
                color = sex), 
            size = 4) + 
  geom_text(data = temp,
            aes(x = focal_animal,
                label = total, y = prop + .005), 
            color = "black", 
            size = 2.5, 
            stat = "identity") + 
  facet_grid(. ~ focal_group, scales="free", space="free", drop=TRUE) + 
  labs(x="Focal Animal", y="Proportion Of Scans On Ground") + 
  theme_classic() + 
  theme(legend.position = "bottom", 
        panel.border = element_rect(fill = NA, colour = "grey50")) +
  scale_fill_brewer(palette="Set2") + 
  scale_color_brewer(palette="Set2")