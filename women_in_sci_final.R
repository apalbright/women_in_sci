#Alex Albright (thelittledataset.com & @AllbriteAllday)
#Updated May 23, 2016
#Code for visuals in "Where My Girls At? (In The Sciences)"
#Using ggplot2 and ggrepel from devtools 2/24/16

#Load libraries
library(ggplot2);library(ggrepel);library(ggthemes);library(plyr);library(reshape);library(grid);library(scales);library(RColorBrewer);library(gridExtra)

#Define theme for all visuals - thanks to Max Woolf (@minimaxir) for tips from his blog on how to format all this
my_theme <- function() {

  # Define colors for the chart
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[4]
  color.panel = palette[3]
  color.axis.text = palette[9]
  color.axis.title = palette[9]
  color.title = palette[9]

  # Create basic construction of chart
  theme_bw(base_size=9, base_family="Georgia") + 

  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill=color.panel, color=color.background)) +
  theme(plot.background=element_rect(fill=color.background, color=color.background)) +
  theme(panel.border=element_rect(color=color.background)) +

  # Format grid
  theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.ticks=element_blank()) +

  # Format legend
  theme(legend.position="bottom") +
  theme(legend.background = element_rect(fill=color.panel)) +
  theme(legend.text = element_text(size=7,color=color.axis.title)) + 
  theme(legend.title = element_text(size=7,face="bold", color=color.axis.title)) + 
  
  #Format facet labels
  theme(strip.text.x = element_text(size = 6, face="bold"))+

  # Format title and axes labels these and tick marks
  theme(plot.title=element_text(color=color.title, size=18, vjust=0.5, hjust=0, face="bold")) +
  theme(axis.text.x=element_text(size=6,color=color.axis.text)) +
  theme(axis.text.y=element_text(size=6,color=color.axis.text)) +
  theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=-1, face="italic")) +
  theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.8, face="italic")) +

  #Format title and facet_wrap title
  theme(strip.text = element_text(size=6.5), plot.title = element_text(size = 14, face = "bold", colour = "black", vjust = 1, hjust=0.5))+
    
  # Plot margins
  theme(plot.margin = unit(c(.2, .2, .2, .2), "cm"))
}

#Import and format datasets
phd_fem <- read.csv('sci_phd_female.csv', check.names=F)
undergrad_fem <- read.csv('sci_undergrad_female.csv', check.names=F)

shaping <- function(dataset){dataset<-reshape(dataset, 
                                     varying = c("2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012"), 
                                     v.names = "count",
                                     timevar = "year", 
                                     times = c("2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012"), 
                                     direction = "long")
  dataset<-dataset[order(dataset$id),]
}

phd_fem<-shaping(phd_fem)
phd_fem<-rename(phd_fem, c("count"="phd_fem"))
undergrad_fem<-shaping(undergrad_fem)
undergrad_fem<-rename(undergrad_fem, c("count"="undergrad_fem"))

all<- merge(undergrad_fem, phd_fem, by=c("Field","year"))
all_full<- all
ids <- names(all) %in% c("id", "id.x", "id.y")
all<-all[!ids]
all_full<-all_full[!ids]
all_full1<-all_full[which(all_full$year==2012), ]
  
all1<-reshape(all, 
            varying = c("phd_fem","undergrad_fem"), 
            v.names = "perc",
            timevar = "perc_type", 
            times = c("Doctoral","Undergraduate"), 
            direction = "long")
all1<-all1[order(all1$id),]

all1 <- ddply(all1, .(Field), mutate, perc_change = (((perc[year == 2012] - perc[year == 2002]) / perc[year == 2002]) * 100))
doc<-subset(all1, year == 2007 & perc_type=="Doctoral")
under<-subset(all1, year == 2007 & perc_type=="Undergraduate")
pt2002u<-subset(all1, year == 2002 & perc_type=="Undergraduate")
pt2012u<-subset(all1, year == 2012 & perc_type=="Undergraduate")
pt2002d<-subset(all1, year == 2002 & perc_type=="Doctoral")
pt2012d<-subset(all1, year == 2012 & perc_type=="Doctoral")                   

#Part I: Compare 2012 % PhD Females and % Undergrad Females
sizecat <- read.csv('size_cat1.csv', check.names=F)
all_full1<- merge(all_full1, sizecat, by=c("Field"))

#Make first visual for the 2012 comparisons!                                                   
fem2012<-ggplot(data=all_full1, aes(x=undergrad_fem, y=phd_fem)) + 
  my_theme()+ theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))+  
  geom_point(aes(color=factor(Subdomain)), size=3) +
  geom_hline(aes(yintercept=0.5), colour="grey25")+
  geom_vline(aes(xintercept=0.5), colour="grey25")+
  geom_label_repel(aes(label=all_full1$Field, fill=factor(Subdomain)), label.size=0.1, family = "Georgia", size=2.5, show.legend = F)+
  scale_colour_brewer(palette = "Set2", guide_legend(title="Parent Science Category"))+
  scale_fill_brewer(palette = "Set2", guide_legend(title="Parent Science Category"))+
  guides(colour=guide_legend(title.position="top", 
                                     title.hjust =0.5))+
  labs(x="\nPercentage of Female Undergraduate Degree Earners", y="Percentage of Female PhD Earners\n")+
  scale_x_continuous(limits=c(0,.8), breaks=c(0,.2,.4,.5,.6,.8), labels = percent_format())+
  scale_y_continuous(limits=c(0,.8), breaks=c(0,.2,.4,.5,.6,.8), labels = percent_format())+
  theme(axis.text.x = element_text(face=c("plain","plain","plain","bold","plain")))+
  theme(axis.text.y = element_text(face=c("plain","plain","plain","bold","plain","plain")))+
  ggtitle(expression(atop(bold("Where My Girls At? (In the Sciences)"), 
                          atop(italic("Part I: Comparing Percentages of Women in the Sciences at the Undergraduate & Doctoral Levels"), 
                               atop(italic("Source: NSF Data on Degrees Awarded, 2012; Created by: Alex Albright (thelittledataset.com & @AllbriteAllday)")),""))))

#Part II: Look at 10 year trends! For both % females undergrad and grad
pal1<- c("#1d91c0", "#fc4e2a")

trends_fem<-ggplot(data=all1, aes(x=year, y=perc, group=perc_type, color=perc_type)) + 
  scale_color_manual(values = pal1, guide_legend(title="Level of Education")) +
  geom_line(size=1, alpha=0.75)+
  my_theme()+
  geom_point(data=pt2002u)+ geom_text(data=pt2002u, aes(label=sprintf('%0.01f%%', perc*100), x=year), size=2, vjust=-3,  hjust=0, fontface = 'bold', family = 'Georgia',show.legend = F)+
  geom_point(data=pt2002d,)+ geom_text(data=pt2002d, aes(label=sprintf('%0.01f%%', perc*100), x=year), size=2, vjust=4, hjust=0, fontface = 'bold', family = 'Georgia',show.legend = F)+
  geom_point(data=pt2012u)+ geom_text(data=pt2012u, aes(label=sprintf('%0.01f%%', perc*100), x=year), size=2, vjust=-3,  hjust=1, fontface = 'bold', family = 'Georgia', show.legend = F)+
  geom_point(data=pt2012d)+ geom_text(data=pt2012d, aes(label=sprintf('%0.01f%%', perc*100), x=year), size=2, vjust=4, hjust=1, fontface = 'bold', family = 'Georgia',show.legend = F)+
  geom_text(data=doc, aes(label=sprintf('%+0.01f%%', perc_change), x=year, y=0.92), size=2.5, fontface = 'bold', family = 'Georgia', show.legend = F)+
  geom_text(data=under, aes(label=sprintf('%+0.01f%%', perc_change), x=year, y=1), size=2.5, fontface = 'bold', family = 'Georgia', show.legend = F)+
  facet_wrap(~Field, ncol=6)+
  labs(x="\nNote: Female representation for a given education level and discipline is defined as the percentage of female degree earners in that category.\nThe percentage changes are calculated from the exact 2002 and 2012 percentage values, rather than the rounded values herein presented.", y="")+
  scale_x_discrete(breaks = seq(2002, 2012, by = 2))+
  scale_y_continuous(limits=c(0,1), labels = percent_format())+
  ggtitle(expression(atop(bold("Where My Girls At? (In the Sciences)"), 
                          atop(italic("Part II: A Decade of Female Representation in the Sciences"), 
                               atop(italic("Source: NSF Data on Degrees Awarded, 2002-2012; Created by: Alex Albright (thelittledataset.com & @AllbriteAllday)")),""))))   
                               
#Part III: Compare the 10 year growth percentages both % females undergrad and grad
#Scatter plot with X % growth undergrad, % growth graduate 
under$perc_change_under<-under$perc_change/100
doc$perc_change_doc<-doc$perc_change/100
doc<-doc[c("perc_change_doc", "Field")]
under<-under[c("perc_change_under", "Field")]
growth<-merge(doc,under,by="Field") 
growth<- merge(growth, sizecat, by="Field") 

change<-ggplot(data=growth, aes(x=perc_change_under, y=perc_change_doc)) + 
  geom_hline(aes(yintercept=0), colour="grey25")+
  geom_vline(aes(xintercept=0), colour="grey25")+
  my_theme()+ theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))+  
  geom_point(aes(color=factor(Subdomain)), size=3) +
  geom_label_repel(aes(label=all_full1$Field, fill=factor(Subdomain)), label.size=0.1, family = "Georgia", size=2.5, show.legend = F)+
  scale_colour_brewer(palette = "Set2", guide_legend(title="Parent Science Category"))+
  scale_fill_brewer(palette = "Set2", guide_legend(title="Parent Science Category"))+
  theme(axis.text.x = element_text(face=c("plain","plain","plain","plain","bold","plain","plain","plain")))+
  theme(axis.text.y = element_text(face=c("plain","bold","plain","plain","plain","plain","plain")))+
  guides(colour=guide_legend(title.position="top", 
                                     title.hjust =0.5))+
  labs(x="\nPercentage Change in Female Representation Within Undergraduate Disciplines\nFemale representation for a given education level and discipline is defined as the percentage of female degree earners in that category.", y="Percentage Change in Female Representation Within PhD Disciplines\n")+
  scale_x_continuous(limits=c(-.4,.3), breaks=c(-.4,-.3,-.2,-.1,0,.1,.2,.3), labels = percent_format())+
  scale_y_continuous(limits=c(-.15,.75), breaks=c(-.15,0,.15,.3,.45,.6,.75), labels = percent_format())+
  ggtitle(expression(atop(bold("Where My Girls At? (In the Sciences)"), 
                          atop(italic("Part III: Comparing the Percentage Changes in Female Representation Over the Decade"), 
                               atop(italic("Source: NSF Data on Degrees Awarded, 2002-2012; Created by: Alex Albright (thelittledataset.com & @AllbriteAllday)")),""))))                                            