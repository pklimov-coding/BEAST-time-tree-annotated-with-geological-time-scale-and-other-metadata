library(ggtree)
library(treeio)
library(tidyverse)
library(deeptime)
library(readxl)
library(cowplot)

#This R script annotates  BEAST trees calibrated with absolute time (eg., input/0-Acariformes.BEAST2.7.runs1-6.tree.TA.tre) with various metadata: 
#(1) Geological time scale (R package deeptime)
#(2) Temperature and CO2 levels on Earth (from Royer et al. 2004: input/All_palaeotemps_CO2_temperature.xlsx). For convenience, a summary of these data is saved in this file: input/Temp-summary-from-Royer-et-al-2004.xlsx.
#(3) Major events on Earth (see Supplementary Table 1 for references)  defined in the file input/paleoEvents.xlsx. Paleoevents are visualized as:
  #3.1. Shaded boxes (data are defined the worksheet "area")
  #3.2. Vertical lines  (worksheet "line"))
  #3.3. Double vertical lines (worksheet "Rhynie", used to show the Rhynie chert dating range).
#(4) Fossil calibration points (worksheet "fossil")

#Script Author: Qixin He (Purdue University, USA)

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path) #sets the working dir in Rstudio 
setwd(script_dir) #OR define the working dir manually

inputTree <- "input/0-Acariformes.BEAST2.7.runs1-8.burnin10.720008.trees.TA.tre" #must be calibrated with absolute time
annot_tree <- read.beast(inputTree)
annot_tree@phylo$tip.label <- str_replace_all(annot_tree@phylo$tip.label,"_"," ") # change the label to replace all the "_" to spaces
RootTime <- max(annot_tree@data$height_median) # get root time

# get paleo data
paleoTempData="input/Temp-summary-from-Royer-et-al-2004.xlsx"
paleodata = read_excel(paleoTempData,sheet = 1,range = "A4:J56",na = "",col_names = names(read_excel(paleoTempData,sheet = 1,range = "A1:J1",na = "")))

# convert the data to a tidy format for plotting
paleodata_long<- paleodata%>%select(`Age (Ma)`,`GEOCARB III + Horita`,`CO2 proxies + Horita`,`GEOCARB III`,`GEOCARB III + Horita + W`)%>%pivot_longer(cols=c(`GEOCARB III + Horita`,`CO2 proxies + Horita`),names_to="measure")
paleodata_long$`GEOCARB III`[paleodata_long$measure=="CO2 proxies + Horita"]<-NA
paleodata_long$`GEOCARB III + Horita + W`[paleodata_long$measure=="CO2 proxies + Horita"]<-NA

# get important paleo events
paleoEvents="input/paleoEvents.xlsx"
paleo_area <- read_excel(paleoEvents,sheet = "area");paleo_area$y<-0;paleo_area$x<-0
paleo_line <- read_excel(paleoEvents,sheet = "line");paleo_line$y<-0;paleo_line$x<-0
paleo_rhynie <- read_excel(paleoEvents,sheet = "Rhynie");paleo_rhynie$y<-0;paleo_rhynie$x<-0
fossils <- read_excel(paleoEvents,sheet = "fossil") #Fossil Calibration Points 

# Plotting the tree
pg<-ggtree(annot_tree,ladderize=T,right=T)

#IMPORTANT. Use this tree to get node numbers for fossil calibration points. The node numbers then should to be entered to the file paleoEvents.xlsx (worksheet "fossil"). Re-run this code once this the paleoEvents.xlsx file has been updated
pg.node.labels<-pg+geom_text2(aes(label=node), hjust=-0.3)+geom_tiplab(size=3,hjust=-0.19) + coord_cartesian(clip = 'off') + theme_tree2(plot.margin=margin(6, 120, 6, 6)) 
pg.node.labels;ggsave("Time_Tree_node.labels.pdf",pg.node.labels,width=10,height=12)

pg+geom_tiplab(size=3,hjust=0) 
p1<-pg %<+% fossils +
  geom_rect(data=paleo_area,aes(xmin=-TimeHigh,xmax=-TimeLow,ymax=Inf,ymin=-Inf),alpha=0.1)+
  geom_text(data=paleo_area[5:6,],aes(x=-TimeHigh,y=0,label=Event),size=3,angle=c(0),hjust = 0)+
  geom_text(data=paleo_area[1:4,],aes(x=-Mid,y=0,label=Event),size=3,angle=c(90),hjust = 0)+
  geom_vline(data=paleo_line,aes(xintercept=-TimeLow),alpha=0.8,linetype=2,linewidth=0.3,color="grey")+
  geom_text(data=paleo_line[1,],aes(x=-TimeLow,y=Ntip(annot_tree)+1,label=Event),size=3)+
  geom_text(data=paleo_line[2:3,],aes(x=-TimeLow,y=Ntip(annot_tree)+1,label=Event),size=3,hjust=0.2)+
  geom_vline(data=paleo_rhynie,aes(xintercept=-TimeLow),linetype=2,linewidth=0.3,col = "#be792a")+
  geom_text(data=paleo_rhynie[2,],aes(x=-TimeLow,y=Ntip(annot_tree),label=Event),size=3,hjust=-0.1)+
    geom_tiplab(size=3,hjust=0)+
  geom_range("height_0.95_HPD", color='blue', size=1, alpha=.3,center="height_median") + 
  geom_nodelab(aes( label=round(posterior, 2)), hjust=1.4,vjust=-0.5, size=2) +
  geom_segment(aes(x=-high,xend=-low,yend=y),color='red', linewidth=1, alpha=.3)+
  geom_nodepoint(aes(x=-age-3),color='steelblue',size=5) + 
  geom_nodelab(aes(x=-age-3,label=as.character(mark)),color="white",hjust=0.5)+
  geom_tippoint(aes(x=-age-3),color='steelblue',size=5) + 
  geom_tiplab(aes(x=-age-3, label=mark),color="white",hjust=0.5)+
  theme_tree2()+
  coord_geo(
    xlim = c(-530, 0), ylim = c(-2, Ntip(annot_tree)+2),
    pos = list("top"), 
    dat = list("periods"), abbrv =list(F),
    rot = list(0),
    clip="off",
    height = unit(1, "line"),
    size = list(2), neg = T, center_end_labels = TRUE
  )
p3<-revts(p1)+
  geom_cladelab(node=5, label="Major soil lineage\n(Oribatida,10,312 spp)", 
                align=TRUE, angle=270,offset=120,offset.text=10,hjust='center',fontsize=3,fontface="bold")+
  geom_cladelab(node=85, label="Major plant-feeding lineage\n(Eriophyoidea,4,400 spp)", 
                align=TRUE, angle=270,offset=120,offset.text=15,hjust='center',fontsize=3,fontface="bold")+
  
  theme(axis.line = element_blank(),axis.text.x=element_blank(),
                    axis.ticks.x=element_blank(),
                    plot.margin = margin(t=0, r=10, b=0, l=10))

p3
p3$data
labelDat<-data.frame(x=200,y=-0.9,label="1960-1990 average",measure="CO2 proxies + Horita")

p2<-ggplot()+
  geom_rect(data=paleo_area,aes(xmin=TimeHigh,xmax=TimeLow,ymax=Inf,ymin=-Inf),alpha=0.1)+
  geom_vline(data=paleo_line,aes(xintercept=TimeLow),alpha=0.8,linetype=2,linewidth=0.3,color="grey")+
  geom_vline(data=paleo_rhynie,aes(xintercept=TimeLow),linetype=2,linewidth=0.3,col = "#be792a")+
  geom_hline(yintercept = 0)+
  geom_ribbon(data=paleodata_long, aes(x=`Age (Ma)`,
                                       ymin = `GEOCARB III`,ymax=`GEOCARB III + Horita + W`,group=measure),
              fill="#67a9cf",alpha=0.5)+
  geom_line(data=paleodata_long, aes(x=`Age (Ma)`,y=value,col=measure))+
  geom_text(data=labelDat,aes(x=x,y=y,label=label))+
  scale_color_manual(values=c("#ef8a62","#67a9cf"),
                     name=expression(atop("Temperature and C" * O[2], "levels on Earth")),
                     labels=c(expression(Delta *  "C" * O[2] * " from proxies"),
                                         expression(Delta *  degree * "C")
                              ))+
  xlab("")+ylab("")+
  coord_geo(
    xlim = c(530, 0), ylim = c(-2.5, 7.5),
    pos = list("bottom", "bottom"), 
    dat = list("epochs","periods"), abbrv =list(F,F),
    rot = list(90,0),
    clip="off",
    height=list(unit(2.5,"line"),unit(1,"line")),
    size = list(1.8, 2.3), neg = F, center_end_labels = TRUE
  )+
  scale_x_reverse(name="Age (Ma)")+
  #theme_cowplot(font_size=10)+
  theme_classic()+
theme(legend.text.align = 0,legend.title.align = 0,
plot.margin = margin(t=0))
  
p2
library(patchwork)

fig1<-p3/p2+plot_layout(heights = c(5,1))
fig1
ggsave("Time_Tree_Annotated.pdf",fig1,width=10,height=12)
