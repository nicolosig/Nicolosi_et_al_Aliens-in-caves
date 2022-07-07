###############################################################

# Title: Aliens in caves: the global dimension of biological invasions in subterranean ecosystems 
# Authors: Giuseppe Nicolosi, Stefano Mammola, Laura Verbrugge, Marco Isaia 


###############################################################

## ------------------------------------------------------------------------
# 'R script to reproduce the analyses'
## ------------------------------------------------------------------------

# Software: R (v. R 4.1.0) and R studio (v. 1.4.1103)
# Authors: Stefano Mammola & Giuseppe Nicolosi

###############################################################

# clean the workspace -----------------------------------------------------

rm(list=ls())

# Loading R package -------------------------------------------------------

library("dplyr")
library("ggplot2")
library("scatterpie")
library("tidyverse")
library("rworldmap")
library("lme4")
library("dplyr")
library("performance")
library('plyr')
library('hrbrthemes')
library('stringr')
library('plotly')
library('viridis')


# Set seed ----------------------------------------------------------------

set.seed(123)

###############################################################

## Data preparation:

###############################################################

# Loading the Database ----------------------------------------------------

db <- read.csv(file = "Database_Aliens in caves.csv", sep = '\t', dec = ',', header = TRUE, as.is = FALSE)

str(db)
dim(db)

#Selecting unique species/location
db <- db %>% distinct(Location, Species, .keep_all= TRUE)

###############################################################
###############################################################

## Figure 2: number of articles published per year

#-------------------------------------------------------------

db_unique_yrs <- DB %>% distinct(ID, .keep_all = TRUE) #Remove duplicated rows based on ID

df_year_fr <- count(db_unique_yrs$Year) 

colnames(df_year_fr) <- c('Year','Freq')

df_year_fr


Figure_S2 <- ggplot(df_year_fr, aes(x = Year, y = Freq)) + 
             geom_bar(stat = 'identity', fill="#1D6D84", color="black") + 
             scale_y_continuous(breaks = c(0:10),expand = c(0,0),limits = c(0, 10)) +
             scale_x_continuous(breaks=seq(1970,2021,3))+
             xlab("") + ylab("Frequency") + 
  
  theme(
    
             axis.text.x=element_text(angle=45, hjust=1, size = 8,colour="black"),
             axis.text.y=element_text(hjust=1, size = 8,colour="black"),
             axis.title.y = element_text(size=11),
             panel.background = element_blank(),
             panel.border = element_blank(), axis.line = element_line()
    
  )


ggsave (
  "Figure_S2.pdf",
  plot = last_plot(),
  device = pdf(),
  path = 'c:/Users/Utente/Desktop/',
  scale = 1,
  width = 22,
  height = 10,
  units = c("cm"),
  dpi = 300,
  bg = NULL
  
)


###############################################################
###############################################################

## Figure 3: Barplot representing the number of species 
             #within each taxonomic class

#-------------------------------------------------------------

#Selecting unique species/location
db <- db %>% distinct(Species, .keep_all= TRUE)

db$Class <- factor(db$Class,levels = c("Magnoliopsida","Liliopsida","Pinopsida","Insecta","Arachnida","Entognatha","Malacostraca", "Diplopoda",
                                       "Clitellata","Gastropoda","Hexanauplia","Chilopoda","Symphyla","Bivalvia","Rhabditophora",
                                       "Actinopterygii", "Amphibia","Mammalia"))

db$Organism_group <- factor(db$Organism_group,levels = c("plant","invertebrate","vertebrate"))

colours.vector <- c("#AF2F51","#1C6E85","#B86C1E")

Figure_3a <- ggplot(db, aes(Class, fill=Organism_group, group=Organism_group)) + 
             geom_bar(show.legend = FALSE, width = 0.9, color="black", size=0.7) +
             scale_fill_manual(name="Organism_group", 
             labels = levels(db$Organism_group),
             values = setNames(colours.vector, levels(db$Organism_group))) + 
             scale_y_continuous(expand = c(0,00), limits = c(0,70)) + 
             labs(title=NULL, subtitle = NULL,x=NULL, y =NULL)+
  
  theme(
    
             strip.background =element_rect(color="black", fill="grey90", size=1),
             strip.text = element_text(colour = 'black'),
             axis.text.x = element_text(angle=45, hjust=1, color="black", size=13),
             axis.text.y=element_text(color="black", size=rel(1.5)),
             axis.title = element_text(color="black", size=15),
             plot.margin = margin(1,1,1.5,1.2, "cm"),
             panel.background = element_rect(fill = "white", colour = "black", size = 0.5, linetype = "solid")
    
  )

###############################################################
###############################################################

## Figure 4: The number of subterranean alien taxa exchanges 
             #among biogeographic regions and countries. 

#--------------------------------------------------------------

# Country database

db_country <- data.frame(table(db$Location)) ; colnames(db_country) <- c("Country","N")

db_country <- unique(dplyr::left_join(x  = db_country, 
                                      y  = data.frame(Country = db$Location, lon = db$X_location, lat = db$Y_location), 
                                      by = "Country", copy = FALSE))

# Origin

db_origin <- data.frame(table(db$Origin_bioregion)) ; colnames(db_origin) <- c("Country","N")

db_origin <- unique(dplyr::left_join(x  = db_origin, 
                                      y  = data.frame(Country = db$Origin_bioregion, lon = db$X_bioreg, lat = db$Y_bioreg), 
                                      by = "Country", copy = FALSE)) ; colnames(db_origin) <- c("Origin", "N_origin", "lon_origin", "lat_origin")

# Db connecttion

connection <- data.frame(table(db$Origin_bioregion,db$Location)) ; colnames(connection) <- c("Origin","Country","N_connections")

connection <- dplyr::left_join(x  = connection, 
                               y  = db_origin[,c(1,3,4)], 
                               by = "Origin", copy = FALSE)

connection <- dplyr::left_join(x  = connection, 
                               y  = db_country[,c(1,3,4)], 
                               by = "Country", copy = FALSE)

db_connection <- connection[connection$N_connections>0,]

# Colors

palette <- c("blue4","darkorchid4","darkorange2","darkred", "deeppink2", "cyan4","black")
  
# Map

net_map <- rworldmap::getMap(resolution = "high") %>% 
  ggplot() +
  
  geom_polygon(aes(long, lat, group = group), colour = "grey55", fill = "grey75", size = 0.25)+
  
  ylim(-56.8, 90) + 
  xlim(-180, 195) +
  
  geom_point(data = db_origin, 
             aes(x = lon_origin, y = lat_origin, color = Origin, fill = Origin), size = 10, #color = palette,
             alpha = 0.6,
             stroke = 0.8, shape=21) +
  
  geom_curve(data = db_connection,
             aes(x = jitter(lon_origin,0.0001), 
                 y = jitter(lat_origin,0.0001), 
                 xend = jitter(lon, 0.0001), 
                 yend = jitter(lat, 0.0001),  color = Origin, alpha = N_connections),
             curvature = 0.30, 
             lwd = 0.7) +
  
  geom_point(data = db_country, 
             aes(x = lon, y = lat, size = N),
             color = "grey5", stroke = 0.8, shape = 2) +
  
  scale_size(name = "Number of\nalien species", breaks = c(1,20,40,60), range = c(1.5,9)) +
  scale_alpha(name = "Number of\nspecies exchanged", breaks = c(1,10,20), range = c(0.1,0.9)) +
  scale_color_manual(name = "Biogeographic region\nof origin", values = palette) +
  scale_fill_manual(name = "Biogeographic region\nof origin", values = palette) +
  
  ggthemes::theme_map() + 
  
  theme( 
    axis.line=element_blank(),axis.text.x=element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),legend.position="top",
    panel.background=element_rect(fill ="white", colour="white"),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_rect(fill ="white", colour="white")
  )

ggplot2::ggsave("Figure_map.pdf", 
                net_map, 
                device = cairo_pdf,
                units = "cm",
                width = 36,
                height = 15)


###############################################################
###############################################################

#FIGURE 5: (a) The use of introduction pathways (b), mechanisms of impacts (c), 
#and management activities of the 250 alien species present in our database.


#---------------------- Plot_a---------------------------------------------------

path <- do.call(rbind, Map(cbind, db$Organism_group, strsplit(db$Pathway, ";"))) #Pathway contains multiple variables in a single row

colnames(path) <- c("Organism_group", "Pathway")

df_path <- data.frame(path)

df_path$Pathway <- str_trim(df_path$Pathway, side="both")

df_path #db with one variable for each row

df_path$Organism_group <- as.factor(df_path$Organism_group)


Plot_a <- ggplot(df_path, aes(x=Organism_group, y=Pathway, colour=Organism_group)) + 
          geom_count(alpha=0.8) +
          scale_color_manual(values=c("#1D6D84",  "#AF2F50", "#E59E34"))+
          labs( x= "", y = "")+ #elimino testo asse x e y
          scale_size(range=c(0, 25))+
          scale_y_discrete(labels=c("Escape", "Release","Contaminant", "Stowaway","Unknown")) + 
  
  theme(     
    
          axis.line.y = element_line(color="black"),
          axis.text.x = element_blank(),
          axis.text.y=element_text(hjust=1, size = 12,colour="black"),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey90"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = "none",
          legend.title = element_blank(),
          legend.key = element_blank()
  )


#---------------------- Plot_b---------------------------------------------------

mech <- do.call(rbind, Map(cbind, db$Organism_group, strsplit(db$Mechanism, ";")))

colnames(mech) <- c("Organism_group", "Mechanism")

df_mech <- data.frame(mech)

df_mech$Mechanism <- str_trim(df_mech$Mechanism, side="both")


Plot_b <- ggplot(df_mech, aes(x=Organism_group, y=Mechanism, colour=Organism_group)) + 
          geom_count(alpha=0.8) +
          scale_color_manual(values=c("#1D6D84",  "#AF2F50", "#E59E34"),labels = c("Invertebrates", "Plants","Vertebrates"))+
          labs( x= "", y = "")+ 
          guides(color = guide_legend(override.aes = list(size = 7)))+ 
          scale_size(range=c(0, 25))+
          scale_y_discrete(labels=c("Competition", "Disease transmission","Flammability", "Grazing/herbivory/browsing",
                            "Hybridisation", "Interaction with other IAS","Others", 
                            "Parasitism","Poisoning/toxicity","Predation","Rooting/digging","Unknown")) + 
  
  theme(
    
          axis.line.y = element_line(color="black"),
          axis.text.x = element_blank(),
          #axis.text.x=element_text(angle=45, hjust=1, size = 10,colour="black"),
          axis.text.y=element_text(hjust=1, size = 12,colour="black"),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey90"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.key = element_blank()
        )


#---------------------- Plot_c---------------------------------------------------

man <- do.call(rbind, Map(cbind, db$Organism_group, strsplit(db$Management, ";")))

colnames(man) <- c("Organism_group", "Management")

df_man <- data.frame(man)

df_man$Management <- str_trim(df_man$Management, side="both")


Plot_c <- ggplot(df_man, aes(x=Organism_group, y=Management, colour=Organism_group)) + 
          geom_count(alpha=0.8) +
          scale_color_manual(values=c("#1D6D84",  "#AF2F50", "#E59E34"))+
          labs( x= "", y = "")+ 
          scale_size(range=c(0, 25))+
          scale_y_discrete(labels=c("Control", "Eradication","Prevention", "None")) + 
  
theme(
    
          axis.line.y = element_line(color="black"),
          axis.text.x = element_blank(),
          axis.text.y=element_text(hjust=1, size = 12,colour="black"),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey90"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = "none",
          legend.title = element_blank(),
          legend.key = element_blank()
    
  ) 

#---------------- FIGURE_5 (Plot a+b+c) --------------------------------------------


legend_b <- get_legend(Plot_b + theme(legend.position="bottom")) # Pick arbitrarily legend from Plot b

Figure_5 <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1, .2))

Figure_5

ggsave("Figure_5.tiff", path= "C:/Users/Utente/Desktop", width = 16, height = 10.3)



###############################################################
###############################################################

# Figure 6: Effect sizes for the Bernoulli GLMM assessing 
            #the species traits correlating with their probability of 
            #becoming established in subterranean ecosystems

#--------------------------------------------------------------

db <- read.csv(file = "Traits.csv", sep = '\t', dec = ',', header = TRUE, as.is = FALSE)
str(db)
dim(db)

Var <- c("Adaptive_trait","Depigmented", "Eye_regression", "Leg_elong", "Behavior", "Phys_adap","Wing")
pairs.panels(db[,Var])

table(db$Trophic_level)

#db <- db[db$Trophic_level != "parasite",] ; db$Trophic_level <- droplevels(db$Trophic_level)

levels(db$Trophic_level)[c(2,4,6)] <- "Others" ; db$Trophic_level <- droplevels(db$Trophic_level)


m1b <- glmer(Established ~ Adaptive_trait + Wing + Trophic_level  + (1|Class / Order), 
            data = db, family = binomial(link= "cloglog"))

summary(m1b)

pairs(emmeans::emmeans(m1b, ~ Trophic_level), simple=c("Trophic_level"))

Estimates_m1 <- 
  m1b %>% 
  summary %>% 
  magrittr::extract2("coefficients") %>% # extract estimates
  as.data.frame %>% rownames_to_column("Variable") %>% 
  dplyr::filter(!row_number() %in% 1) %>%  #remove intercept
  dplyr::rename(SE = 3, z = 4, p = 5) #rename

# Set variable order and rename
order_var1 <- c("Adaptive traits [yes]",
                "Winged species [yes]",
                "Trophic level [others]",
                "Trophic level [omnivorous]",
                "Trophic level [predators]")

Estimates_m1$Variable <- order_var1 #Rename
Estimates_m1$Variable <- factor(Estimates_m1$Variable, rev(order_var1)) #Sort

sign <- ifelse(Estimates_m1$p > 0.05, "", ifelse(Estimates_m1$Estimate>0.01," *", " **")) #Significance
# col_p <- ifelse(Estimates_m1$p > 0.05, "grey5", ifelse(Estimates_m1$Estimate>0,"orange","blue")) #Significance

# Plot
(plot_model1 <- ggplot2::ggplot(data = Estimates_m1) +
  
  geom_pointrange(aes(x = Variable, 
                      y = Estimate,
                      ymin = Estimate-SE, 
                      ymax = Estimate+SE), col = "grey5", size = 0.5) + 
  
  geom_hline(lty = 3, size = 0.7, col = "grey50", yintercept = 0) +
  
  geom_text(aes(Variable, Estimate),
            label = paste0(round(Estimates_m1$Estimate,2),sign), 
            vjust = -1, size = 3, col = "grey5") +
  
  labs(title = NULL,
       y = expression(paste("Effect size" %+-% "Standard Error")),
       x = NULL)+
    theme_bw() +
    theme(
      axis.text = element_text(size = 12), 
      axis.title = element_text(size = 14),
      axis.line.x = element_line(color="black"), 
      axis.line.y = element_line(color="black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),                                          
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),  
      plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
      plot.title = element_text(size = 15, vjust = 1, hjust = 0)) + 
      theme(axis.text.y  = element_text(colour = "grey5")) + coord_flip()+
      annotate(geom = 'text', x = 1, y = -1.5, size =5,
           label = paste0("R^2 ==",round(as.numeric(performance::r2(m1b)[1]),2)), parse = TRUE))

ggplot2::ggsave("Figure_regression.pdf", 
                plot_model1, 
                device = cairo_pdf,
                units = "cm",
                width = 18,
                height = 10)


####################################################################################
