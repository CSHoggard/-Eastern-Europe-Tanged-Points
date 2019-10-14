### ARTICLE TITLE: All these Fantastic Cultures? Research History and Regionalisation in the Late Palaeolithic Tanged Point Cultures of Eastern Europe ###
### AUTHORS: Livija Ivanovaite, Kamil Swertka, Christian Steven Hoggard, Florian Sauer and Felix Riede ###
### SCRIPT AUTHOR: Christian Steven Hoggard ###
### SCRIPT CONTACT: C.Hoggard@soton.ac.uk / Christianhoggard@gmail.com ###
### LAST EDITED: 14/10/2019 ###

### ABSTRACT ###
### The Late Glacial, that is the period from the first pronounced warming ###
### after the Last Glacial Maximum to the beginning of the Holocene (c. 16,000-11,700 cal BP), is ###
### traditionally viewed as a time when northern Europe was being recolonized and Late Palaeolithic ### 
### cultures diversified. These cultures are characterized by particular artefact types, or the co-occurrence ###
### or specific relative frequencies of these. In north-eastern Europe, numerous cultures have been proposed ###
### on the basis of supposedly different tanged points. This practice of naming new cultural units based on ### 
### these perceived differences has been repeatedly critiqued but robust alternatives have rarely been offered. ### 
### Here, we review the taxonomic landscape of Late Palaeolithic large tanged point cultures in Eastern Europe as ###
### currently envisaged, which leads us to be cautious about the epistemological validity of many of the constituent groups. ### 
### This, in turn, motivates us to investigate the key artefact class, the large tanged point, using geometric ### 
### morphometric methods. Using these methods, we show that distinct groups are difficult to recognize, with major ### 
### implications for our understanding of patterns and processes of culture change in this period in north-eastern Europe ###
### and perhaps elsewhere.

### SYSTEM INFORMATION ###
### R version 3.6.1 (2019-07-05) ###
### Platform: x86_64-w64-mingw32/x64 (64-bit) ###
### Running under: Windows 10 x64 (build 18362) ###

### ATTACHED BASE PACKAGES:
### [1] stats     graphics  grDevices utils     datasets 
### [6] methods   base    

### SCRIPT ###
### STAGE 1: DATA COLLECTION AND PACKAGE INSTALLATION --------------------

setwd()
if(!require("ggtree")) install.packages('ggtree', repos='http://cran.us.r-project.org') ### GGTREE 1.16.6
if(!require("tidyverse")) install.packages('tidyverse', repos='http://cran.us.r-project.org') ### TIDYVERSE 1.2.1
if(!require("Momocs")) install.packages('Momocs', repos='http://cran.us.r-project.org') ### MOMOCS 1.2.9 
if(!require("directlabels")) install.packages('directlabels', repos='http://cran.us.r-project.org') ### DIRECTLABELS 2018.05.22 
if(!require("cowplot")) install.packages('cowplot', repos='http://cran.us.r-project.org') ### COWPLOT 1.0.0
if(!require("readxl")) install.packages('readxl', repos='http://cran.us.r-project.org') ### READXL 1.3.1  
library("ggtree")
library("tidyverse")
library("Momocs")
library("directlabels")
library("cowplot")
library("readxl")

dataset <- import_tps("ivanovaite_et_al_2019.tps", curves = TRUE) ### IMPORT .TPS FILE (ORDERED)
print(dataset)
database <- read_excel(path = "ivanovaite_et_al_2019.xlsx") ### IMPORT .XLSX FILE (ORDERED) #readxl is used for encoding purposes
database$Country <- as.factor(database$Country) ### CONVERT TO FACTOR
database$Archaeological_Unit <- as.factor(database$Archaeological_Unit) ### CONVERT TO FACTOR
View(database)

summary(database$Archaeological_Unit) ### COUNT SUMMARY FOR EACH ARCHAEOLOGICAL UNIT
summary(database$Country) ### COUNT SUMMARY FOR EACH COUNTRY
database$Site ### SITE COUNT AND LIST
database$Country ### COUNTRY COUNT AND LIST

### STAGE 2A: OUTLINE CREATION (ALL EXAMPLES) -----------------------------

outline <- Out(dataset$coo, fac = database) ### CREATION OF THE OUTLINE FILE
outline <- coo_close(outline) ### CLOSE ALL OUTLINES
outline <- coo_center(outline) ### NORMALISATION PROCEDURE: CENTRE ALL OUTLINES
outline <- coo_scale(outline) ### NORMALISATION PROCEDURE: SCALE ALL OUTLINES

stack(outline, title="") ### STACK VISUALISATION: ALL OUTLINES
panel(outline) ### PANEL VISUALISATION: ALL OUTLINES

### STAGE 2B (OPTIONAL): OUTLINE CREATION (INDIVIDUAL GROUP) ---------------

bmpoint   <- filter(outline, Archaeological_Unit %in% c("Baltic Magdalenian")) ### ALL BALTIC MAGDALENIAN EXAMPLES
beepoint  <- filter(outline, Archaeological_Unit %in% c("Bromme (Eastern Europe)")) ### ALL EASTERN EUROPEAN BROMME EXAMPLES
bwepoint  <- filter(outline, Archaeological_Unit %in% c("Bromme (Western Europe)")) ### ALL wESTERN EUROPEAN BROMME EXAMPLES
grepoint  <- filter(outline, Archaeological_Unit %in% c("Grensk")) ### ALL GRENSK EXAMPLES
krapoint  <- filter(outline, Archaeological_Unit %in% c("Krasnosillya")) ### ALL KRASNOSILLYA EXAMPLES
perpoint  <- filter(outline, Archaeological_Unit %in% c("Perstunian")) ### ALL PERSTUNIAN EXAMPLES
pwtapoint <- filter(outline, Archaeological_Unit %in% c("Pitted Ware (Type A)")) ### ALL PITTED WARE EXAMPLES
podpoint  <- filter(outline, Archaeological_Unit %in% c("Podolian")) ### ALL PODOLIAN EXAMPLES
vyspoint  <- filter(outline, Archaeological_Unit %in% c("Vyshegorian")) ### ALL VYSHEGORIAN EXAMPLES
wolkpoint <- filter(outline, Archaeological_Unit %in% c("Wolkushian")) ### ALL WOLKUSHIAN EXAMPLES

bmpoint   <- coo_close(bmpoint)    ### CLOSE OUTLINES (BALTIC MAGDALENIAN)
beepoint  <- coo_close(beepoint)   ### CLOSE OUTLINES (BROMME EASTERN EUROPE)
bWepoint  <- coo_close(bwepoint)   ### CLOSE OUTLINES (BROMME WESTERN EUROPE)
grepoint  <- coo_close(grepoint)   ### CLOSE OUTLINES (GRENSK)
krapoint  <- coo_close(krapoint)   ### CLOSE OUTLINES (KRASNOSILLYA)
perpoint  <- coo_close(perpoint)   ### CLOSE OUTLINES (PERSTUNIAN)
pwtapoint <- coo_close(pwtapoint)  ### CLOSE OUTLINES (PITTED WARE)
podpoint  <- coo_close(podpoint)   ### CLOSE OUTLINES (PODOLIAN)
vyspoint  <- coo_close(vyspoint)   ### CLOSE OUTLINES (VYSHEGORIAN)
wolkpoint <- coo_close(wolkpoint)  ### CLOSE OUTLINES (WOLKUSHIAN)

bmpoint   <- coo_centre(bmpoint)   ### CENTRE OUTLINES (BALTIC MAGDALENIAN)
beepoint  <- coo_centre(beepoint)  ### CENTRE OUTLINES (BROMME EASTERN EUROPE)
bWepoint  <- coo_centre(bwepoint)  ### CENTRE OUTLINES (BROMME WESTERN EUROPE)
grepoint  <- coo_centre(grepoint)  ### CENTRE OUTLINES (GRENSK)
krapoint  <- coo_centre(krapoint)  ### CENTRE OUTLINES (KRASNOSILLYA)
perpoint  <- coo_centre(perpoint)  ### CENTRE OUTLINES (PERSTUNIAN)
pwtapoint <- coo_centre(pwtapoint) ### CENTRE OUTLINES (PITTED WARE)
podpoint  <- coo_centre(podpoint)  ### CENTRE OUTLINES (PODOLIAN)
vyspoint  <- coo_centre(vyspoint)  ### CENTRE OUTLINES (VYSHEGORIAN)
wolkpoint <- coo_centre(wolkpoint) ### CENTRE OUTLINES (WOLKUSHIAN)

bmpoint   <- coo_scale(bmpoint)    ### SCALE OUTLINES (BALTIC MAGDALENIAN)
beepoint  <- coo_scale(beepoint)   ### SCALE OUTLINES (BROMME EASTERN EUROPE)
bWepoint  <- coo_scale(bwepoint)   ### SCALE OUTLINES (BROMME WESTERN EUROPE)
grepoint  <- coo_scale(grepoint)   ### SCALE OUTLINES (GRENSK)
krapoint  <- coo_scale(krapoint)   ### SCALE OUTLINES (KRASNOSILLYA)
perpoint  <- coo_scale(perpoint)   ### SCALE OUTLINES (PERSTUNIAN)
pwtapoint <- coo_scale(pwtapoint)  ### SCALE OUTLINES (PITTED WARE)
podpoint  <- coo_scale(podpoint)   ### SCALE OUTLINES (PODOLIAN)
vyspoint  <- coo_scale(vyspoint)   ### SCALE OUTLINES (VYSHEGORIAN)
wolkpoint <- coo_scale(wolkpoint)  ### SCALE OUTLINES (WOLKUSHIAN)

stack(bmpoint, title = "Baltic Magdalenian", xy.axis = FALSE, centroid = FALSE)
stack(beepoint, title = "Bromme (Eastern Europe)", xy.axis = FALSE, centroid = FALSE)
stack(bwepoint, title = "Bromme (western Europe)", xy.axis = FALSE, centroid = FALSE)
stack(grepoint, title = "Grensk", xy.axis = FALSE, centroid = FALSE)
stack(krapoint, title = "Krasnosillya", xy.axis = FALSE, centroid = FALSE)
stack(perpoint, title = "Perstunian", xy.axis = FALSE, centroid = FALSE)
stack(pwtapoint, title = "Pitted Ware", xy.axis = FALSE, centroid = FALSE)
stack(podpoint, title = "Podolian", xy.axis = FALSE, centroid = FALSE)
stack(vyspoint, title = "Vyshegorian", xy.axis = FALSE, centroid = FALSE)
stack(wolkpoint, title = "Wolkushian", xy.axis = FALSE, centroid = FALSE)

### STAGE 3: ELLIPTIC FOURIER PROCEDURE ----------------------------------

calibrate_harmonicpower_efourier(outline) ### CALIBRATION EXERCISE #1
calibrate_reconstructions_efourier(outline, range = 1:20) ### CALIBRATION EXERCISE #2
calibrate_deviations_efourier(outline) ### CALIBRATION EXERCISE #3

efourieroutline <- efourier(outline, nb.h = 11, smooth.it = 0, norm = TRUE) ### 99% HARMONIC POWER USED
### NOTE: NORMALISATION IS NOT RECOMMENDED HOWEVER ALL ARTEFACTS WERE ROTATED, SCALED AND RESIZED PRIOR THE TRANSFORMATION ###

pcaoutline <- PCA(efourieroutline) ### CREATION OF A PCA FILE

### STAGE 4: EXPLORATORY AND ANALYRICAL PROCEDURE -------------------------

scree(pcaoutline) ### SCREE TIBBLE
scree_plot(pcaoutline) ### SCREE PLOT

figure_4 <- PCcontrib(pcaoutline, nax = 1:3) ### XY SHAPE TRANSFORMATIONS FOR THE FIRST THREE PCS
ggsave2("Figure_4.tiff", plot = last_plot(), scale = 1, width = 184, height = 60, units = "mm")

PCcontrib(pcaoutline, nax = 1:20) ### XY SHAPE TRANSFORMATIONS FOR THE FIRST 20 PCS

pcdata <- as_df(pcaoutline)
row.names(pcdata) <- pcdata$File_Name

figure_5a <- ggplot(pcdata, aes(PC1, PC2, color = Archaeological_Unit)) + geom_point(size = 1) + labs(color = "Archaeological Unit", x = "Principal Component 1 (57.7%)", y = "Principal Component 2 (16.4%)") + stat_ellipse(type = "norm", linetype = 1) + scale_colour_manual(values = c("#EEA236", "#D43F3A", "#46B8DA", "#357EBD", "#5BB85C", "#9632B8", "#767676", "#D8099C", "#8F3931", "#340E20")) + theme_bw() + theme(legend.title = element_blank(), legend.position = "none", axis.title=element_text(size=14, face="bold")) + geom_dl(aes(label = Archaeological_Unit), method = "smart.grid")
figure_5b <- ggplot(pcdata, aes(PC2, PC3, color = Archaeological_Unit)) + geom_point(size = 1) + labs(color = "Archaeological Unit", x = "Principal Component 2 (16.4%)", y = "Principal Component 3 (8.1%)") + stat_ellipse(type = "norm", linetype = 1) + scale_colour_manual(values = c("#EEA236", "#D43F3A", "#46B8DA", "#357EBD", "#5BB85C", "#9632B8", "#767676", "#D8099C", "#8F3931", "#340E20")) + theme_bw() + theme(legend.title = element_blank(), legend.position = "none", axis.title=element_text(size=14, face="bold")) + geom_dl(aes(label = Archaeological_Unit), method = "smart.grid")
figure_5c <- ggplot(pcdata, aes(PC1, PC3, color = Archaeological_Unit)) + geom_point(size = 1) + labs(color = "Archaeological Unit", x = "Principal Component 1 (57.7%)", y = "Principal Component 3 (8.1%)") + stat_ellipse(type = "norm", linetype = 1) + scale_colour_manual(values = c("#EEA236", "#D43F3A", "#46B8DA", "#357EBD", "#5BB85C", "#9632B8", "#767676", "#D8099C", "#8F3931", "#340E20")) + theme_bw() + theme(legend.title = element_blank(), legend.position = "none", axis.title=element_text(size=14, face="bold")) + geom_dl(aes(label = Archaeological_Unit), method = "smart.grid")
figure_5  <- plot_grid(figure_5a, figure_5b, figure_5c, ncol = 1, align = 'h')
ggsave2("Figure_5.tiff", plot = figure_5, scale = 1.1, width = 184, height = 290, units = "mm", dpi = 400)

MANOVA(pcaoutline, "Archaeological_Unit") #MANOVA OF THE FIRST 21 PRINCIPAL COMPONENT SCORES
MANOVA_PW(pcaoutline, "Archaeological_Unit") #PAIRWISE ANALYSES OF THE FIRST 21 PRINCIPAL COMPONENT SCORES

cluster <- CLUST(pcaoutline, dist_method = "euclidean", hclust_method = "complete", retain = 1:20, tip_labels = "File_Name")
clusterdata <- select(pcdata, File_Name, Site, Archaeological_Unit) %>% droplevels()

figure_6 <- ggtree(cluster, layout="circular") %<+% clusterdata + geom_tippoint(aes(colour=Archaeological_Unit)) + geom_tiplab2(aes(label = Site, colour = Archaeological_Unit, align = TRUE), offset = 0.005, size = 2.5) + labs(colour = "Archaeological Unit") + scale_colour_manual(values = c("#EEA236", "#D43F3A", "#46B8DA", "#357EBD", "#5BB85C", "#9632B8", "#767676", "#D8099C", "#8F3931", "#340E20")) + guides(col = guide_legend(ncol = 3, byrow = TRUE)) + theme(legend.position = "bottom")
ggsave2("Figure_6.tiff", plot = figure_6, scale = 1.2, width = 184, height = 184, units = "mm", dpi = 500)
