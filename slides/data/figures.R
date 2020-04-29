#   R code for:
#   Visual Data Exploration for Balance Quantification in Real-Time During Exergaming
#
#   Venustiano Soancatl1* (1), Jasper J. van de Gronde (1), Claudine C.J.C. Lamoth (3), 
#   Mike van Diest (3,4), Natasha M. Maurits (2), Jos B.T.M. Roerdink (1)
#
#   1 Johann Bernoulli Institute for Mathematics and Computer Science, University of Groningen, Groningen, The Netherlands
#   2 Department of Neurology, University Medical Center Groningen, University of Groningen, Groningen, The Netherlands
#   3 Center of Human Movement Sciences, University Medical Center Groningen, University of Groningen, Groningen, The Netherlands
#   4 INCAS3, Assen, The Netherlands#
#
#   This file was produced by:
#   Venustiano Soancatl Aguilar (v.soancatl.aguilar@rug.nl)
#
#
######################################################################
#    
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed WITHOUT ANY WARRANTY.  See the
#    GNU General Public License.
#
######################################################################
#
#    The R code below was tested on
#    R version 3.2.5 (2016-04-14)
#    Attached packages: Hmisc_3.17-4 scales_0.4.0 overlap_0.2.6 tikzDevice_0.10-1 
#                       ggplot2_2.1.0     data.table_1.9.6 
#    Platform: x86_64-pc-linux-gnu (64-bit)
#    Running under: Ubuntu precise (12.04.5 LTS)
#    June 2016

library(data.table)
library(ggplot2)
library(tikzDevice)

tableRes <-read.table("results.txt")
tableRes<-as.data.table(tableRes)

tableRes[,Decade:=factor(Decade)]

str(tableRes)

#################################################################################################################
## balance map (Figure 5)
#################################################################################################################

normalize2 <-function(x)
{
  return((x-min(x))/(max(x)-min(x)))
}

columns <- c("medSpeed","medLsd51","medLsdD51","medLrms51","medLrmslD51",
             "medLcov51","medLcovD51","medTI51","medTIms51","medK","meanK")

normTable <- tableRes[,.(iSubj,trial,Age,Decade,
                         medSpeed,medLsd51,medLsdD51,medLrms51,medLrmslD51,
                         medLcov51,medLcovD51,medTI51,medTIms51,medK,meanK)]
normTable[,group:=1]

normTable <- normTable[,lapply(.SD,normalize2),by=.(group),.SDcols = columns]

normTable<- cbind(tableRes[,.(iSubj,trial,Age)],normTable)

normTable[,group:=NULL]

# reordering rows by age
setkey(normTable,Age,iSubj,trial)

# years of participants
anios <- normTable[,Age[1],by=.(iSubj)][,V1]

# Temporal table to insert white columns between trials per participant
tmp <- data.table(iSubj = normTable[,unique(iSubj)],trial = rep(11,40),Age = anios,     
                  medSpeed=rep(NA,40), medK=rep(NA,40),meanK=rep(NA,40),medTIms51=rep(NA,40),
                  medTI51=rep(NA,40), medLsd51=rep(NA,40),medLrms51=rep(NA,40),medLcov51=rep(NA,40),
                  medLsdD51=rep(NA,40),medLrmslD51=rep(NA,40),medLcovD51=rep(NA,40)
)

normTable <- rbind(normTable,tmp)                  

# Reordering again
setkey(normTable,Age,iSubj,trial)

# Indexing the trials
normTable[,x:=seq_len(.N)]

# Long format
longNormTable <- melt(normTable,id.vars=c("Age","iSubj","trial","x"),
                      variable.name="BalanceMeasures",
                      value.name="value")

BSColors <- colorRampPalette(c("#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26","#a50f15"))

# plot
balanceMap <- ggplot(longNormTable, aes(x = x, y = BalanceMeasures,fill=value)) +
  geom_raster() +
  scale_fill_gradientn(colours=BSColors(255),name=paste("Normalized measures: "))+
  scale_x_discrete(name = paste("Younger (20+)",
                                "$\\qquad\\qquad\\qquad\\qquad|\\qquad\\qquad\\qquad\\qquad$",
                                "Older (61+)   ",
                                "\nTrials per participant ordered by age"),
                   expand=c(0,0),
                   breaks = seq(5,440,11),labels = anios) +
  scale_y_discrete(name = "Balance measures", expand=c(0,0),
                   breaks =c("medSpeed","medK","meanK","medTIms51","medTI51","medLsd51",
                             "medLrms51","medLcov51","medLsdD51","medLrmslD51","medLcovD51"),
                   labels = c(paste("$Speed$"),paste("$\\kappa$"),paste("$\\overline\\kappa$"),
                              "$I'$","$I$","$SD$","$RMS$","$CoV$","$SD'$","$RMS'$","$CoV'$"))+
  theme(panel.background = element_blank(),legend.position="top",
        legend.key.width = unit(1.5,"cm"),
        axis.title=element_text(face="bold")
  )+
  geom_tile(data = subset(longNormTable,  is.na(value)),fill= "#ffffff") 
balanceMap

# uncomment to create a tikz tex file
# tikz("balanceMapPlos.tex",standAlone = TRUE, width=8, height=4)
#   balanceMap
# dev.off()


#################################################################################################################
# Overlapping violin plots (Figure 6)
#################################################################################################################
library(scales)
#long format
longMeasures <- melt(tableRes, id.vars = c("iSubj","trial","Age","Decade"),
                     variable.name="measures",value.name="values")

# Older and younger (2,1)
longMeasures[,grupo:=ifelse(Age>60,2,1)]

# Standardize measures
longMeasures[,normValues:=(values-mean(values))/sd(values),by=.(measures)]

#rescaling to meet the overlap package requirements
longMeasures[,rescValues := rescale( normValues, to = c(0,2*pi))]

# ordering by group
setkey(longMeasures,grupo)
# Younger data
ytable <- longMeasures[.(1),.(iSubj,grupo,measures,normValues,rescValues)]
# ordering by measure
setkey(ytable,measures)
# Older data
otable <- longMeasures[.(2),.(iSubj,grupo,measures,normValues,rescValues)]
# ordering by measure
setkey(otable,measures)

# Renaming older columns to bind older and younger columns
setnames(otable,c("iSubj","normValues","rescValues"),c("iSubjO","normValuesO","rescValuesO"))

# Binding younger and older columns
ovelapTab <- cbind(ytable,otable[,.(iSubjO,normValuesO,rescValuesO)])

# estimating overlap by measure
# according to https://cran.r-project.org/web/packages/overlap/vignettes/overlap.pdf
# Use the estimator Dhat4 if the smaller sample has more than 75 observations "2".
library(overlap)
measureOverlaps <- ovelapTab[,.(mOvlp = overlapEst(rescValues, rescValuesO)[2]),by=measures]

# y coordinate for labels
measureOverlaps[,y:=rep(5.9,11)]
measureOverlaps[,grupo:=1]

# measures oredered by area of overlap
setkey(measureOverlaps,mOvlp)
#measureOverlaps

# Plot limits
limites <- longMeasures[,range(normValues)] + c(0,1.6)
measureOverlaps[,y:=rep(limites[2]-0.2,11)]

# Wilxox test
mwUtest <- longMeasures[,.(statistic=wilcox.test(values~grupo)$statistic,
                           pvalue=wilcox.test(values~grupo)$p.value),
                        by=.(measures)]

# Additional data for the labesl 
mwUtest[,y:=limites[2]-0.6]
mwUtest[,yp:=limites[2]-1.0]
mwUtest[,grupo:=1]
mwUtest[pvalue>0.01]
mwUtest[,padjusted := p.adjust(pvalue,"bonferroni")]
mwUtest[padjusted>0.01]
mwUtest[,ptext:=ifelse(pvalue<0.001,"$p<0.001$",paste("$",round(pvalue,digit=3),"$"))]

# plot

colores<-rev(c("#ef8a62","#67a9cf"))

# rename facets
featureNames <- c(`medSpeed`=paste("$Speed$"),`medLsd51`=paste("$SD$"),`medLsdD51`=paste("$SD'$"),
                  `medLrms51`=paste("$RMS$"),
                  `medLrmslD51`=paste("$RMS'$"),`medLcov51`=paste("$CoV$"),
                  `medLcovD51`=paste("$CoV'$"),`medTI51`=paste("$I$"),
                  `medTIms51`=paste("$I'$"),`medK`=paste("$\\kappa$"),
                  `meanK`=paste("$\\overline\\kappa$"))

p1 <- ggplot(longMeasures, 
             aes(x = normValues,group=grupo,fill = factor(grupo))) + 
  stat_density(aes(ymax = ..density..,  ymin = -..density..),
               geom = "ribbon", alpha = 0.6,position = "identity")+
  scale_y_continuous(name= "Measures of balance",expand=c(0.02,0.02))+
  scale_x_continuous(name= "Normalized measures",expand=c(0,0),limits=limites,
                     breaks=c(-2.0,-1.0,0.0,1.0,2.0,3,4.0,5.0,
                              limites[2]-1.0,limites[2]-0.6,limites[2]-0.2),
                     labels=c("$-2$","$-1$","$0$","$1$","$2$",
                              "$3$","$4$","$5$","$p$","$U$","$OVL$"))+
  scale_fill_manual(values=colores,name=paste("Age group: "),
                    breaks=c("1","2"),
                    labels=c("Younger (20+)","Older (61+)"))+
  theme_bw() +
  theme(legend.position="bottom",
        axis.title=element_text(face="bold"),
        plot.title=element_text(face="bold"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(colour=NA, fill=NA),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.margin.x=unit(0,"lines")
  )+
  facet_grid(. ~ measures,labeller=as_labeller(featureNames),switch = "x")+
  geom_text(data=measureOverlaps,
            aes(x=y,y=0,label=paste("$",round(mOvlp*100,digit=1),"\\%$")))+
  geom_text(data=mwUtest[-3],
            aes(x=y,y=0,label=paste("$",round(statistic,digit=2),"$")))+
  geom_text(data=mwUtest[-3],
            aes(x=yp,y=0,label=ptext))+
  coord_flip()
p1

# tikz("densityPlot.tex",standAlone = TRUE, width=9, height=6)
#   p1
# dev.off()

#################################################################################################################
# Parallel coordinates (Figure 7)
#################################################################################################################
colores<-rev(c("#ef8a62","#67a9cf"))
labsMath <- c(paste("$Speed$"),paste("$SD$"),paste("$SD'$"),paste("$RMS$"),paste("$RMS'$"),
              paste("$CoV$"),paste("$CoV'$"),paste("$I$"),paste("$I'$"),
              paste("$\\kappa$"),paste("$\\overline\\kappa$"))

setkey(longMeasures,iSubj,trial)
longMeasures[,iTrial:=.GRP,by=key(longMeasures)]

parPlot <- ggplot(data=longMeasures,aes(x=measures,y=normValues,group=iTrial)) +
  geom_boxplot(aes(x=measures,y=normValues,group=measures),size=0.85)+
  geom_line(aes(colour=factor(grupo)),alpha=0.5)+
  scale_colour_manual(values=colores,name=paste("Age group: "),
                      breaks=c("1","2"),
                      labels=c("Younger (20+)","Older (61+)"))+
  theme_bw() +
  scale_x_discrete(name = "Measures of balance",expand=c(0.02,0.02),
                   breaks =c("medSpeed","medLsd51","medLsdD51","medLrms51","medLrmslD51",
                             "medLcov51","medLcovD51","medTI51","medTIms51","medK","meanK"),
                   labels = labsMath) +
  scale_y_continuous(name = "Normalized measures",expand=c(0.01,0.01),breaks=c(-2:5))+
  theme(legend.position="bottom",
        axis.title=element_text(face="bold"),
        plot.title=element_text(face="bold"))+
  guides(colour = guide_legend(override.aes = list(size=1.5)))

parPlot

# tikz("orderedPCPlot.tex",standAlone = TRUE, width=8, height=5)
# parPlot
# dev.off()

#################################################################################################################
## PCA projection (Figure 8)
#################################################################################################################

colores<-rev(c("#ef8a62","#67a9cf"))
labsMath <- c(paste("$Speed$"),paste("$SD$"),paste("$SD'$"),paste("$RMS$"),paste("$RMS'$"),
              paste("$CoV$"),paste("$CoV'$"),paste("$I$"),paste("$I'$"),
              paste("$\\kappa$"),paste("$\\overline\\kappa$"))

normalizedRes <- as.data.table(scale(
                                  tableRes[,.(medSpeed,medLsd51,medLsdD51,medLrms51,medLrmslD51,
                                                 medLcov51,medLcovD51,medTI51,medTIms51,medK,meanK)]
                                  )
                               )
balance.pca <- prcomp(normalizedRes)

balancePcaPlot <- tableRes[,.(iSubj,trial,Age,Decade)]
balancePcaPlot[,PCA1:=balance.pca$x[,1]]
balancePcaPlot[,PCA2:=balance.pca$x[,2]]
balancePcaPlot[,grupo:=ifelse(Age < 61, 1, 2)]

# Arrow coordinates and labels
biplotDat <- as.data.table(balance.pca$rotation[,1:2]*5)
biplotDat[,vars:=labsMath]
biplotDat[,PCA1:=0]
biplotDat[,PCA2:=0]
biplotDat[,grupo:=1]

# Extracting variance from the eigenvalues (i.e. standard dev: pr$sdev)
variance <- round(balance.pca$sdev^2/sum(balance.pca$sdev^2)*100,digit=2)

bpp <- ggplot(balancePcaPlot,aes(PCA1,PCA2,group=factor(grupo)))
bpp <- bpp + geom_point(aes(colour=factor(grupo)),size=1,alpha=0.5)+ stat_ellipse(size=0.1) +
  scale_x_continuous(name=paste("PC1","(explained variance,",variance[1],"\\%)"),
                     expand=c(0.01,0.01))+
  scale_y_continuous(name=paste("PC2","(explained variance,",variance[2],"\\%)"),
                     expand= c(0.01,0.01))+
  scale_color_manual(values=colores,guide = guide_legend(reverse=TRUE),
                     name=paste("Groups"),
                     breaks=c("1","2"),
                     labels=c("20+","61+")) +
  geom_segment(data=biplotDat,aes(xend=PC1,yend=PC2),
               arrow = arrow(length = unit(0.3,"cm")))+
  geom_text(data=biplotDat,aes(x=PC1*1.2,y=PC2*1.2,label=vars))+
  theme(legend.position="right",
        panel.background = element_rect(fill = NA,colour = NA),
        legend.key = element_rect(fill=NA,colour = NA)
  )+
  coord_fixed(ratio = 1)+
  annotate("text", x = 4, y = -4, label = "Older",colour=colores[2])+
  annotate("text", x = 6.5, y = 2.5, label = "Younger",colour=colores[1])
bpp

# tikz("PCAprojectionPlos.tex",standAlone = TRUE, width=8, height=5)
# bpp
# dev.off()

# ##########################################################################################################
# ## Contributions (Figure 9)
# ##########################################################################################################

biplotDat[,contPC1:=(PC1/5)^2*100]
biplotDat[,contPC2:=(PC2/5)^2*100]

# order according to the coefficient of overlap
biplotDat[,varNames:=rownames(balance.pca$rotation[,1:2])]
biplotDat[,mOvlp:=measureOverlaps[,mOvlp][match(varNames,measureOverlaps[,measures])]]
setkey(biplotDat,mOvlp)
biplotDat[,order:=seq_along(mOvlp)]


colores <- c("#1b9e77","#d95f02","#7570b3")

contribsPlot <- ggplot(data=biplotDat,aes(x=factor(order),y=contPC1,group=grupo)) + 
  geom_line(aes(colour="1")) + geom_point(aes(colour="1"))+
  geom_line(aes(y=contPC2,colour="2")) + geom_point(aes(y=contPC2,colour="2"))+
  theme_bw()+
  scale_colour_manual(values=colores[c(2,3)],name=paste("Principal components: "),
                      breaks=c(1,2),
                      labels=c("PC1","PC2"))+    
  scale_y_continuous(name="Contribution (\\%)")+
  scale_x_discrete(name = "Variables",expand=c(0.02,0.02)
                   ,breaks =c(1:11),
                   labels = biplotDat[,vars])+
  theme(legend.position="bottom",
        axis.title=element_text(face="bold"),
        plot.title=element_text(face="bold"))+    
  geom_hline(yintercept=100/11,colour=colores[1],linetype="dashed",size=1)

contribsPlot

# tikz("../pics/contriVar.tex",standAlone = TRUE, width=6, height=3)
# contribsPlot
# dev.off()

# ##########################################################################################################
# ## matrix scatterplot costume version (Figure 10)
# ##########################################################################################################

# Excluding 5% of the points
# index of values > 3.8
oul <- longMeasures[,.I[normValues>3.4]]
# extracting indexes in a non-long format form (as a matrix)
# 400 rows
oul <- unique(oul%%400)
# repeating the pattern 11 times because there are 11 variables
oul <- rep(oul,11)
# repeating the pattern at the position of each variable
oul <- oul + rep(0:10,each=length(oul)/11)*400

lmeas <-longMeasures[-oul]

# limits
limites3 <- lmeas[,.(inferior=range(normValues)[1],superior=range(normValues)[2]),
                  by=.(measures)]

# Local translation between consecutive scatterplots in j direction
limites3[,lt := c(0,superior[1:(.N-1)] - inferior[2:.N])]

# Global translation of the scatterplots in the main plot in j direction
limites3[,ttrans := cumsum(lt)]

# Local translation between consecutive scatterplots in i direction (rows) see the next loops
limites3[,lti := c(rev(superior[.N:2] - inferior[(.N-1):1]),0)]
# Global translation of the scatterplots in the main plot in j direction (columns)
limites3[,ttransi := rev(cumsum(rev(lti)))]

# Extracting the names of the variables
cols <- colnames(tableRes)[5:15]

# Setting measures as key
setkey(lmeas,measures)

# Creating an empty datatable
scatPlotDat <- data.table(data.frame())

# Formatting the data for the scatterplot matrix
for (i in c(1:10))  
{
  # Rows in the scatterplot matrix 
  colsDat <- lmeas[.(cols[i]),.(grupo,x=normValues+limites3[,ttrans][i])]
  for (j in c(11:(i+1)))
  {  # Columns in the scatterplot matrix 
    colsDat[,y:=lmeas[.(cols[j]),(normValues+limites3[,ttransi][j])]]
    scatPlotDat <- rbind(scatPlotDat,colsDat)
  }
}


labMeas<-c(paste("$RMS$"),paste("$I$"),paste("$\\kappa$"),paste("$\\overline\\kappa$"),paste("$Speed$"),
           paste("$I'$"),paste("$CoV'$"),paste("$SD$"),paste("$CoV$"),paste("$RMS'$"),paste("$SD'$"))

#################################################################################################################
## Pearson correlation
#################################################################################################################
library(Hmisc)
corData <- tableRes[,.(medLrms51,medTI51,medK,meanK,medSpeed,medTIms51,medLcovD51 ,medLsd51,medLcov51,
                       medLrmslD51,medLsdD51)]

# Estimating correlations and significance
cors <- rcorr(as.matrix(corData),type="pearson")

# Extracting correlation values and formatting in a data table
tmpcor <- cors[[1]]
# Discarding lower triangular matrix
tmpcor[upper.tri(tmpcor)] <- NA

# Converting to data table
tmpcor <- as.data.table(tmpcor)
# Adding the names of the variables in a column
tmpcor[,corMeasures:=colnames(tmpcor)]

# Long format
longCorTable <- melt(tmpcor,id.vars=c("corMeasures"),
                     variable.name="Measures",
                     value.name="valueCors")

# Extracting p-values
tmpcor <- cors[[3]]
# Discarding lower triangular matrix
tmpcor[upper.tri(tmpcor)] <- NA

# Converting to data table
tmpcor <- as.data.table(tmpcor)
# Adding the names of the variables in a column
tmpcor[,corMeasures:=colnames(tmpcor)]
#tmpcor[,idMeasurep:=seq_along(corMeasures)]

# Long format
longPTable <- melt(tmpcor,id.vars=c("corMeasures"),
                   variable.name="Measures",
                   value.name="pValues")

longCorTable[,pValues:=longPTable[,pValues]]

longCorTable[pValues<0.05]

# Vector containing names of the variables
features <- limites3[,measures]
# Vector containing the x postions of the variables
#posfeat <- limites3[,rev(breaksx)]
posfeat <- limites3[,ttrans+(superior+inferior)/2]

# Matching names of the variables and x positions in the plot
longCorTable[,posX:=posfeat[match(corMeasures,features)]]

#posfeaty <- limites3[,breaksy]
posfeaty <- limites3[,ttransi+(superior+inferior)/2]

# Matching names of the variables and y positions in the plot
longCorTable[,posY:=posfeaty[match(Measures,features)]]

#tmp <- longCorTable[pValues<0.0001]
tmp <- longCorTable[!is.na(pValues)]
tmp[,grupo:=2]

############################################################################################
# lower matrix scatterplot 
############################################################################################
colores<-rev(c("#ef8a62","#67a9cf"))

scatMatPlot <- ggplot(data=scatPlotDat,aes(x=x,y=y,colour=factor(grupo))) + 
  geom_point(alpha=0.5,size=0.3)+
  scale_colour_manual(values=colores,name=paste("Age group: "),
                      breaks=c("1","2"),
                      labels=c("Younger (20+)","Older (61+)"))+
  geom_vline(xintercept=limites3[-.N,ttrans+superior],colour="gray")+
  geom_hline(yintercept=limites3[-.N,ttransi+inferior],colour="gray")+
  scale_x_continuous(expand=c(0,0),breaks=NULL,
                     limits=c(limites3[,inferior][1],limites3[,ttrans][11]+3.5))+
  scale_y_continuous(expand=c(0,0),breaks=NULL,
                     limits=c(limites3[,inferior][11],limites3[,ttransi][1]+2.4))+
  annotate(geom="text",x=limites3[,ttrans+(superior+inferior)/2],
           y=limites3[,ttransi+(superior+inferior)/2],label=labMeas,size=4)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="bottom",
        axis.title=element_blank(),
        legend.key.size=unit(0.5,"cm"))
#scatMatPlot 
# upper scatterplot matrix

scatMatPlot <- scatMatPlot + 
  geom_text(data=tmp,aes(x=posX,y=posY,label=as.character(signif(valueCors,2)),
                         size=abs(valueCors)),colour="darkgray")+
  scale_size(range=c(3,7))+
  guides(size=FALSE)+
  guides(colour = guide_legend(override.aes = list(size=5)))

scatMatPlot

# tikz("../pics/scatMatC.tex",standAlone = TRUE, width=8, height=8)
# scatMatPlot
# dev.off()

