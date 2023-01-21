#PHYSIOLOGY----
    #Fold change 14C fixation tolerant vs susceptible cultivar----
    setwd("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/")
    library(ggplot2)
    library(ggpubr)
    library(officer)
    library(rvg)
    library(dplyr)
      #----chart theme
      chartTheme <- theme(plot.title=element_text(size=20, face = "bold", hjust = 0.5),
                          axis.title.x=element_text(size=9, face = "bold"), 
                          axis.title.y=element_text(size=9, face = "bold"),
                          axis.text.x=element_text(size=9, angle= 0, vjust=0.5),
                          axis.text.y=element_text(size=9),
                          panel.grid.minor.y=element_line(linetype="dotted"),
                          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                          plot.background=element_rect(linetype="blank"),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(), 
                          axis.line = element_line(colour = "black"),
                          legend.key = element_rect(fill = "white"))
      
      
      #----Comparison FIX PROPORTION
      EXP_DATA<- read.csv("Data/EXP_OBJ_1_FILTERED_2022_07_27.csv")
      EXP_DATA<- EXP_DATA[-which(interaction(EXP_DATA$Batch, EXP_DATA$Cultivar)=="3.SB"),]
      # EXP_DATA<- select(EXP_DATA, Batch,Tree.Code, GS.before.injectoin, X1.h, Export_Ratio, Cultivar, Status, diff, fix)
      # write.csv(EXP_DATA, "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/Objective 1 Corr.csv")
      EXP_DATA<- filter(EXP_DATA, diff!=1)
      EXP_DATA$fix<- 1/EXP_DATA$fix
      t_test_FIX_PA<- t.test(log(filter(EXP_DATA, Cultivar=="PA")$fix, base = 2),mu = 0, alternative = "less", var.equal = F)
      t_test_FIX_SB<- t.test(log(filter(EXP_DATA, Cultivar=="SB")$fix, base = 2),mu = 0, alternative = "greater", var.equal = F)
      shapiro.test(log(filter(EXP_DATA, Cultivar=="PA")$fix, base = 2))
      shapiro.test(log(filter(EXP_DATA, Cultivar=="SB")$fix, base = 2))
      figure_1_A<- ggplot(EXP_DATA, aes(x=Cultivar, y=log(fix, base = 2), fill= Cultivar))+
        geom_violin(trim = FALSE, color= "#C7C9C8")+
        scale_fill_manual(values=c("#003074", "#F57330"))+
        geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
        geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 1.4, position=position_jitter(h=0.1, w=0.1), alpha=0.5)+
        stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.30)+
        #geom_signif(map_signif_level = T, test = "t.test", comparisons = list(c("PA", "SB")))+
        geom_hline(yintercept = 0, color="#343741", size=0.7, linetype="dashed")+
        annotate(geom="text", y=-3.5,x=1, label=bquote(atop(H[a]~"= m<"*mu*~ phantom(),"P= 0.01")), size=2.5)+
        annotate(geom="text", y=-3.5,x=2, label=bquote(atop(H[a]~"= m>"*mu*~ phantom(),"P= 0.06")), size=2.5)+
        guides(fill=FALSE)+
        chartTheme+
        theme(axis.title.x =element_blank())+
        ylab(bquote(atop(Log[2]~Fold-Change~(Infected/Healthy)~ phantom(),"in"~{}^14*C~Fixation)))
        

      #SAVE THE FIGURE PPTX
      # 
      # p_dml <- rvg::dml(ggobj = figure_1)
      # officer::read_pptx() %>%
      #   # add slide 
      # officer::add_slide() %>%
      #   # specify object and location of object
      # officer::ph_with(p_dml, ph_location()) %>%
      #   # export slide
      # base::print(
      #   target = "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-06 Figures/Fixation.pptx"
      # )
    
    #Fold change 14C Export tolerant vs susceptible cultivar----
      setwd("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/")
      library(ggplot2)
      library(ggpubr)
      library(officer)
      library(rvg)
      library(dplyr)
      #----chart theme
      chartTheme <- theme(plot.title=element_text(size=20, face = "bold", hjust = 0.5),
                          axis.title.x=element_text(size=9, face = "bold"), 
                          axis.title.y=element_text(size=9, face = "bold"),
                          axis.text.x=element_text(size=9, angle= 0, vjust=0.5),
                          axis.text.y=element_text(size=9),
                          panel.grid.minor.y=element_line(linetype="dotted"),
                          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                          plot.background=element_rect(linetype="blank"),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(), 
                          axis.line = element_line(colour = "black"),
                          legend.key = element_rect(fill = "white"))
      
      EXP_DATA<- read.csv("Data/EXP_OBJ_1_FILTERED_2022_07_27.csv")
      EXP_DATA<- EXP_DATA[-which(interaction(EXP_DATA$Batch, EXP_DATA$Cultivar)=="3.SB"),]
      EXP_DATA<- filter(EXP_DATA, diff!=1)
      EXP_DATA$diff<- 1/EXP_DATA$diff
      t_test_FIX_PA<- t.test(log(filter(EXP_DATA, Cultivar=="PA")$diff, base = 2),mu = 0, alternative = "less")
      t_test_FIX_SB<- t.test(log(filter(EXP_DATA, Cultivar=="SB")$diff, base = 2),mu = 0, alternative = "less")
      shapiro.test(log(filter(EXP_DATA, Cultivar=="PA")$diff, base = 2))
      shapiro.test(log(filter(EXP_DATA, Cultivar=="SB")$diff, base = 2))
      
      figure_1_C<- ggplot(EXP_DATA, aes(x=Cultivar, y=log(diff), fill= Cultivar))+
        geom_violin(trim = FALSE, color= "#C7C9C8")+
        scale_fill_manual(values=c("#003074", "#F57330"))+
        geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
        geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 1.4, position=position_jitter(h=0.1, w=0.1), alpha=0.5)+
        stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.30, alpha=0.6)+
        #geom_signif(map_signif_level = T, test = "t.test", comparisons = list(c("PA", "SB")))+
        geom_hline(yintercept = 0, color="#343741", size=0.7, linetype="dashed")+
        annotate(geom="text", y=-1.4,x=1, label=bquote(atop(H[a]~"= m<"*mu*~ phantom(),"P < 0.001")), size=2.5)+
        annotate(geom="text", y=-1.4,x=2, label=bquote(atop(H[a]~"= m<"*mu*~ phantom(),"P= 0.01")), size=2.5)+
        guides(fill=FALSE)+
        coord_cartesian(ylim = c(-1.6,0.4))+
        chartTheme+
        theme(axis.title.x =element_blank())+
        ylab(bquote(atop(Log[2]~Fold-Change~(Infected/Healthy)~ phantom(),"in"~{}^14*C~Export~Ratio)))
      
      
      # 
      # p_dml <- rvg::dml(ggobj = figure_2)
      # officer::read_pptx() %>%
      #   # add slide 
      #   officer::add_slide() %>%
      #   # specify object and location of object
      #   officer::ph_with(p_dml, ph_location()) %>%
      #   # export slide
      #   base::print(
      #     target = "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-06 Figures/EXP.pptx"
      #   )
    
    #stomatal conductance healthy VS infected SB and PA----
      setwd("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/")
      library(ggplot2)
      library(ggpubr)
      library(rvg)
      library(dplyr)
      #----chart theme
      chartTheme <- theme(plot.title=element_text(size=20, face = "bold", hjust = 0.5),
                          axis.title.x=element_text(size=18, face = "bold"), 
                          axis.title.y=element_text(size=18, face = "bold"),
                          axis.text.x=element_text(size=14, angle= 0, vjust=0.5),
                          axis.text.y=element_text(size=14),
                          panel.grid.minor.y=element_line(linetype="dotted"),
                          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                          plot.background=element_rect(linetype="blank"),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(), 
                          axis.line = element_line(colour = "black"),
                          legend.key = element_rect(fill = "white"))
      
      
      #-----Code
      EXP_DATA<- read.csv("Data/EXP_OBJ_1_FILTERED_2022_07_27.csv")
      
      figure_2<- ggplot(EXP_DATA, aes(x=Status, y=GS.before.injectoin, fill=Status))+
        geom_violin(trim = FALSE, color= "#C7C9C8")+
        scale_fill_manual(values=c("#003074", "#F57330"))+
        geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
        geom_point(color= "#343741", fill = "#D8D4D7", size = 0.7, position=position_jitter(h=0.1, w=0.1), alpha=0.5)+
        scale_shape_manual(values=c(21, 22))+
        stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.15, alpha=0.6)+
        geom_signif(test = "t.test", comparisons = list(c("Healthy","HLB+")))+
        facet_grid(~Cultivar)+
        chartTheme
      # p_dml <- rvg::dml(ggobj = figure_2)
      # officer::read_pptx() %>%
      #   # add slide 
      #   officer::add_slide() %>%
      #   # specify object and location of object
      #   officer::ph_with(p_dml, ph_location()) %>%
      #   # export slide
      #   base::print(
      #     target = "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-06 Figures/Estomatal conductance_2022-12-15.pptx"
      #   )
      shapiro.test(filter(EXP_DATA, Cultivar=="PA", Status=="Healthy")$GS.before.injectoin)
      shapiro.test(filter(EXP_DATA, Cultivar=="SB", Status=="Healthy")$GS.before.injectoin)
      shapiro.test(filter(EXP_DATA, Cultivar=="PA", Status=="HLB+")$GS.before.injectoin)
      shapiro.test(filter(EXP_DATA, Cultivar=="SB", Status=="HLB+")$GS.before.injectoin)
     
       perm_estomatal_conduntace<- select(EXP_DATA, Cultivar, GS.before.injectoin, Status)
       average_cultivars_and_Status<- aggregate(perm_estomatal_conduntace$GS.before.injectoin, list(perm_estomatal_conduntace$Cultivar, perm_estomatal_conduntace$Status), mean, na.rm=T)
       fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
       fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
       observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
       
       perm<- 5000
       vect<- c(rep(NA, perm))
       for (i in 1:perm) {
         temp_data<- data.frame(Cultivar= perm_estomatal_conduntace$Cultivar, Status= perm_estomatal_conduntace$Status, GS.before.injectoin= c(sample(filter(perm_estomatal_conduntace, Cultivar=="PA")$GS.before.injectoin, replace = F), sample(filter(perm_estomatal_conduntace, Cultivar=="SB")$GS.before.injectoin, replace = F)))
         temp_average_cultivars_and_Status<- aggregate(temp_data$GS.before.injectoin, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
         temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
         temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
         vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
       }
       
       hist(vect)
       abline(v=observed_fold_GS, col="red")
       p_value<- mean(vect>observed_fold_GS)
       # res_estomatal_conductance<- data.frame(variable=rep("Stomatal Conductance", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
       # write.csv(res_estomatal_conductance, "C:/Users/jrobl/OneDrive - University of Florida/Desktop/res_estomatal_conductance.csv", row.names = F)
    
    #Starch healthy vs infected SB and PA----
       setwd("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/")
       library(dplyr)
       library(ggplot2)
       library(ggpubr)
       library(officer)
       library(rvg)
       #----chart theme
       chartTheme <- theme(plot.title=element_text(size=20, face = "bold", hjust = 0.5),
                           axis.title.x=element_text(size=9, face = "bold"), 
                           axis.title.y=element_text(size=9, face = "bold"),
                           axis.text.x=element_text(size=9, angle= 0, vjust=0.5),
                           axis.text.y=element_text(size=9),
                           panel.grid.minor.y=element_line(linetype="dotted"),
                           plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                           plot.background=element_rect(linetype="blank"),
                           panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank(),
                           panel.background = element_blank(), 
                           axis.line = element_line(colour = "black"),
                           legend.key = element_rect(fill = "white"))
       #----code
       starch<- read.csv("Data/STARCH_OBJ_1 2022_06_23.csv")
       tree_codes<- read.csv("Data/Tree codes.csv")
       colnames(tree_codes)<- c("Tree.Code", "Cultivar", "Status")
       starch<- inner_join(starch, tree_codes)
       #Calculate the starch by grame of fresh weight (the original value is in mg)
       starch$`Starch (mg/g)`<- starch$Starch.by.mL..mg./(starch$Weight..mg./1000)
       #calculate the starch content by area. Every punch is 2.83×10-5 m2 and the "Starch.by.mL..mg." is the starch in three punches
       area_of_one_punch<- 2.83e-5
       starch$`Starch (mg/m2)`<- (starch$Starch.by.mL..mg./3)/area_of_one_punch
       starch$`Starch (ug/mm2)`<- ((starch$Starch.by.mL..mg.*1000)/3)/28.27
       starch$`log Starch (ug/mm2)`<- log(starch$`Starch (ug/mm2)`)
       
       figure_1_D<- ggplot(starch, aes(x=Status, y=`log Starch (ug/mm2)`))+
         geom_violin(trim = FALSE, color= "#C7C9C8", aes(fill= Cultivar))+
         geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
         geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 1.4, position=position_jitter(h=0.1, w=0.1), alpha=0.5)+
         stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.30, alpha=0.6)+
         facet_grid(~Cultivar)+
         geom_signif(map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",paste("P= ",round(p,2)))},textsize = 2.5, test = "t.test", comparisons = list(c("Healthy", "HLB+")))+
         scale_fill_manual(values=c("#003074", "#F57330", "#003074", "#003074"))+
         guides(fill=FALSE)+
         ylab(bquote(Log~Starch~"("*mu*g/mm^2*")"))+
         chartTheme+
         theme(axis.title.x = element_blank())
         
       shapiro.test(filter(starch, Cultivar=="PA" & Status=="Healthy")$`log Starch (ug/mm2)`)
       shapiro.test(filter(starch, Cultivar=="PA" & Status=="HLB+")$`log Starch (ug/mm2)`)
       shapiro.test(filter(starch, Cultivar=="SB" & Status=="Healthy")$`log Starch (ug/mm2)`)
       shapiro.test(filter(starch, Cultivar=="SB" & Status=="HLB+")$`log Starch (ug/mm2)`)
       
       
       
       # p_dml <- rvg::dml(ggobj = figure_Starch)
       # officer::read_pptx() %>%
       #   # add slide 
       #   officer::add_slide() %>%
       #   # specify object and location of object
       #   officer::ph_with(p_dml, ph_location()) %>%
       #   # export slide
       #   base::print(
       #     target = "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-06 Figures/Starch_22022-12-15.pptx"
       #   )
       # 
       perm_starch<- select(starch, Cultivar, `Starch (ug/mm2)`, Status)
       average_cultivars_and_Status<- aggregate(perm_starch$`Starch (ug/mm2)`, list(perm_starch$Cultivar, perm_starch$Status), mean, na.rm=T)
       fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
       fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
       observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
       
       perm<- 5000
       vect<- c(rep(NA, perm))
       for (i in 1:perm) {
         temp_data<- data.frame(Cultivar= perm_starch$Cultivar, Status= perm_starch$Status, Starch= c(sample(filter(perm_starch, Cultivar=="PA")$`Starch (ug/mm2)`, replace = F), sample(filter(perm_starch, Cultivar=="SB")$`Starch (ug/mm2)`, replace = F)))
         temp_average_cultivars_and_Status<- aggregate(temp_data$Starch, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
         temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
         temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
         vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
       }
       
       hist(vect)
       abline(v=observed_fold_GS, col="red")
       p_value<- mean(vect>observed_fold_GS)
       
    
       res_starch<- data.frame(variable=rep("Starch", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
       write.csv(res_starch, "C:/Users/jrobl/OneDrive - University of Florida/Desktop/res_starch.csv", row.names = F)
    
    #Callose confocal healthy VS infected SB and PA----
       setwd("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/CALL confocal/")
       library("tidyverse")
       library("dplyr")
       library("agricolae")
       library("reshape2")
       library("fs")
       library("stringr")
       library("readxl")
       library("writexl")
       library("MASS")
       library("magrittr")
       library("car")
       library("Metrics")
       library("performance")
       library("ggpubr")
       library("rstatix")
       library("purrr")
       library(officer)
       library(rvg)
       library(spatstat)
       
       #----chart theme
       chartTheme <- theme(plot.title=element_text(size=20, face = "bold", hjust = 0.5),
                           axis.title.x=element_text(size=9, face = "bold"), 
                           axis.title.y=element_text(size=9, face = "bold"),
                           axis.text.x=element_text(size=9, angle= 0, vjust=0.5),
                           axis.text.y=element_text(size=9),
                           panel.grid.minor.y=element_line(linetype="dotted"),
                           plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                           plot.background=element_rect(linetype="blank"),
                           panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank(),
                           panel.background = element_blank(), 
                           axis.line = element_line(colour = "black"),
                           legend.key = element_rect(fill = "white"))
       ####Get callose counts from ilastik csv files
       
       #first, make a function that reads csv files from the working directory,
       #converts all the columns to characters, 
       #combines all the common columns into one big dataframe,
       #and mutates the data frame so that the file names are attached to each row
       read_plus <- function(flnm) {
         read_csv(flnm, col_types = cols(.default = "c")) %>% 
           mutate(filename = flnm)
       }
       
       #use the function to make a giant table
       
       big_table <-
         list.files(pattern = "*.csv",
                    full.names = F) %>% 
         map_df(~read_plus(.))
       
       #for convenience, replace spaces in column names with underscores
       names(big_table) <- gsub(" ", "_", names(big_table))
       names(big_table)
       
       #clean up the data frame by dropping unneeded variables
       #keep only object size and  file name
       #other variables might be useful later, but not now
       big_table <- select(big_table, c('Size_in_pixels','filename', 'Center_of_the_object_0', 'Center_of_the_object_1'))
       
       #get useful info from file name
       #split filename wherever _ occurs and make each piece a new column
       big_table[,5:9]<-str_split_fixed(big_table$filename, "_", 5)
       
       #drop unneeded columns
       big_table <- select(big_table, c('Size_in_pixels','V1', 'V2', 'Center_of_the_object_0', 'Center_of_the_object_1'))
       
       #rename columns
       colnames(big_table)[2] = "Tree.Code"
       colnames(big_table)[3] = "picture"
       
       #convert the size columns to a number to avoid calculation issues
       typeof(big_table$Size_in_pixels)
       big_table$Size_in_pixels<-as.numeric(big_table$Size_in_pixels)
       
       
       
       #make a summary for stats: first, get count of callose objects by image,
       #also including mean size in case that is useful.
       big_table_summary <- big_table %>%
         group_by(Tree.Code, picture) %>%
         summarise(count = n(),meansize = mean(Size_in_pixels),dist_near_neight= mean(nndist(X =Center_of_the_object_0, Y=Center_of_the_object_1)))
       
       #ungroup to avoid issues
       big_table_summary<- ungroup(big_table_summary)
       tree_codes<- read.csv("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/Tree codes.csv")
       big_table_summary<- inner_join(big_table_summary, tree_codes)
       big_table_summary$area<- big_table_summary$count*big_table_summary$meansize
       
       
       #Counts
       f_counts_status<-ggplot(data=big_table_summary, aes(x=Status, y = log(count)))+
         geom_violin(trim = FALSE, color= "#C7C9C8", aes(fill= Cultivar))+
         geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
         geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 1.4, position=position_jitter(h=0.1, w=0.1), alpha=0.5)+
         stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.30, alpha=0.6)+
         facet_grid(~Cultivar)+
         geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "t.test", map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",paste("P= ",round(p,2)))},textsize = 2.5)+
         scale_fill_manual(values=c("#003074", "#F57330", "#003074", "#003074"))+
         guides(fill=FALSE)+
         ylab(bquote(atop(Log~Number~of~Callose~ phantom(), Deposits~per~Frame)))+
         chartTheme+
         theme(axis.title.x = element_blank())
       
      
       
       
       # p_dml <- rvg::dml(ggobj = f_counts_status)
       # officer::read_pptx() %>%
       #   # add slide
       #   officer::add_slide() %>%
       #   # specify object and location of object
       #   officer::ph_with(p_dml, ph_location()) %>%
       #   # export slide
       #   base::print(
       #     target = "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-11 Figures/CALL_counts_BY_status.pptx"
       #   )
       
       # 
       # f_counts_cultivars<-ggplot(data=big_table_summary, aes(x=Cultivar, y = log(count), fill=Cultivar))+
       #   geom_violin(trim = FALSE, color= "#C7C9C8")+
       #   scale_fill_manual(values=c("#003074", "#F57330"))+
       #   geom_boxplot(width=0.1, , fill= "white", outlier.shape = NA, color= "#C7C9C8")+
       #   geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 0.7, position=position_jitter(h=0.1, w=0.1))+
       #   stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.15)+
       #   facet_grid(~Status)+
       #   geom_signif(comparisons = list(c("PA", "SB")), test = "t.test", map_signif_level = TRUE)+
       #   chartTheme
       # 
       # # p_dml <- rvg::dml(ggobj = f_counts_cultivars)
       # # officer::read_pptx() %>%
       # #   # add slide 
       # #   officer::add_slide() %>%
       # #   # specify object and location of object
       # #   officer::ph_with(p_dml, ph_location()) %>%
       # #   # export slide
       # #   base::print(
       # #     target = "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-11 Figures/CALL_counts_BY_cultivar.pptx"
       # #   )
       # # 
       
       shapiro.test(log(filter(big_table_summary, Cultivar=="PA" & Status=="Healthy")$count))
       shapiro.test(log(filter(big_table_summary, Cultivar=="PA" & Status=="HLB+")$count))
       shapiro.test(log(filter(big_table_summary, Cultivar=="SB" & Status=="Healthy")$count))
       shapiro.test(log(filter(big_table_summary, Cultivar=="PA" & Status=="HLB+")$count))
       
       perm_counts_callose<- select(big_table_summary, Cultivar, count, Status)
       average_cultivars_and_Status<- aggregate(perm_counts_callose$count, list(perm_counts_callose$Cultivar, perm_counts_callose$Status), mean, na.rm=T)
       fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
       fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
       observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
       
       perm<- 5000
       vect<- c(rep(NA, perm))
       for (i in 1:perm) {
         temp_data<- data.frame(Cultivar= perm_counts_callose$Cultivar, Status= perm_counts_callose$Status, count= c(sample(filter(perm_counts_callose, Cultivar=="PA")$count, replace = F), sample(filter(perm_counts_callose, Cultivar=="SB")$count, replace = F)))
         temp_average_cultivars_and_Status<- aggregate(temp_data$count, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
         temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
         temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
         vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
       }
       
       hist(vect)
       abline(v=observed_fold_GS, col="red")
       p_value<- mean(vect>observed_fold_GS)
       
    
       res_call_counts<- data.frame(variable=rep("CALL counts", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
       #write.csv(res_call_counts, "C:/Users/jrobl/OneDrive - University of Florida/Desktop/res_call_counts.csv", row.names = F)
    
       
       #Mean size
       f_meansize_status<- ggplot(data=big_table_summary, aes(x=Status, y = meansize))+
         geom_violin(trim = FALSE, color= "#C7C9C8", aes(fill= Cultivar))+
         scale_fill_manual(values=c("#003074", "#F57330"))+
         geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
         geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 1.4, position=position_jitter(h=0.1, w=0.1), alpha=0.5)+
         stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.30, alpha=0.6)+
         facet_grid(~Cultivar)+
         geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "wilcox.test", map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",paste("P= ",round(p,2)))},textsize = 2.5)+
         guides(fill=FALSE)+
         ylab(bquote(atop(Mean~Size~of~Callose~ phantom(), Deposits~per~Frame)))+
         chartTheme+
         theme(axis.title.x = element_blank())
      
       
    
       # p_dml <- rvg::dml(ggobj = f_meansize_status)
       # officer::read_pptx() %>%
       #   # add slide
       #   officer::add_slide() %>%
       #   # specify object and location of object
       #   officer::ph_with(p_dml, ph_location()) %>%
       #   # export slide
       #   base::print(
       #     target = "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-11 Figures/CALL_Mean_size_2022-12-10-BY_status.pptx"
       #   )
    
       perm_mean_size_callose<- select(big_table_summary, Cultivar, meansize, Status)
       average_cultivars_and_Status<- aggregate(perm_mean_size_callose$meansize, list(perm_mean_size_callose$Cultivar, perm_mean_size_callose$Status), mean, na.rm=T)
       fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
       fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
       observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
       
       perm<- 5000
       vect<- c(rep(NA, perm))
       for (i in 1:perm) {
         temp_data<- data.frame(Cultivar= perm_mean_size_callose$Cultivar, Status= perm_mean_size_callose$Status, mean_size= c(sample(filter(perm_mean_size_callose, Cultivar=="PA")$meansize, replace = F), sample(filter(perm_mean_size_callose, Cultivar=="SB")$meansize, replace = F)))
         temp_average_cultivars_and_Status<- aggregate(temp_data$mean_size, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
         temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
         temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
         vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
       }
       
       hist(vect)
       abline(v=observed_fold_GS, col="red")
       p_value<- mean(vect>observed_fold_GS)
       
    
      res_call_mean_size<- data.frame(variable=rep("CALL mean size", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      #write.csv(res_call_mean_size, "C:/Users/jrobl/OneDrive - University of Florida/Desktop/res_call_mean_size.csv", row.names = F)
      
      #AREA
      f_area_status<- ggplot(data=big_table_summary, aes(x=Status, y = log(area)))+
        geom_violin(trim = FALSE, color= "#C7C9C8", aes(fill= Cultivar))+
        scale_fill_manual(values=c("#003074", "#F57330"))+
        geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
        geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 1.4, position=position_jitter(h=0.1, w=0.1),alpha=0.5)+
        stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.30, alpha=0.6)+
        facet_grid(~Cultivar)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "t.test", map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",ifelse(p<0.01,"P < 0.01", paste("P= ",round(p,2))))},textsize = 2.5)+
        guides(fill=FALSE)+
        ylab(bquote(atop(Log~Total~Area~of~Callose~ phantom(), Deposits~per~Frame)))+
        chartTheme+
        theme(axis.title.x = element_blank())
      
      
      
      
      
      shapiro.test(log(filter(big_table_summary, Cultivar=="PA" & Status=="Healthy")$area))
      shapiro.test(log(filter(big_table_summary, Cultivar=="PA" & Status=="HLB+")$area))
      shapiro.test(log(filter(big_table_summary, Cultivar=="SB" & Status=="Healthy")$area))
      shapiro.test(log(filter(big_table_summary, Cultivar=="PA" & Status=="HLB+")$area))
    
      # p_dml <- rvg::dml(ggobj = f_area_status)
      # officer::read_pptx() %>%
      #   # add slide
      #   officer::add_slide() %>%
      #   # specify object and location of object
      #   officer::ph_with(p_dml, ph_location()) %>%
      #   # export slide
      #   base::print(
      #     target = "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-11 Figures/CALL_Area_2022-12-10-BY_status.pptx"
      #   )
      
      perm_area_callose<- select(big_table_summary, Cultivar, area, Status)
      average_cultivars_and_Status<- aggregate(perm_area_callose$area, list(perm_area_callose$Cultivar, perm_area_callose$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_area_callose$Cultivar, Status= perm_area_callose$Status, area= c(sample(filter(perm_area_callose, Cultivar=="PA")$area, replace = F), sample(filter(perm_area_callose, Cultivar=="SB")$area, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$area, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      res_call_area<- data.frame(variable=rep("CALL area", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      write.csv(res_call_area, "C:/Users/jrobl/OneDrive - University of Florida/Desktop/res_call_area.csv", row.names = F)
    
      #distribution CAllose
      nndist_fig_Status<- ggplot(data=big_table_summary, aes(x=Status, y = log(dist_near_neight)))+
        geom_violin(trim = FALSE, color= "#C7C9C8", aes(fill= Cultivar))+
        scale_fill_manual(values=c("#003074", "#F57330"))+
        geom_boxplot(width=0.1,  fill= "white", outlier.shape = NA, color= "#C7C9C8")+
        geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 1.4, position=position_jitter(h=0.1, w=0.1), alpha=0.5)+
        stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.30, alpha=0.6)+
        facet_grid(~Cultivar)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "wilcox.test", map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",ifelse(p<0.01,"P < 0.01", paste("P= ",round(p,2))))},textsize = 2.5)+
        guides(fill=FALSE)+
        ylab(bquote(atop(Log~Mean~Closest~Neighbor~ phantom(), of~Callose~Deposits~per~Frame)))+
        chartTheme+
        theme(axis.title.x = element_blank())
      
      
      shapiro.test(log(filter(big_table_summary, Cultivar=="PA" & Status=="Healthy")$dist_near_neight))
      shapiro.test(log(filter(big_table_summary, Cultivar=="PA" & Status=="HLB+")$dist_near_neight))
      shapiro.test(log(filter(big_table_summary, Cultivar=="SB" & Status=="Healthy")$dist_near_neight))
      shapiro.test(log(filter(big_table_summary, Cultivar=="PA" & Status=="HLB+")$dist_near_neight))
      
    # 
    # 
    # p_dml <- rvg::dml(ggobj = nndist_fig_Status)
    # officer::read_pptx() %>%
    #   # add slide
    #   officer::add_slide() %>%
    #   # specify object and location of object
    #   officer::ph_with(p_dml, ph_location()) %>%
    #   # export slide
    #   base::print(
    #     target = "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-11 Figures/CALL_closest_neightbour_BY_status_2022-12-19.pptx"
    #   )
    
    
      
      perm_dis_callose<- select(big_table_summary, Cultivar, dist_near_neight, Status)
      average_cultivars_and_Status<- aggregate(perm_dis_callose$dist_near_neight, list(perm_dis_callose$Cultivar, perm_dis_callose$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_dis_callose$Cultivar, Status= perm_dis_callose$Status, dist_near_neight= c(sample(filter(perm_dis_callose, Cultivar=="PA")$dist_near_neight, replace = F), sample(filter(perm_dis_callose, Cultivar=="SB")$dist_near_neight, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$dist_near_neight, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      # res_Dist_area<- data.frame(variable=rep("CALL distribution", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      # write.csv(res_Dist_area, "C:/Users/jrobl/OneDrive - University of Florida/Desktop/res_dist_area.csv", row.names = F)
      # 
    #Callose gene expression----
      setwd("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/")
      library(dplyr)
      library(reshape2)
      library(ggplot2)
      library(ggpubr)
      library(rvg)
      library(officer)
      
      
      chartTheme <- theme(plot.title=element_text(size=20, face = "bold", hjust = 0.5),
                          axis.title.x=element_text(size=18, face = "bold"), 
                          axis.title.y=element_text(size=18, face = "bold"),
                          axis.text.x=element_text(size=14, angle= 0, vjust=0.5),
                          axis.text.y=element_text(size=14),
                          panel.grid.minor.y=element_line(linetype="dotted"),
                          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                          plot.background=element_rect(linetype="blank"),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(), 
                          axis.line = element_line(colour = "black"),
                          legend.key = element_rect(fill = "white"))
      
      
      gene_expression<- read.csv("Data/SYBR Results 2022-08-02.csv")
      gene_expression<- gene_expression[-c(which(interaction(gene_expression$Sample.ID, gene_expression$File)=="258.SYBR Results 2022-07-29 (1).xlsx")),]
      gene_expression<- gene_expression[-c(which(interaction(gene_expression$Sample.ID, gene_expression$File)=="261.SYBR Results 2022-07-29 (2).xlsx")),]
      
      
      
      id_samples<-read.csv("Data/Samples IDs and Comments.csv", skip = 7)
      gene_expression<- inner_join(gene_expression, id_samples)
      tree_codes<- read.csv("Data/Tree codes.csv")
      gene_expression<- inner_join(tree_codes, gene_expression)
      gene_expression<- select(gene_expression, Cultivar, Status, Gene, Sample.ID, LEAF,Ct)
      #gene_expression_PA<- filter(gene_expression, Cultivar=="PA")
      #gene_expression_SB<- filter(gene_expression, Cultivar=="SB")
      gene_expression_means<- dcast(gene_expression, Cultivar +Status+ Sample.ID+ LEAF~Gene, fun.aggregate = mean, na.rm = TRUE)
      
      gene_expression_means$Dt_CsCALS3<- gene_expression_means$CsCALS3- gene_expression_means$GAPDH
      gene_expression_means$Dt_CsCALS7<- gene_expression_means$CsCALS7- gene_expression_means$GAPDH
      gene_expression_means$Dt_CsCALS9<- gene_expression_means$CsCALS9- gene_expression_means$GAPDH
      gene_expression_means$Dt_CsCALS11<- gene_expression_means$CsCALS11- gene_expression_means$GAPDH
      gene_expression_means$Dt_CsCALS12<- gene_expression_means$CsCALS12- gene_expression_means$GAPDH
      gm_mean = function(x, na.rm=TRUE){
        exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
      }
      
      gene_expression_means<- gene_expression_means[,-c(5:10)]
      gene_expression_Dt_Dt<- melt(data =gene_expression_means, id = c("Cultivar","Status", "Sample.ID","LEAF"))
      
      aritmetic_means_healthy<- aggregate(filter(gene_expression_Dt_Dt, Status=="Healthy")$value, list(filter(gene_expression_Dt_Dt, Status=="Healthy")$Cultivar, filter(gene_expression_Dt_Dt, Status=="Healthy")$LEAF, filter(gene_expression_Dt_Dt, Status=="Healthy")$variable), mean, na.rm=T)
      colnames(aritmetic_means_healthy)<- c("Cultivar", "LEAF", "variable", "arti_mean_healthy")
      geometric_means_healthy<- aggregate(filter(gene_expression_Dt_Dt, Status=="Healthy")$value, list(filter(gene_expression_Dt_Dt, Status=="Healthy")$Cultivar, filter(gene_expression_Dt_Dt, Status=="Healthy")$LEAF, filter(gene_expression_Dt_Dt, Status=="Healthy")$variable), gm_mean)
      colnames(geometric_means_healthy)<- c("Cultivar", "LEAF", "variable", "geom_mean_healthy")
      healthy_means<- inner_join(aritmetic_means_healthy, geometric_means_healthy)
      gene_expression_Dt_Dt<- full_join(healthy_means,gene_expression_Dt_Dt)
      gene_expression_Dt_Dt$dt_dt_ct<- gene_expression_Dt_Dt$value - gene_expression_Dt_Dt$arti_mean_healthy
      #gene_expression_Dt_Dt$dt_dt_ct_G<- gene_expression_Dt_Dt$value - gene_expression_Dt_Dt$geom_mean_healthy
      gene_expression_Dt_Dt$Fold_gene_expression<- 2^-(gene_expression_Dt_Dt$dt_dt_ct)
      #gene_expression_Dt_Dt$Fold_gene_expression_G<- 2^-(gene_expression_Dt_Dt$dt_dt_ct_G)
      
      
      
      
      count<- 1
      statistics<- matrix(ncol=6,nrow=30)
      colnames(statistics)<- c("Cultivar", "LEAF","Gene","T test P value", "shap healt P value", "shap HLB+ P value")
      
      
      
      
      # 
      # for (i in unique(gene_expression_Dt_Dt$Cultivar)) {
      #   for (j in unique(gene_expression_Dt_Dt$LEAF)) {
      #     for (k in unique(gene_expression_Dt_Dt$variable)) {
      #       a<- filter(gene_expression_Dt_Dt, Cultivar==i, LEAF==j, variable==k, Status=="Healthy")$Fold_gene_expression
      #       b<- filter(gene_expression_Dt_Dt, Cultivar==i, LEAF==j, variable==k, Status=="HLB+")$Fold_gene_expression
      #       if((sum(!is.na(a))+sum(!is.na(b)))>3){
      #         statistics[count,1]<- i
      #         statistics[count,2]<- j
      #         statistics[count,3]<- k
      #         statistics[count,4]<- t.test(a,b)$p.value
      #         statistics[count,5]<- ifelse(sum(!is.na(a))>2,shapiro.test(a)$p.value, "ao data" )
      #         statistics[count,6]<- ifelse(sum(!is.na(b))>2,shapiro.test(b)$p.value, "ao data" )
      #         count<- count+1
      #         print(paste(i,j,k,sep = " "))
      #       }else{
      #         statistics[count,1]<- i
      #         statistics[count,2]<- j
      #         statistics[count,3]<- k
      #         statistics[count,4]<- "no data"
      #         statistics[count,5]<- "no data"
      #         statistics[count,6]<- "no data"
      #         count<- count+1
      #       }
      # 
      #     }
      # 
      #   }
      # 
      # }
      # 
      
      
      
      
      
      
      for (i in unique(gene_expression_Dt_Dt$Cultivar)) {
        for (j in unique(gene_expression_Dt_Dt$LEAF)) {
          for (k in unique(gene_expression_Dt_Dt$variable)) {
            a<- filter(gene_expression_Dt_Dt, Cultivar==i, LEAF==j, variable==k, Status=="Healthy")$Fold_gene_expression
            b<- filter(gene_expression_Dt_Dt, Cultivar==i, LEAF==j, variable==k, Status=="HLB+")$Fold_gene_expression
            if(sum(!is.na(a))>1 & sum(!is.na(b))>1){
              statistics[count,1]<- i
              statistics[count,2]<- j
              statistics[count,3]<- k
              statistics[count,4]<- t.test(a,b)$p.value
              statistics[count,5]<- ifelse(sum(!is.na(a))>2,shapiro.test(a)$p.value, "ao data" )
              statistics[count,6]<- ifelse(sum(!is.na(b))>2,shapiro.test(b)$p.value, "ao data" )
              count<- count+1
              print(paste(i,j,k,sep = " "))
            }else{
              statistics[count,1]<- i
              statistics[count,2]<- j
              statistics[count,3]<- k
              statistics[count,4]<- "no data"
              statistics[count,5]<- "no data"
              statistics[count,6]<- "no data"
              count<- count+1
            }
            
          }
          
        }
        
      }
      
      
      #write.csv(statistics,"C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-06 Figures/Statistics_expression.csv" )
      
      
      
      
      
      summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                            conf.interval=.95, .drop=TRUE) {
        library(plyr)
        
        # New version of length which can handle NA's: if na.rm==T, don't count them
        length2 <- function (x, na.rm=FALSE) {
          if (na.rm) sum(!is.na(x))
          else       length(x)
        }
        
        # This does the summary. For each group's data frame, return a vector with
        # N, mean, and sd
        datac <- ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                         c(N    = length2(xx[[col]], na.rm=na.rm),
                           mean = mean   (xx[[col]], na.rm=na.rm),
                           sd   = sd     (xx[[col]], na.rm=na.rm)
                         )
                       },
                       measurevar
        )
        
        # Rename the "mean" column    
        datac <- rename(datac, c("mean" = measurevar))
        
        datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
        
        # Confidence interval multiplier for standard error
        # Calculate t-statistic for confidence interval: 
        # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
        ciMult <- qt(conf.interval/2 + .5, datac$N-1)
        datac$ci <- datac$se * ciMult
        
        return(datac)
      }
      
      
      
      sample<- summarySE(gene_expression_Dt_Dt, measurevar="Fold_gene_expression", groupvars=c("Status","variable", "LEAF", "Cultivar"), na.rm = T)
      
      
      
      
      fig<-ggplot(filter(gene_expression_Dt_Dt, Cultivar=="PA"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar))+
        geom_bar(data=filter(sample, Cultivar=="PA"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar), color= "#343741",outlier.shape = NA, stat = "identity",position = position_dodge())+
        geom_point(aes(x=Status, y=Fold_gene_expression, fill=Cultivar), colour = "#D8D4D7", size = 0.15, position=position_jitterdodge())+
        geom_errorbar(data=filter(sample, Cultivar=="PA"), aes(ymin=Fold_gene_expression-se, ymax=Fold_gene_expression+se), position = position_dodge(width = 0.5), alpha=0.9, size=0.5, width=0.4)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "t.test", map_signif_level = TRUE)+
        facet_grid(variable~LEAF,  scales = "free_y")+
        chartTheme+
        theme(axis.text.x = element_text(angle = 45))+
        scale_fill_manual(values = c("#0021A5", "#FA4616"))
      
      # 
      # #Save PPT
      # p_dml <- rvg::dml(ggobj = fig)
      # # initialize PowerPoint slide 
      # officer::read_pptx() %>%
      #   # add slide 
      # officer::add_slide() %>%
      #   # specify object and location of object 
      # officer::ph_with(p_dml, ph_location()) %>%
      #   # export slide -
      # base::print(
      #   target = "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-11 Figures/Expression_CAllose_PA_healthy_infected 2022-12-20.pptx"
      # )
      
      
      
      
      
      
      fig<-ggplot(filter(gene_expression_Dt_Dt, Cultivar=="SB"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar))+
        geom_bar(data=filter(sample, Cultivar=="SB"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar), color= "#343741",outlier.shape = NA, stat = "identity",position = position_dodge())+
        geom_point(aes(x=Status, y=Fold_gene_expression, fill=Cultivar), colour = "#D8D4D7", size = 0.15, position=position_jitterdodge())+
        geom_errorbar(data=filter(sample, Cultivar=="SB"), aes(ymin=Fold_gene_expression-se, ymax=Fold_gene_expression+se), position = position_dodge(width = 0.5), alpha=0.9, size=0.5, width=0.4)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "t.test", map_signif_level = TRUE)+
        facet_grid(variable~LEAF,  scales = "free_y")    +
        chartTheme+
        theme(axis.text.x = element_text(angle = 45))+
        scale_fill_manual(values = c("#FA4616"))
      
      # 
      # #Save PPT
      # p_dml <- rvg::dml(ggobj = fig)
      # # initialize PowerPoint slide 
      # officer::read_pptx() %>%
      #   # add slide 
      # officer::add_slide() %>%
      #   # specify object and location of object 
      # officer::ph_with(p_dml, ph_location()) %>%
      #   # export slide -
      # base::print(
      #   target = "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-11 Figures/Expression_CAllose_SB_healthy_infected 2022-12-20.pptx"
      # )
      # 
      # 
      
      
      sample<- summarySE(gene_expression_Dt_Dt, measurevar="Fold_gene_expression", groupvars=c("Status","variable","Cultivar"), na.rm = T)
      
      fig<-ggplot(filter(gene_expression_Dt_Dt, Cultivar=="PA"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar))+
        geom_bar(data=filter(sample, Cultivar=="PA"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar), color= "#343741",outlier.shape = NA, stat = "identity",position = position_dodge())+
        geom_point(aes(x=Status, y=Fold_gene_expression, color=LEAF, shape=LEAF), size = 0.3, position=position_jitterdodge())+
        geom_errorbar(data=filter(sample, Cultivar=="PA"), aes(ymin=Fold_gene_expression-se, ymax=Fold_gene_expression+se), position = position_dodge(width = 0.5), alpha=0.9, size=0.5, width=0.4)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "wilcox.test", map_signif_level = F)+
        facet_grid(variable~Cultivar,  scales = "free_y")+
        chartTheme+
        theme(axis.text.x = element_text(angle = 45))+
        scale_fill_manual(values = c("#0021A5", "#0021A5"))+
        scale_color_manual(values = c("#A9A9A9", "#6082B6", "black"))
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="Healthy", variable=="Dt_CsCALS3")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="HLB+", variable=="Dt_CsCALS3")$Fold_gene_expression)
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="Healthy", variable=="Dt_CsCALS7")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="HLB+", variable=="Dt_CsCALS7")$Fold_gene_expression)
      
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="Healthy", variable=="Dt_CsCALS9")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="HLB+", variable=="Dt_CsCALS9")$Fold_gene_expression)
      
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="Healthy", variable=="Dt_CsCALS11")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="HLB+", variable=="Dt_CsCALS11")$Fold_gene_expression)
      
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="Healthy", variable=="Dt_CsCALS12")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="HLB+", variable=="Dt_CsCALS12")$Fold_gene_expression)
      
      
      # 
      # #Save PPT
      # p_dml <- rvg::dml(ggobj = fig)
      # # initialize PowerPoint slide 
      # officer::read_pptx() %>%
      #   # add slide 
      # officer::add_slide() %>%
      #   # specify object and location of object 
      # officer::ph_with(p_dml, ph_location()) %>%
      #   # export slide -
      # base::print(
      #   target = "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-11 Figures/Expression_CAllose_PA_ages_mixed 2022-12-20.pptx"
      # )
      
      
      
      
      fig<-ggplot(filter(gene_expression_Dt_Dt, Cultivar=="SB"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar))+
        geom_bar(data=filter(sample, Cultivar=="SB"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar), color= "#343741",outlier.shape = NA, stat = "identity",position = position_dodge())+
        geom_point(aes(x=Status, y=Fold_gene_expression, color=LEAF, shape=LEAF), size = 0.3, position=position_jitterdodge())+
        geom_errorbar(data=filter(sample, Cultivar=="SB"), aes(ymin=Fold_gene_expression-se, ymax=Fold_gene_expression+se), position = position_dodge(width = 0.5), alpha=0.9, size=0.5, width=0.4)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "t.test", map_signif_level = F)+
        facet_grid(variable~Cultivar,  scales = "free_y")+
        chartTheme+
        theme(axis.text.x = element_text(angle = 45))+
        scale_fill_manual(values = c("#FA4616", "#FA4616"))+
        scale_color_manual(values = c("#A9A9A9", "#6082B6", "black"))
      gene_expression_CALLOSE<- gene_expression_Dt_Dt
      SAMPLE_CALLOSE<- sample
      
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="Healthy", variable=="Dt_CsCALS3")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="HLB+", variable=="Dt_CsCALS3")$Fold_gene_expression)
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="Healthy", variable=="Dt_CsCALS7")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="HLB+", variable=="Dt_CsCALS7")$Fold_gene_expression)
      
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="Healthy", variable=="Dt_CsCALS9")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="HLB+", variable=="Dt_CsCALS9")$Fold_gene_expression)
      
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="Healthy", variable=="Dt_CsCALS11")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="HLB+", variable=="Dt_CsCALS11")$Fold_gene_expression)
      
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="Healthy", variable=="Dt_CsCALS12")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="HLB+", variable=="Dt_CsCALS12")$Fold_gene_expression)
      
    
    # 
    #   #Save PPT
    #   p_dml <- rvg::dml(ggobj = fig)
    #   # initialize PowerPoint slide 
    #   officer::read_pptx() %>%
    #     # add slide 
    #   officer::add_slide() %>%
    #     # specify object and location of object 
    #   officer::ph_with(p_dml, ph_location()) %>%
    #     # export slide -
    #   base::print(
    #     target = "C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-11 Figures/Expression_CAllose_SB_ages_mixed_2022-12-20.pptx"
    #   )
      
      #permutations CALL3
      
      perm_callose_3<- gene_expression_Dt_Dt %>%
        filter(variable=="Dt_CsCALS3") %>%
        select(Cultivar, Fold_gene_expression, Status)
      average_cultivars_and_Status<- aggregate(perm_callose_3$Fold_gene_expression, list(perm_callose_3$Cultivar, perm_callose_3$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_callose_3$Cultivar, Status= perm_callose_3$Status, Fold_gene_expression= c(sample(filter(perm_callose_3, Cultivar=="PA")$Fold_gene_expression, replace = F), sample(filter(perm_callose_3, Cultivar=="SB")$Fold_gene_expression, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$Fold_gene_expression, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      Call_3<- data.frame(variable=rep("CALS3", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      
      #permutations CALL7
      
      perm_callose_7<- gene_expression_Dt_Dt %>%
        filter(variable=="Dt_CsCALS7") %>%
        select(Cultivar, Fold_gene_expression, Status)
      average_cultivars_and_Status<- aggregate(perm_callose_7$Fold_gene_expression, list(perm_callose_7$Cultivar, perm_callose_7$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_callose_7$Cultivar, Status= perm_callose_7$Status, Fold_gene_expression= c(sample(filter(perm_callose_7, Cultivar=="PA")$Fold_gene_expression, replace = F), sample(filter(perm_callose_7, Cultivar=="SB")$Fold_gene_expression, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$Fold_gene_expression, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      Call_7<- data.frame(variable=rep("CALS7", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      #permutations CALL9
      
      perm_callose_9<- gene_expression_Dt_Dt %>%
        filter(variable=="Dt_CsCALS9") %>%
        select(Cultivar, Fold_gene_expression, Status)
      average_cultivars_and_Status<- aggregate(perm_callose_9$Fold_gene_expression, list(perm_callose_9$Cultivar, perm_callose_9$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_callose_9$Cultivar, Status= perm_callose_9$Status, Fold_gene_expression= c(sample(filter(perm_callose_9, Cultivar=="PA")$Fold_gene_expression, replace = F), sample(filter(perm_callose_9, Cultivar=="SB")$Fold_gene_expression, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$Fold_gene_expression, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      Call_9<- data.frame(variable=rep("CALS9", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      #permutations CALL11
      
      perm_callose_11<- gene_expression_Dt_Dt %>%
        filter(variable=="Dt_CsCALS11") %>%
        select(Cultivar, Fold_gene_expression, Status)
      average_cultivars_and_Status<- aggregate(perm_callose_11$Fold_gene_expression, list(perm_callose_11$Cultivar, perm_callose_11$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_callose_11$Cultivar, Status= perm_callose_11$Status, Fold_gene_expression= c(sample(filter(perm_callose_11, Cultivar=="PA")$Fold_gene_expression, replace = F), sample(filter(perm_callose_11, Cultivar=="SB")$Fold_gene_expression, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$Fold_gene_expression, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      Call_11<- data.frame(variable=rep("CALS11", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      #permutations CALL12
      
      perm_callose_12<- gene_expression_Dt_Dt %>%
        filter(variable=="Dt_CsCALS12") %>%
        select(Cultivar, Fold_gene_expression, Status)
      average_cultivars_and_Status<- aggregate(perm_callose_12$Fold_gene_expression, list(perm_callose_12$Cultivar, perm_callose_12$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_callose_12$Cultivar, Status= perm_callose_12$Status, Fold_gene_expression= c(sample(filter(perm_callose_12, Cultivar=="PA")$Fold_gene_expression, replace = F), sample(filter(perm_callose_12, Cultivar=="SB")$Fold_gene_expression, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$Fold_gene_expression, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      Call_12<- data.frame(variable=rep("CALS12", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      #write.csv(rbind(Call_3,Call_7, Call_9, Call_11, Call_12), "C:/Users/jrobl/OneDrive - University of Florida/Desktop/res_call_area.csv", row.names = F)
      
      
      
      
    #GPI, glucanases and Ferr----
      setwd("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/")
      chartTheme <- theme(plot.title=element_text(size=20, face = "bold", hjust = 0.5),
                          axis.title.x=element_text(size=9, face = "bold"), 
                          axis.title.y=element_text(size=9, face = "bold"),
                          axis.text.x=element_text(size=9, angle= 0, vjust=0.5),
                          axis.text.y=element_text(size=9),
                          panel.grid.minor.y=element_line(linetype="dotted"),
                          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                          plot.background=element_rect(linetype="blank"),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(), 
                          axis.line = element_line(colour = "black"),
                          legend.key = element_rect(fill = "white"))
      
      
      library(tidyverse)
      library(reshape2)
      setwd("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/")
      gene_expression_means_others<- read.csv("Data/SYBR Results summary means 2022-10-18.csv")
      gene_expression_means_others<- gene_expression_means_others[,-6]
      gene_expression_means_GLU<- read.csv("Data/SYBR Results summary means 2022-12-01.csv")
      gene_expression_means_GLU<- gene_expression_means_GLU[,-6]
      gene_expression_means<- inner_join(gene_expression_means_others, gene_expression_means_GLU)
      gene_expression_means<- select(gene_expression_means, Cultivar, Status, Sample.ID, LEAF, Dt_Ferr, Dt_GPI, GLU.1..41.42., GLU.2..65.66.)
      gene_expression_Dt_Dt<- melt(data =gene_expression_means, id = c("Cultivar","Status", "Sample.ID","LEAF"))
      aritmetic_means_healthy<- aggregate(filter(gene_expression_Dt_Dt, Status=="Healthy")$value, list(filter(gene_expression_Dt_Dt, Status=="Healthy")$Cultivar, filter(gene_expression_Dt_Dt, Status=="Healthy")$LEAF, filter(gene_expression_Dt_Dt, Status=="Healthy")$variable), mean, na.rm=T)
      colnames(aritmetic_means_healthy)<- c("Cultivar", "LEAF", "variable", "arti_mean_healthy")
      gene_expression_Dt_Dt<- full_join(aritmetic_means_healthy,gene_expression_Dt_Dt)
      gene_expression_Dt_Dt$dt_dt_ct<- gene_expression_Dt_Dt$value - gene_expression_Dt_Dt$arti_mean_healthy
      #gene_expression_Dt_Dt$dt_dt_ct_G<- gene_expression_Dt_Dt$value - gene_expression_Dt_Dt$geom_mean_healthy
      gene_expression_Dt_Dt$Fold_gene_expression<- 2^-(gene_expression_Dt_Dt$dt_dt_ct)
      #gene_expression_Dt_Dt$Fold_gene_expression_G<- 2^-(gene_expression_Dt_Dt$dt_dt_ct_G)
      
      
      
      
      
      count<- 1
      statistics<- matrix(ncol=6,nrow=30)
      colnames(statistics)<- c("Cultivar", "LEAF","Gene","T test P value", "shap healt P value", "shap HLB+ P value")
      
      
      
      
      # 
      # for (i in unique(gene_expression_Dt_Dt$Cultivar)) {
      #   for (j in unique(gene_expression_Dt_Dt$LEAF)) {
      #     for (k in unique(gene_expression_Dt_Dt$variable)) {
      #       a<- filter(gene_expression_Dt_Dt, Cultivar==i, LEAF==j, variable==k, Status=="Healthy")$Fold_gene_expression
      #       b<- filter(gene_expression_Dt_Dt, Cultivar==i, LEAF==j, variable==k, Status=="HLB+")$Fold_gene_expression
      #       if((sum(!is.na(a))+sum(!is.na(b)))>3){
      #         statistics[count,1]<- i
      #         statistics[count,2]<- j
      #         statistics[count,3]<- k
      #         statistics[count,4]<- t.test(a,b)$p.value
      #         statistics[count,5]<- ifelse(sum(!is.na(a))>2,shapiro.test(a)$p.value, "ao data" )
      #         statistics[count,6]<- ifelse(sum(!is.na(b))>2,shapiro.test(b)$p.value, "ao data" )
      #         count<- count+1
      #         print(paste(i,j,k,sep = " "))
      #       }else{
      #         statistics[count,1]<- i
      #         statistics[count,2]<- j
      #         statistics[count,3]<- k
      #         statistics[count,4]<- "no data"
      #         statistics[count,5]<- "no data"
      #         statistics[count,6]<- "no data"
      #         count<- count+1
      #       }
      # 
      #     }
      # 
      #   }
      # 
      # }
      # 
      
      
      
      
      
      
      for (i in unique(gene_expression_Dt_Dt$Cultivar)) {
        for (j in unique(gene_expression_Dt_Dt$LEAF)) {
          for (k in unique(gene_expression_Dt_Dt$variable)) {
            a<- filter(gene_expression_Dt_Dt, Cultivar==i, LEAF==j, variable==k, Status=="Healthy")$Fold_gene_expression
            b<- filter(gene_expression_Dt_Dt, Cultivar==i, LEAF==j, variable==k, Status=="HLB+")$Fold_gene_expression
            if(sum(!is.na(a))>1 & sum(!is.na(b))>1){
              statistics[count,1]<- i
              statistics[count,2]<- j
              statistics[count,3]<- k
              statistics[count,4]<- t.test(a,b)$p.value
              statistics[count,5]<- ifelse(sum(!is.na(a))>2,shapiro.test(a)$p.value, "ao data" )
              statistics[count,6]<- ifelse(sum(!is.na(b))>2,shapiro.test(b)$p.value, "ao data" )
              count<- count+1
              print(paste(i,j,k,sep = " "))
            }else{
              statistics[count,1]<- i
              statistics[count,2]<- j
              statistics[count,3]<- k
              statistics[count,4]<- "no data"
              statistics[count,5]<- "no data"
              statistics[count,6]<- "no data"
              count<- count+1
            }
            
          }
          
        }
        
      }
      
      
      #write.csv(statistics,"C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-06 Figures/Statistics_expression.csv" )
      
      
      
      
      
      summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                            conf.interval=.95, .drop=TRUE) {
        library(plyr)
        
        # New version of length which can handle NA's: if na.rm==T, don't count them
        length2 <- function (x, na.rm=FALSE) {
          if (na.rm) sum(!is.na(x))
          else       length(x)
        }
        
        # This does the summary. For each group's data frame, return a vector with
        # N, mean, and sd
        datac <- ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                         c(N    = length2(xx[[col]], na.rm=na.rm),
                           mean = mean   (xx[[col]], na.rm=na.rm),
                           sd   = sd     (xx[[col]], na.rm=na.rm)
                         )
                       },
                       measurevar
        )
        
        # Rename the "mean" column    
        datac <- rename(datac, c("mean" = measurevar))
        
        datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
        
        # Confidence interval multiplier for standard error
        # Calculate t-statistic for confidence interval: 
        # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
        ciMult <- qt(conf.interval/2 + .5, datac$N-1)
        datac$ci <- datac$se * ciMult
        
        return(datac)
      }
      
      
      
      sample<- summarySE(gene_expression_Dt_Dt, measurevar="Fold_gene_expression", groupvars=c("Status","variable", "LEAF", "Cultivar"), na.rm = T)
      
      
      
      
      fig<-ggplot(filter(gene_expression_Dt_Dt, Cultivar=="PA"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar))+
        geom_bar(data=filter(sample, Cultivar=="PA"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar), color= "#343741",outlier.shape = NA, stat = "identity",position = position_dodge())+
        geom_point(aes(x=Status, y=Fold_gene_expression, fill=Cultivar), colour = "#D8D4D7", size = 0.15, position=position_jitterdodge())+
        geom_errorbar(data=filter(sample, Cultivar=="PA"), aes(ymin=Fold_gene_expression-se, ymax=Fold_gene_expression+se), position = position_dodge(width = 0.5), alpha=0.9, size=0.5, width=0.4)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "wilcox.test", map_signif_level = TRUE)+
        facet_grid(variable~LEAF,  scales = "free_y")+
        chartTheme+
        theme(axis.text.x = element_text(angle = 45))+
        scale_fill_manual(values = c("#0021A5", "#FA4616"))
      
    # 
    #   #Save PPT
    #   p_dml <- rvg::dml(ggobj = fig)
    #   # initialize PowerPoint slide
    #   officer::read_pptx() %>%
    #     # add slide
    #   officer::add_slide() %>%
    #     # specify object and location of object
    #   officer::ph_with(p_dml, ph_location()) %>%
    #     # export slide -
    #   base::print(
    #     target = "C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/Expression_Fer_GPI_GLU_PA_healthy_infected 2022-12-20.pptx"
    #   )
    
      
      
      
      
      
      fig<-ggplot(filter(gene_expression_Dt_Dt, Cultivar=="SB"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar))+
        geom_bar(data=filter(sample, Cultivar=="SB"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar), color= "#343741",outlier.shape = NA, stat = "identity",position = position_dodge())+
        geom_point(aes(x=Status, y=Fold_gene_expression, fill=Cultivar), colour = "#D8D4D7", size = 0.15, position=position_jitterdodge())+
        geom_errorbar(data=filter(sample, Cultivar=="SB"), aes(ymin=Fold_gene_expression-se, ymax=Fold_gene_expression+se), position = position_dodge(width = 0.5), alpha=0.9, size=0.5, width=0.4)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "t.test", map_signif_level = TRUE)+
        facet_grid(variable~LEAF,  scales = "free_y")    +
        chartTheme+
        theme(axis.text.x = element_text(angle = 45))+
        scale_fill_manual(values = c("#FA4616"))
      
      # 
      # #Save PPT
      # p_dml <- rvg::dml(ggobj = fig)
      # # initialize PowerPoint slide 
      # officer::read_pptx() %>%
      #   # add slide 
      #   officer::add_slide() %>%
      #   # specify object and location of object 
      #   officer::ph_with(p_dml, ph_location()) %>%
      #   # export slide -
      #   base::print(
      #     target = "C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/Expression_Fer_GPI_GLU_SB_healthy_infected 2022-12-20.pptx"
      #   )
      # 
      # 
      
      
      sample<- summarySE(gene_expression_Dt_Dt, measurevar="Fold_gene_expression", groupvars=c("Status","variable","Cultivar"), na.rm = T)
      
      fig<-ggplot(filter(gene_expression_Dt_Dt, Cultivar=="PA"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar))+
        geom_bar(data=filter(sample, Cultivar=="PA"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar), color= "#343741",outlier.shape = NA, stat = "identity",position = position_dodge())+
        geom_point(aes(x=Status, y=Fold_gene_expression, color=LEAF, shape=LEAF), size = 0.3, position=position_jitterdodge())+
        geom_errorbar(data=filter(sample, Cultivar=="PA"), aes(ymin=Fold_gene_expression-se, ymax=Fold_gene_expression+se), position = position_dodge(width = 0.5), alpha=0.9, size=0.5, width=0.4)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "wilcox.test", map_signif_level = F)+
        facet_grid(variable~Cultivar,  scales = "free_y")+
        chartTheme+
        theme(axis.text.x = element_text(angle = 45))+
        scale_fill_manual(values = c("#0021A5", "#0021A5"))+
        scale_color_manual(values = c("#A9A9A9", "#6082B6", "black"))
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="Healthy", variable=="Dt_Ferr")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="HLB+", variable=="Dt_Ferr")$Fold_gene_expression)
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="Healthy", variable=="Dt_GPI")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="HLB+", variable=="Dt_GPI")$Fold_gene_expression)
      
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="Healthy", variable=="GLU.1..41.42.")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="HLB+", variable=="GLU.1..41.42.")$Fold_gene_expression)
      
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="Healthy", variable=="GLU.2..65.66.")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="HLB+", variable=="GLU.2..65.66.")$Fold_gene_expression)
      
      names(gene_expression_Dt_Dt)[names(gene_expression_Dt_Dt)=='LEAF']<- 'Leaf age'
      
      figure_1_B<-ggplot(filter(gene_expression_Dt_Dt, variable=="Dt_Ferr"), aes(x=Status, y=Fold_gene_expression))+
        geom_bar(data=filter(sample, variable=="Dt_Ferr"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar), color= "#343741",outlier.shape = NA, stat = "identity",position = position_dodge())+
        geom_point(aes(x=Status, y=Fold_gene_expression, color=`Leaf age`, shape=`Leaf age`), size = 1.4, position=position_jitterdodge())+
        geom_errorbar(data=filter(sample, variable=="Dt_Ferr"), aes(ymin=Fold_gene_expression-se, ymax=Fold_gene_expression+se), position = position_dodge(width = 0.5), alpha=0.9, size=0.5, width=0.1)+
        facet_grid(~Cultivar)+
        theme(legend.text = element_text(size=7))+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "t.test", map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",paste("P= ",round(p,2)))},textsize = 2.5)+
        chartTheme+
        scale_color_manual(values = c("#A9A9A9", "#6082B6", "black"))+
        scale_fill_manual(values=c("#003074", "#F57330"))+
        guides(fill=FALSE)+
        ylab(bquote(atop(Relative~Expression~"("*Fold-change*")"~phantom(), "("*Infected/Healthy*")"~of~Ferredoxin-NADP)))+
        chartTheme+
        coord_cartesian(ylim = c(0,10.8))+
        theme(axis.title.x = element_blank())
     
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="Healthy", variable=="Dt_Ferr")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="HLB+", variable=="Dt_Ferr")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="Healthy", variable=="Dt_Ferr")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="HLB+", variable=="Dt_Ferr")$Fold_gene_expression)
      
    
    # 
    #   #Save PPT
    #   p_dml <- rvg::dml(ggobj = fig)
    #   # initialize PowerPoint slide
    #   officer::read_pptx() %>%
    #     # add slide
    #   officer::add_slide() %>%
    #     # specify object and location of object
    #   officer::ph_with(p_dml, ph_location()) %>%
    #     # export slide -
    #   base::print(
    #     target = "C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/Expression_Fer_GPI_GLU_PA_AGES_MIXED_healthy_infected 2022-12-20.pptx"
    #   )
    # 
    #   
    #   
      
      fig<-ggplot(filter(gene_expression_Dt_Dt, Cultivar=="SB"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar))+
        geom_bar(data=filter(sample, Cultivar=="SB"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar), color= "#343741",outlier.shape = NA, stat = "identity",position = position_dodge())+
        geom_point(aes(x=Status, y=Fold_gene_expression, color=LEAF, shape=LEAF), size = 0.3, position=position_jitterdodge())+
        geom_errorbar(data=filter(sample, Cultivar=="SB"), aes(ymin=Fold_gene_expression-se, ymax=Fold_gene_expression+se), position = position_dodge(width = 0.5), alpha=0.9, size=0.5, width=0.4)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "wilcox.test", map_signif_level = F)+
        facet_grid(variable~Cultivar,  scales = "free_y")+
        chartTheme+
        theme(axis.text.x = element_text(angle = 45))+
        scale_fill_manual(values = c("#FA4616", "#FA4616"))+
        scale_color_manual(values = c("#A9A9A9", "#6082B6", "black"))
      
      
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="Healthy", variable=="Dt_Ferr")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="HLB+", variable=="Dt_Ferr")$Fold_gene_expression)
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="Healthy", variable=="Dt_GPI")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="HLB+", variable=="Dt_GPI")$Fold_gene_expression)
      
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="Healthy", variable=="GLU.1..41.42.")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="HLB+", variable=="GLU.1..41.42.")$Fold_gene_expression)
      
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="Healthy", variable=="GLU.2..65.66.")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="HLB+", variable=="GLU.2..65.66.")$Fold_gene_expression)
    
        # 
        # 
        # #Save PPT
        # p_dml <- rvg::dml(ggobj = fig)
        # # initialize PowerPoint slide
        # officer::read_pptx() %>%
        #   # add slide
        # officer::add_slide() %>%
        #   # specify object and location of object
        # officer::ph_with(p_dml, ph_location()) %>%
        #   # export slide -
        # base::print(
        #   target = "C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/Expression_Fer_GPI_GLU_SB_AGES_MIXED_healthy_infected 2022-12-20.pptx"
        # )
      
      gene_expression_GLU<- gene_expression_Dt_Dt
      sample_glu<- sample
      
      #permutations Dt_Ferr
      
      perm_Ferr<- gene_expression_Dt_Dt %>%
        filter(variable=="Dt_Ferr") %>%
        select(Cultivar, Fold_gene_expression, Status)
      average_cultivars_and_Status<- aggregate(perm_Ferr$Fold_gene_expression, list(perm_Ferr$Cultivar, perm_Ferr$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_Ferr$Cultivar, Status= perm_Ferr$Status, Fold_gene_expression= c(sample(filter(perm_Ferr, Cultivar=="PA")$Fold_gene_expression, replace = F), sample(filter(perm_Ferr, Cultivar=="SB")$Fold_gene_expression, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$Fold_gene_expression, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      ferr<- data.frame(variable=rep("Ferr", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      
      #permutations Dt_GPI
      
      perm_GPI<- gene_expression_Dt_Dt %>%
        filter(variable=="Dt_GPI") %>%
        select(Cultivar, Fold_gene_expression, Status)
      average_cultivars_and_Status<- aggregate(perm_GPI$Fold_gene_expression, list(perm_GPI$Cultivar, perm_GPI$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_GPI$Cultivar, Status= perm_GPI$Status, Fold_gene_expression= c(sample(filter(perm_GPI, Cultivar=="PA")$Fold_gene_expression, replace = F), sample(filter(perm_GPI, Cultivar=="SB")$Fold_gene_expression, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$Fold_gene_expression, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      GPI<- data.frame(variable=rep("GPI", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      #permutations GLU.1..41.42.
      
      PERM_GLU_1<- gene_expression_Dt_Dt %>%
        filter(variable=="GLU.1..41.42.") %>%
        select(Cultivar, Fold_gene_expression, Status)
      average_cultivars_and_Status<- aggregate(PERM_GLU_1$Fold_gene_expression, list(PERM_GLU_1$Cultivar, PERM_GLU_1$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= PERM_GLU_1$Cultivar, Status= PERM_GLU_1$Status, Fold_gene_expression= c(sample(filter(PERM_GLU_1, Cultivar=="PA")$Fold_gene_expression, replace = F), sample(filter(PERM_GLU_1, Cultivar=="SB")$Fold_gene_expression, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$Fold_gene_expression, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      GLU_1<- data.frame(variable=rep("GLU.1..41.42.", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      #permutations GLU.2..65.66.
      
      PERM_GLU_2<- gene_expression_Dt_Dt %>%
        filter(variable=="GLU.2..65.66.") %>%
        select(Cultivar, Fold_gene_expression, Status)
      average_cultivars_and_Status<- aggregate(PERM_GLU_2$Fold_gene_expression, list(PERM_GLU_2$Cultivar, PERM_GLU_2$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= PERM_GLU_2$Cultivar, Status= PERM_GLU_2$Status, Fold_gene_expression= c(sample(filter(PERM_GLU_2, Cultivar=="PA")$Fold_gene_expression, replace = F), sample(filter(PERM_GLU_2, Cultivar=="SB")$Fold_gene_expression, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$Fold_gene_expression, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      GLU_2<- data.frame(variable=rep("GLU_2", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      write.csv(rbind(ferr, GPI, GLU_1, GLU_2), "C:/Users/jrobl/OneDrive - University of Florida/Desktop/res_fer_glu_GPI.csv", row.names = F)
      
      
      
    #plot permutations-----
      #----chart theme
      chartTheme <- theme(plot.title=element_text(size=20, face = "bold", hjust = 0.5),
                          axis.title.x=element_text(size=18, face = "bold"), 
                          axis.title.y=element_text(size=18, face = "bold"),
                          axis.text.x=element_text(size=14, angle= 0, vjust=0.5),
                          axis.text.y=element_text(size=14),
                          panel.grid.minor.y=element_line(linetype="dotted"),
                          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                          plot.background=element_rect(linetype="blank"),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(), 
                          axis.line = element_line(colour = "black"),
                          legend.key = element_rect(fill = "white"))
      setwd("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/OBJ 1 Physiology Permutations/")
      library(tidyverse)
      df <-
        list.files(path = "./", pattern = "*.csv") %>% 
        map_df(~read_csv(.))
      df
      df$variable<- factor(df$variable, levels = c("fix", "Ferr", "diff","Stomatal Conductance", "Starch", "CALL counts","CALL mean size", "CALL area", "CALL distribution", "CALS3", "CALS7", "CALS9", "CALS11", 
                                                   "CALS12", "GLU.1..41.42.", "GLU_2","GPI"))
      final_physiology<- ggplot(df, aes(x=variable, y=log(value_inf_vs_healthy, base=2), fill=Cultivar))+
        geom_bar(stat = 'identity')+
        scale_fill_manual(values=c("#003074", "#F57330"))+
        geom_text(aes(label=p_value, y=2.2))+
        facet_grid(~Cultivar)+
        coord_cartesian(expand = T, ylim = c(-3,4))+
        coord_flip()+
        chartTheme
    
    # 
    #   #Save PPT
    #   p_dml <- rvg::dml(ggobj = final_physiology)
    #   # initialize PowerPoint slide
    #   officer::read_pptx() %>%
    #     # add slide
    #   officer::add_slide() %>%
    #     # specify object and location of object
    #   officer::ph_with(p_dml, ph_location()) %>%
    #     # export slide -
    #   base::print(
    #     target = "C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/FINAL_Physiology.pptx"
    #   )
    
#ANATOMY----
    #petioles----
      library(tidyverse)
      library(reshape)
      library(ggpubr)
      library(rvg)
      library(officer)
      chartTheme <- theme(plot.title=element_text(size=20, face = "bold", hjust = 0.5),
                          axis.title.x=element_text(size=9, face = "bold"), 
                          axis.title.y=element_text(size=9, face = "bold"),
                          axis.text.x=element_text(size=9, angle= 0, vjust=0.5),
                          axis.text.y=element_text(size=9),
                          panel.grid.minor.y=element_line(linetype="dotted"),
                          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                          plot.background=element_rect(linetype="blank"),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(), 
                          axis.line = element_line(colour = "black"),
                          legend.key = element_rect(fill = "white"))
      
      
      anatomy<- read.csv("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/ANAT_OBJ_1_2022-09-23.csv")
      id_samples<- read.csv("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/Samples IDs and Comments.csv", skip = 7)
      tree.codes<- read.csv("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/Tree codes.csv")
      anatomy<- inner_join(id_samples, anatomy)
      anatomy<- inner_join(tree.codes, anatomy)
      
      
      anatomy<- anatomy[,-c(1, 4:6, 8:12,14, 15)]
      anatomy_measumetns<- anatomy[,-c(6,8)]
      anatomy_percentage<- data.frame(anatomy_measumetns[,1:3],anatomy[,c(6,8)])
      anatomy_measumetns<- melt(anatomy_measumetns, id=1:3)
      anatomy_percentage<- melt(anatomy_percentage, id=1:3)
      anatomy<- melt(anatomy, id=1:3)
      anatomy$value<- sqrt(anatomy$value)
      
      #levels(anatomy_measumetns$variable)<- list(Pith="Pith.area..um.2.", Xylem="xylem.area..um.2.", Phloem= "phloem.area..um.2.")
      units<- ggplot(anatomy_measumetns, aes(x=Status, y=log(value)))+
        geom_violin(trim = FALSE, color= "#C7C9C8", aes(fill=Cultivar))+
        geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
        geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 1.4, position=position_jitter(h=0.1, w=0.1), alpha=0.5)+
        stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.30, alpha=0.6)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "wilcox.test", map_signif_level = T)+
        facet_grid(rows= vars(Cultivar), cols = vars(variable))+
        guides(fill=FALSE)+
        scale_fill_manual(values=c("#003074", "#F57330"))+
        chartTheme+
        theme(axis.title.x =element_blank())+
        ylab(bquote(Log~Area~(~mu*m^2)))
      
      
      
      # 
      # 
      # p_dml <- rvg::dml(ggobj = units)
      # officer::read_pptx() %>%
      #   # add slide
      #   officer::add_slide() %>%
      #   # specify object and location of object
      #   officer::ph_with(p_dml, ph_location()) %>%
      #   # export slide
      #   base::print(
      #     target = "C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/AN_Petiole_units.pptx"
      #   )
      
      
      #levels(anatomy_percentage$variable)<- list(Xylem="xylem.area....", Phloem= "phloem.area....")
      percentage<- ggplot(anatomy_percentage, aes(x=Status, y=value))+
        geom_violin(trim = FALSE, color= "#C7C9C8", aes(fill=Cultivar))+
        geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
        geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 1.4, position=position_jitter(h=0.1, w=0.1), alpha=0.5)+
        stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.30, alpha=0.6)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "wilcox.test", map_signif_level = T)+
        facet_grid(rows= vars(Cultivar), cols = vars(variable))+
        guides(fill=FALSE)+
        scale_fill_manual(values=c("#003074", "#F57330"))+
        chartTheme+
        theme(axis.title.x =element_blank())+
        ylab(bquote(Proportional~Area~("%")))
      
      
      
      
      # 
      # 
      # 
      # p_dml <- rvg::dml(ggobj = percentage)
      # officer::read_pptx() %>%
      #   # add slide
      #   officer::add_slide() %>%
      #   # specify object and location of object
      #   officer::ph_with(p_dml, ph_location()) %>%
      #   # export slide
      #   base::print(
      #   target = "C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/AN_Petiole_percentages.pptx"
      #   )
      # 
      
      #permutations Pith.area..um.2.
      
      perm_pith<- anatomy_measumetns %>%
        filter(variable=="Pith.area..um.2.") %>%
        select(Cultivar, value, Status)
      average_cultivars_and_Status<- aggregate(perm_pith$value, list(perm_pith$Cultivar, perm_pith$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_pith$Cultivar, Status= perm_pith$Status, value= c(sample(filter(perm_pith, Cultivar=="PA")$value, replace = F), sample(filter(perm_pith, Cultivar=="SB")$value, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$value, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      pith_petiole<- data.frame(variable=rep("pith_petiole", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      
      #permutations xylem.area..um.2.
      perm_xylem<- anatomy_measumetns %>%
        filter(variable=="xylem.area..um.2.") %>%
        select(Cultivar, value, Status)
      average_cultivars_and_Status<- aggregate(perm_xylem$value, list(perm_xylem$Cultivar, perm_xylem$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_xylem$Cultivar, Status= perm_xylem$Status, value= c(sample(filter(perm_xylem, Cultivar=="PA")$value, replace = F), sample(filter(perm_xylem, Cultivar=="SB")$value, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$value, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      xylem_petiole<- data.frame(variable=rep("xylem_petiole", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      #permutations phloem.area..um.2.

      perm_phloem<- anatomy_measumetns %>%
        filter(variable=="phloem.area..um.2.") %>%
        select(Cultivar, value, Status)
      average_cultivars_and_Status<- aggregate(perm_phloem$value, list(perm_phloem$Cultivar, perm_phloem$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_phloem$Cultivar, Status= perm_phloem$Status, value= c(sample(filter(perm_phloem, Cultivar=="PA")$value, replace = F), sample(filter(perm_phloem, Cultivar=="SB")$value, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$value, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      phloem_petiole<- data.frame(variable=rep("phloem_petiole", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      
      #permutations percentage xylem.area....
      
      perm_xylem_per<- anatomy_percentage %>%
        filter(variable=="xylem.area....") %>%
        select(Cultivar, value, Status)
      average_cultivars_and_Status<- aggregate(perm_xylem_per$value, list(perm_xylem_per$Cultivar, perm_xylem_per$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_xylem_per$Cultivar, Status= perm_xylem_per$Status, value= c(sample(filter(perm_xylem_per, Cultivar=="PA")$value, replace = F), sample(filter(perm_xylem_per, Cultivar=="SB")$value, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$value, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      xylem_petiole_per<- data.frame(variable=rep("xylem_petiole_percent", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      #permutations percentage phloem.area....
      
      perm_ploem_per<- anatomy_percentage %>%
        filter(variable=="phloem.area....") %>%
        select(Cultivar, value, Status)
      average_cultivars_and_Status<- aggregate(perm_ploem_per$value, list(perm_ploem_per$Cultivar, perm_ploem_per$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_ploem_per$Cultivar, Status= perm_ploem_per$Status, value= c(sample(filter(perm_ploem_per, Cultivar=="PA")$value, replace = F), sample(filter(perm_ploem_per, Cultivar=="SB")$value, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$value, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      phloem_petiole_per<- data.frame(variable=rep("phloem_petiole_percent", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      
      #write.csv(rbind(pith_petiole, xylem_petiole, phloem_petiole, xylem_petiole_per, phloem_petiole_per), "C:/Users/jrobl/OneDrive - University of Florida/Desktop/res_anatomy_petiole.csv", row.names = F)
      
      
      
    #midrib----
      library(tidyverse)
      library(reshape)
      library(ggpubr)
      library(rvg)
      library(officer)
      
     
      chartTheme <- theme(plot.title=element_text(size=20, face = "bold", hjust = 0.5),
                          axis.title.x=element_text(size=9, face = "bold"), 
                          axis.title.y=element_text(size=9, face = "bold"),
                          axis.text.x=element_text(size=9, angle= 0, vjust=0.5),
                          axis.text.y=element_text(size=9),
                          panel.grid.minor.y=element_line(linetype="dotted"),
                          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                          plot.background=element_rect(linetype="blank"),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(), 
                          axis.line = element_line(colour = "black"),
                          legend.key = element_rect(fill = "white"))
      
      
      
      
      anatomy<- read.csv("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/OBJ_1_Anatomy_midrib.csv")
      anatomy<- separate(anatomy, Sample.ID, into = c("Sample.ID", "delete"),sep = "-")
      anatomy<- anatomy[,-which(colnames(anatomy)=='delete')]
      anatomy$Sample.ID<- as.numeric(anatomy$Sample.ID)
      id_samples<- read.csv("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/Samples IDs and Comments.csv", skip = 7)
      tree.codes<- read.csv("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/Tree codes.csv")
      anatomy<- inner_join(id_samples, anatomy)
      anatomy<- inner_join(tree.codes, anatomy)
      anatomy<- anatomy[,-c(4:11, 13:14)]
      anatomy_measumetns<- melt(anatomy[,-c(6,8)], id=1:3)
      anatomy_percentage<- melt(anatomy[,c(1:3,6,8)], id=1:3)
      
      
      units<- ggplot(anatomy_measumetns, aes(x=Status, y=log(value), fill=Status))+
        geom_violin(trim = FALSE, color= "#C7C9C8")+
        geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
        geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 0.1, position=position_jitter(h=0.1, w=0.1), alpha=0.5)+
        stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.009, alpha=0.6)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "t.test", map_signif_level = F)+
        facet_grid(rows= vars(Cultivar), cols = vars(variable), scales = 'free_y')+
        chartTheme
      
      shapiro.test(log(filter(anatomy, Cultivar=="PA", Status=="Healthy")$Pith.area..um.2.))
      shapiro.test(log(filter(anatomy, Cultivar=="PA", Status=="HLB+")$Pith.area..um.2.))
      
      shapiro.test(log(filter(anatomy, Cultivar=="PA", Status=="Healthy")$xylem.area..um.2.))
      shapiro.test(log(filter(anatomy, Cultivar=="PA", Status=="HLB+")$xylem.area..um.2.))
      
      
      shapiro.test(log(filter(anatomy, Cultivar=="PA", Status=="Healthy")$phloem.area..um.2.))
      shapiro.test(log(filter(anatomy, Cultivar=="PA", Status=="HLB+")$phloem.area..um.2.))
      
      
      shapiro.test(log(filter(anatomy, Cultivar=="SB", Status=="Healthy")$Pith.area..um.2.))
      shapiro.test(log(filter(anatomy, Cultivar=="SB", Status=="HLB+")$Pith.area..um.2.))
      
      shapiro.test(log(filter(anatomy, Cultivar=="SB", Status=="Healthy")$xylem.area..um.2.))
      shapiro.test(log(filter(anatomy, Cultivar=="SB", Status=="HLB+")$xylem.area..um.2.))
      
      
      shapiro.test(log(filter(anatomy, Cultivar=="SB", Status=="Healthy")$phloem.area..um.2.))
      shapiro.test(log(filter(anatomy, Cultivar=="SB", Status=="HLB+")$phloem.area..um.2.))
      
      
      # 
      # 
      # p_dml <- rvg::dml(ggobj = units)
      # officer::read_pptx() %>%
      #   # add slide
      #   officer::add_slide() %>%
      #   # specify object and location of object
      #   officer::ph_with(p_dml, ph_location()) %>%
      #   # export slide
      #   base::print(
      #     target = "C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/AN_midrib_units.pptx"
      #   )
      
      
      
      percentage<- ggplot(anatomy_percentage, aes(x=Status, y=value, fill=Status))+
        geom_violin(trim = FALSE, color= "#C7C9C8")+
        geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
        geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 0.1, position=position_jitter(h=0.1, w=0.1), alpha=0.5)+
        stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.009, alpha=0.6)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "t.test", map_signif_level = F)+
        facet_grid(rows= vars(Cultivar), cols = vars(variable))+
        chartTheme
      
      
      
      shapiro.test(filter(anatomy, Cultivar=="PA", Status=="Healthy")$xylem.area....)
      shapiro.test(filter(anatomy, Cultivar=="PA", Status=="HLB+")$xylem.area....)
      
      shapiro.test(filter(anatomy, Cultivar=="PA", Status=="Healthy")$phloem.area....)
      shapiro.test(filter(anatomy, Cultivar=="PA", Status=="HLB+")$phloem.area....)
      
      
      
      shapiro.test(filter(anatomy, Cultivar=="SB", Status=="Healthy")$xylem.area....)
      shapiro.test(filter(anatomy, Cultivar=="SB", Status=="HLB+")$xylem.area....)
      
      shapiro.test(filter(anatomy, Cultivar=="SB", Status=="Healthy")$phloem.area....)
      shapiro.test(filter(anatomy, Cultivar=="SB", Status=="HLB+")$phloem.area....)
      
      
      # levels(anatomy_measumetns$variable)<- list(Pith="Pith.area..um.2.", Xylem="xylem.area..um.2.", Phloem= "phloem.area..um.2.")
      units<- ggplot(anatomy_measumetns, aes(x=Status, y=log(value)))+
        geom_violin(trim = FALSE, color= "#C7C9C8", aes(fill=Cultivar))+
        geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
        geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 1.2, position=position_jitter(h=0.1, w=0.1), alpha=0.3)+
        stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.30, alpha=0.6)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "t.test", map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",ifelse(p<0.01,"P < 0.01", paste("P= ",round(p,2))))},textsize = 2.5)+
        facet_grid(rows= vars(Cultivar), cols = vars(variable))+
        guides(fill=FALSE)+
        scale_fill_manual(values=c("#003074", "#F57330"))+
        chartTheme+
        theme(axis.title.x =element_blank())+
        ylab(bquote(Log~Area~(~mu*m^2)))
      
      
      # levels(anatomy_percentage$variable)<- list(Xylem="xylem.area....", Phloem= "phloem.area....")
      percentage<- ggplot(anatomy_percentage, aes(x=Status, y=value))+
        geom_violin(trim = FALSE, color= "#C7C9C8", aes(fill=Cultivar))+
        geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
        geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 1.2, position=position_jitter(h=0.1, w=0.1), alpha=0.3)+
        stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.30, alpha=0.6)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "t.test", map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",ifelse(p<0.01,"P < 0.01", paste("P= ",round(p,2))))},textsize = 2.5)+
        facet_grid(rows= vars(Cultivar), cols = vars(variable))+
        guides(fill=FALSE)+
        scale_fill_manual(values=c("#003074", "#F57330"))+
        chartTheme+
        theme(axis.title.x =element_blank())+
        ylab(bquote(Proportional~Area~("%")))
      
      
      
      
      
      
      # p_dml <- rvg::dml(ggobj = percentage)
      # officer::read_pptx() %>%
      #   # add slide
      #   officer::add_slide() %>%
      #   # specify object and location of object
      #   officer::ph_with(p_dml, ph_location()) %>%
      #   # export slide
      #   base::print(
      #     target = "C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/AN_midrib_percentage.pptx"
      #   )
      # 
      
      
      #permutations Pith.area..um.2.
      
      perm_pith<- anatomy_measumetns %>%
        filter(variable=="Pith.area..um.2.") %>%
        select(Cultivar, value, Status)
      average_cultivars_and_Status<- aggregate(perm_pith$value, list(perm_pith$Cultivar, perm_pith$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_pith$Cultivar, Status= perm_pith$Status, value= c(sample(filter(perm_pith, Cultivar=="PA")$value, replace = F), sample(filter(perm_pith, Cultivar=="SB")$value, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$value, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      pith_midrib<- data.frame(variable=rep("pith_midrib", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      
      #permutations xylem.area..um.2.
      perm_xylem<- anatomy_measumetns %>%
        filter(variable=="xylem.area..um.2.") %>%
        select(Cultivar, value, Status)
      average_cultivars_and_Status<- aggregate(perm_xylem$value, list(perm_xylem$Cultivar, perm_xylem$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_xylem$Cultivar, Status= perm_xylem$Status, value= c(sample(filter(perm_xylem, Cultivar=="PA")$value, replace = F), sample(filter(perm_xylem, Cultivar=="SB")$value, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$value, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      xylem_midrib<- data.frame(variable=rep("xylem_midrib", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      #permutations phloem.area..um.2.
      
      perm_phloem<- anatomy_measumetns %>%
        filter(variable=="phloem.area..um.2.") %>%
        select(Cultivar, value, Status)
      average_cultivars_and_Status<- aggregate(perm_phloem$value, list(perm_phloem$Cultivar, perm_phloem$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_phloem$Cultivar, Status= perm_phloem$Status, value= c(sample(filter(perm_phloem, Cultivar=="PA")$value, replace = F), sample(filter(perm_phloem, Cultivar=="SB")$value, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$value, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      phloem_midrib<- data.frame(variable=rep("phloem_midrib", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      
      #permutations percentage xylem.area....
      
      perm_xylem_per<- anatomy_percentage %>%
        filter(variable=="xylem.area....") %>%
        select(Cultivar, value, Status)
      average_cultivars_and_Status<- aggregate(perm_xylem_per$value, list(perm_xylem_per$Cultivar, perm_xylem_per$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_xylem_per$Cultivar, Status= perm_xylem_per$Status, value= c(sample(filter(perm_xylem_per, Cultivar=="PA")$value, replace = F), sample(filter(perm_xylem_per, Cultivar=="SB")$value, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$value, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      xylem_midrib_per<- data.frame(variable=rep("xylem_midrib_per", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      #permutations percentage phloem.area....
      
      perm_ploem_per<- anatomy_percentage %>%
        filter(variable=="phloem.area....") %>%
        select(Cultivar, value, Status)
      average_cultivars_and_Status<- aggregate(perm_ploem_per$value, list(perm_ploem_per$Cultivar, perm_ploem_per$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_ploem_per$Cultivar, Status= perm_ploem_per$Status, value= c(sample(filter(perm_ploem_per, Cultivar=="PA")$value, replace = F), sample(filter(perm_ploem_per, Cultivar=="SB")$value, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$value, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      phloem_midrib_per<- data.frame(variable=rep("phloem_midrib_per", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      
      #write.csv(rbind(pith_midrib, xylem_midrib, phloem_midrib, xylem_midrib_per, phloem_midrib_per), "C:/Users/jrobl/OneDrive - University of Florida/Desktop/res_anatomy_midrib.csv", row.names = F)
      
      
      
      
      
      
    #plot permutations
      #----chart theme
      chartTheme <- theme(plot.title=element_text(size=20, face = "bold", hjust = 0.5),
                          axis.title.x=element_text(size=18, face = "bold"), 
                          axis.title.y=element_text(size=18, face = "bold"),
                          axis.text.x=element_text(size=14, angle= 0, vjust=0.5),
                          axis.text.y=element_text(size=14),
                          panel.grid.minor.y=element_line(linetype="dotted"),
                          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                          plot.background=element_rect(linetype="blank"),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(), 
                          axis.line = element_line(colour = "black"),
                          legend.key = element_rect(fill = "white"))
      setwd("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/OBJ 1 Anatomy Permutations/")
      library(tidyverse)
      df <-
        list.files(path = "./", pattern = "*.csv") %>% 
        map_df(~read_csv(.))
      df
      df$variable<- factor(df$variable, levels =   c("pith_midrib", "xylem_midrib", "phloem_midrib", "xylem_midrib_per", 
                                                     "phloem_midrib_per", "pith_petiole", "xylem_petiole", "phloem_petiole", 
                                                     "xylem_petiole_percent", "phloem_petiole_percent"))
      final_anatomy<- ggplot(df, aes(x=variable, y=log(value_inf_vs_healthy, base=2), fill=Cultivar))+
        geom_bar(stat = 'identity')+
        scale_fill_manual(values=c("#003074", "#F57330"))+
        geom_text(aes(label=p_value, y=2.2))+
        facet_grid(~Cultivar)+
        coord_cartesian(expand = T, ylim = c(-3,4))+
        coord_flip()+
        chartTheme
      
# 
#         #Save PPT
#         p_dml <- rvg::dml(ggobj = final_anatomy)
#         # initialize PowerPoint slide
#         officer::read_pptx() %>%
#           # add slide
#         officer::add_slide() %>%
#           # specify object and location of object
#         officer::ph_with(p_dml, ph_location()) %>%
#           # export slide -
#         base::print(
#           target = "C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/AN_FINAL_anatomy.pptx"
#         )
      
#PATHOLOGY----
      #Senescence and NPR1 genes----
      library(dplyr)
      library(reshape2)
      library(ggplot2)
      library(ggpubr)
      library(rvg)
      library(officer)
      
      setwd("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/")
      chartTheme <- theme(plot.title=element_text(size=20, face = "bold", hjust = 0.5),
                          axis.title.x=element_text(size=9, face = "bold"), 
                          axis.title.y=element_text(size=9, face = "bold"),
                          axis.text.x=element_text(size=9, angle= 0, vjust=0.5),
                          axis.text.y=element_text(size=9),
                          panel.grid.minor.y=element_line(linetype="dotted"),
                          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                          plot.background=element_rect(linetype="blank"),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(), 
                          axis.line = element_line(colour = "black"),
                          legend.key = element_rect(fill = "white"))
      
      
      gene_expression<- read.csv("Data/SYBR Results 2022-10-18.csv")
      #gene_expression<- na.omit(gene_expression)
      id_samples<-read.csv("Data/Samples IDs and Comments.csv", skip = 7)
      gene_expression<- inner_join(gene_expression, id_samples)
      tree_codes<- read.csv("Data/Tree codes.csv")
      gene_expression<- inner_join(tree_codes, gene_expression)
      gene_expression<- select(gene_expression, Cultivar, Status, Gene, Sample.ID, LEAF,Ct)
      #gene_expression_PA<- filter(gene_expression, Cultivar=="PA")
      #gene_expression_SB<- filter(gene_expression, Cultivar=="SB")
      gene_expression_means<- dcast(gene_expression, Cultivar +Status+ Sample.ID+ LEAF~Gene, fun.aggregate = mean, na.rm = TRUE)
      gene_expression_means$Dt_CaAALP<- gene_expression_means$'CaAALP (627-628)'- gene_expression_means$GAPDH
      gene_expression_means$Dt_CsRD19<- gene_expression_means$'CsRD19 (623-624)'- gene_expression_means$GAPDH
      gene_expression_means$Dt_Ferr<- gene_expression_means$'Ferr (709-710)'- gene_expression_means$GAPDH
      gene_expression_means$Dt_GPI<- gene_expression_means$'GPI (603-604)'- gene_expression_means$GAPDH
      gene_expression_means$Dt_NPR1<- gene_expression_means$'NPR1 (547-548)'- gene_expression_means$GAPDH
      gene_expression_means$Dt_SRG1B<- gene_expression_means$'SRG1B (631-632)'- gene_expression_means$GAPDH
      #write.csv(gene_expression_means, "C:/Users/jrobl/OneDrive - University of Florida/Desktop/dpi.csv")
     
      gene_expression_means<- gene_expression_means[,-c(5:11)]
      gene_expression_Dt_Dt<- melt(data =gene_expression_means, id = c("Cultivar","Status", "Sample.ID","LEAF"))
      aritmetic_means_healthy<- aggregate(filter(gene_expression_Dt_Dt, Status=="Healthy")$value, list(filter(gene_expression_Dt_Dt, Status=="Healthy")$Cultivar, filter(gene_expression_Dt_Dt, Status=="Healthy")$LEAF, filter(gene_expression_Dt_Dt, Status=="Healthy")$variable), mean, na.rm=T)
      colnames(aritmetic_means_healthy)<- c("Cultivar", "LEAF", "variable", "arti_mean_healthy")
      gene_expression_Dt_Dt<- full_join(aritmetic_means_healthy,gene_expression_Dt_Dt)
      gene_expression_Dt_Dt$dt_dt_ct<- gene_expression_Dt_Dt$value - gene_expression_Dt_Dt$arti_mean_healthy
      gene_expression_Dt_Dt$Fold_gene_expression<- 2^-(gene_expression_Dt_Dt$dt_dt_ct)
      
      
      
      count<- 1
      statistics<- matrix(ncol=6,nrow=36)
      colnames(statistics)<- c("Cultivar", "LEAF","Gene","T test P value", "shap healt P value", "shap HLB+ P value")
      
      
      
      
      # 
      # for (i in unique(gene_expression_Dt_Dt$Cultivar)) {
      #   for (j in unique(gene_expression_Dt_Dt$LEAF)) {
      #     for (k in unique(gene_expression_Dt_Dt$variable)) {
      #       a<- filter(gene_expression_Dt_Dt, Cultivar==i, LEAF==j, variable==k, Status=="Healthy")$Fold_gene_expression
      #       b<- filter(gene_expression_Dt_Dt, Cultivar==i, LEAF==j, variable==k, Status=="HLB+")$Fold_gene_expression
      #       if((sum(!is.na(a))+sum(!is.na(b)))>3){
      #         statistics[count,1]<- i
      #         statistics[count,2]<- j
      #         statistics[count,3]<- k
      #         statistics[count,4]<- t.test(a,b)$p.value
      #         statistics[count,5]<- ifelse(sum(!is.na(a))>2,shapiro.test(a)$p.value, "ao data" )
      #         statistics[count,6]<- ifelse(sum(!is.na(b))>2,shapiro.test(b)$p.value, "ao data" )
      #         count<- count+1
      #         print(paste(i,j,k,sep = " "))
      #       }else{
      #         statistics[count,1]<- i
      #         statistics[count,2]<- j
      #         statistics[count,3]<- k
      #         statistics[count,4]<- "no data"
      #         statistics[count,5]<- "no data"
      #         statistics[count,6]<- "no data"
      #         count<- count+1
      #       }
      # 
      #     }
      # 
      #   }
      # 
      # }
      # 
      
      
      
      
      
      
      for (i in unique(gene_expression_Dt_Dt$Cultivar)) {
        for (j in unique(gene_expression_Dt_Dt$LEAF)) {
          for (k in unique(gene_expression_Dt_Dt$variable)) {
            a<- filter(gene_expression_Dt_Dt, Cultivar==i, LEAF==j, variable==k, Status=="Healthy")$Fold_gene_expression
            b<- filter(gene_expression_Dt_Dt, Cultivar==i, LEAF==j, variable==k, Status=="HLB+")$Fold_gene_expression
            if(sum(!is.na(a))>1 & sum(!is.na(b))>1){
              statistics[count,1]<- i
              statistics[count,2]<- j
              statistics[count,3]<- k
              statistics[count,4]<- t.test(a,b)$p.value
              statistics[count,5]<- ifelse(sum(!is.na(a))>2,shapiro.test(a)$p.value, "ao data" )
              statistics[count,6]<- ifelse(sum(!is.na(b))>2,shapiro.test(b)$p.value, "ao data" )
              count<- count+1
              print(paste(i,j,k,sep = " "))
            }else{
              statistics[count,1]<- i
              statistics[count,2]<- j
              statistics[count,3]<- k
              statistics[count,4]<- "no data"
              statistics[count,5]<- "no data"
              statistics[count,6]<- "no data"
              count<- count+1
            }
            
          }
          
        }
        
      }
      
      
      #write.csv(statistics,"C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Monthly Reports/2022-06 Figures/Statistics_expression.csv" )
      
      
      
      
      
      
      
      summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                            conf.interval=.95, .drop=TRUE) {
        library(plyr)
        
        # New version of length which can handle NA's: if na.rm==T, don't count them
        length2 <- function (x, na.rm=FALSE) {
          if (na.rm) sum(!is.na(x))
          else       length(x)
        }
        
        # This does the summary. For each group's data frame, return a vector with
        # N, mean, and sd
        datac <- ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                         c(N    = length2(xx[[col]], na.rm=na.rm),
                           mean = mean   (xx[[col]], na.rm=na.rm),
                           sd   = sd     (xx[[col]], na.rm=na.rm)
                         )
                       },
                       measurevar
        )
        
        # Rename the "mean" column    
        datac <- rename(datac, c("mean" = measurevar))
        
        datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
        
        # Confidence interval multiplier for standard error
        # Calculate t-statistic for confidence interval: 
        # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
        ciMult <- qt(conf.interval/2 + .5, datac$N-1)
        datac$ci <- datac$se * ciMult
        
        return(datac)
      }
      
      
      gene_expression_Dt_Dt<- filter(gene_expression_Dt_Dt, variable=="Dt_CaAALP" | variable== "Dt_CsRD19" | variable=="Dt_NPR1" | variable=="Dt_SRG1B")
      gene_expression_Dt_Dt$variable<- factor(gene_expression_Dt_Dt$variable, levels = c("Dt_CaAALP", "Dt_CsRD19", "Dt_SRG1B", "Dt_NPR1"))
      sample<- summarySE(gene_expression_Dt_Dt, measurevar="Fold_gene_expression", groupvars=c("Status","variable", "LEAF", "Cultivar"), na.rm = T)
       
      
      
      fig<-ggplot(filter(gene_expression_Dt_Dt, Cultivar=="PA"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar))+
        geom_bar(data=filter(sample, Cultivar=="PA"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar), color= "#343741",outlier.shape = NA, stat = "identity",position = position_dodge())+
        geom_point(aes(x=Status, y=Fold_gene_expression, fill=Cultivar), colour = "#D8D4D7", size = 0.15, position=position_jitterdodge())+
        geom_errorbar(data=filter(sample, Cultivar=="PA"), aes(ymin=Fold_gene_expression-se, ymax=Fold_gene_expression+se), position = position_dodge(width = 0.5), alpha=0.9, size=0.5, width=0.4)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "t.test", map_signif_level = TRUE)+
        facet_grid(variable~LEAF,  scales = "free_y")+
        chartTheme+
        theme(axis.text.x = element_text(angle = 45))+
        scale_fill_manual(values = c("#0021A5", "#FA4616"))
      

      # #Save PPT
      # p_dml <- rvg::dml(ggobj = fig)
      # # initialize PowerPoint slide
      # officer::read_pptx() %>%
      #   # add slide
      # officer::add_slide() %>%
      #   # specify object and location of object
      # officer::ph_with(p_dml, ph_location()) %>%
      #   # export slide -
      # base::print(
      #   target = "C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/Expression_Senescence_2022-12-20.pptx"
      # )

      
      
      
      
      
      fig<-ggplot(filter(gene_expression_Dt_Dt, Cultivar=="SB"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar))+
        geom_bar(data=filter(sample, Cultivar=="SB"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar), color= "#343741",outlier.shape = NA, stat = "identity",position = position_dodge())+
        geom_point(aes(x=Status, y=Fold_gene_expression, fill=Cultivar), colour = "#D8D4D7", size = 0.15, position=position_jitterdodge())+
        geom_errorbar(data=filter(sample, Cultivar=="SB"), aes(ymin=Fold_gene_expression-se, ymax=Fold_gene_expression+se), position = position_dodge(width = 0.5), alpha=0.9, size=0.5, width=0.4)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "t.test", map_signif_level = TRUE)+
        facet_grid(variable~LEAF,  scales = "free_y")    +
        chartTheme+
        theme(axis.text.x = element_text(angle = 45))+
        scale_fill_manual(values = c("#FA4616"))

# 
#       #Save PPT
#       p_dml <- rvg::dml(ggobj = fig)
#       # initialize PowerPoint slide
#       officer::read_pptx() %>%
#         # add slide
#         officer::add_slide() %>%
#         # specify object and location of object
#         officer::ph_with(p_dml, ph_location()) %>%
#         # export slide -
#         base::print(
#           target = "C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/Expression_Senescence_2022-12-26.pptx"
#         )


      
      
      sample<- summarySE(gene_expression_Dt_Dt, measurevar="Fold_gene_expression", groupvars=c("Status","variable","Cultivar"), na.rm = T)
      sample<- filter(sample, variable=="Dt_CaAALP" | variable== "Dt_CsRD19" | variable=="Dt_NPR1" | variable=="Dt_SRG1B")
      levels(gene_expression_Dt_Dt$variable)<- list(CaAALP= "Dt_CaAALP", CsRD19="Dt_CsRD19", SRG1B="Dt_SRG1B", CsNPR1="Dt_NPR1")
      levels(sample$variable)<- list(CaAALP= "Dt_CaAALP", CsRD19="Dt_CsRD19", SRG1B="Dt_SRG1B", CsNPR1="Dt_NPR1")
      names(gene_expression_Dt_Dt)[names(gene_expression_Dt_Dt)=="LEAF"]<- "Leaf age"
      names(sample)[names(sample)=="LEAF"]<- "Leaf age"

      
      figure_3_C<-ggplot(gene_expression_Dt_Dt, aes(x=Status, y=Fold_gene_expression))+
        geom_bar(data=sample, aes(x=Status, y=Fold_gene_expression, fill=Cultivar), color= "#343741",outlier.shape = NA, stat = "identity",position = position_dodge())+
        geom_point(aes(x=Status, y=Fold_gene_expression, color=`Leaf age`, shape=`Leaf age`), size = 1.4, position=position_jitterdodge())+
        geom_errorbar(data=sample, aes(ymin=Fold_gene_expression-se, ymax=Fold_gene_expression+se), position = position_dodge(width = 0.5), alpha=0.9, size=0.5, width=0.1)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "wilcox.test", map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",ifelse(p<0.01,"P < 0.01", paste("P= ",round(p,2))))},textsize = 2.5, margin_top = -0.2)+
        facet_grid(variable~Cultivar, scale="free_y")+
        theme(legend.text = element_text(size=7))+
        chartTheme+
        scale_color_manual(values = c("#A9A9A9", "#6082B6", "black"))+
        scale_fill_manual(values=c("#003074", "#F57330"))+
        guides(fill=FALSE)+
        ylab(bquote(Relative~Expression~"("*Fold-change*")"~"("*Infected/Healthy*")"))+
        chartTheme+
        theme(axis.title.x = element_blank())
      
      
      
      
      fig<-ggplot(filter(gene_expression_Dt_Dt, Cultivar=="PA"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar))+
        geom_bar(data=filter(sample, Cultivar=="PA"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar), color= "#343741",outlier.shape = NA, stat = "identity",position = position_dodge())+
        geom_point(aes(x=Status, y=Fold_gene_expression, color=LEAF, shape=LEAF), size = 0.3, position=position_jitterdodge())+
        geom_errorbar(data=filter(sample, Cultivar=="PA"), aes(ymin=Fold_gene_expression-se, ymax=Fold_gene_expression+se), position = position_dodge(width = 0.5), alpha=0.9, size=0.5, width=0.4)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "wilcox.test", map_signif_level = F)+
        facet_grid(variable~Cultivar,  scales = "free_y")+
        chartTheme+
        theme(axis.text.x = element_text(angle = 45))+
        scale_fill_manual(values = c("#0021A5", "#0021A5"))+
        scale_color_manual(values = c("#A9A9A9", "#6082B6", "black"))
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="Healthy", variable=="Dt_CaAALP")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="HLB+", variable=="Dt_CaAALP")$Fold_gene_expression)
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="Healthy", variable=="Dt_CsRD19")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="HLB+", variable=="Dt_CsRD19")$Fold_gene_expression)
      
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="Healthy", variable=="Dt_SRG1B")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="HLB+", variable=="Dt_SRG1B")$Fold_gene_expression)
      
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="Healthy", variable=="Dt_NPR1")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="PA", Status=="HLB+", variable=="Dt_NPR1")$Fold_gene_expression)
      
      
# 
# 
#       #Save PPT
#       p_dml <- rvg::dml(ggobj = fig)
#       # initialize PowerPoint slide
#       officer::read_pptx() %>%
#         # add slide
#       officer::add_slide() %>%
#         # specify object and location of object
#       officer::ph_with(p_dml, ph_location()) %>%
#         # export slide -
#       base::print(
#         target = "C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/Expression_others_PA_ages_mixed 2022-12-26.pptx"
#       )
# 
#       
      
      
      fig<-ggplot(filter(gene_expression_Dt_Dt, Cultivar=="SB"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar))+
        geom_bar(data=filter(sample, Cultivar=="SB"), aes(x=Status, y=Fold_gene_expression, fill=Cultivar), color= "#343741",outlier.shape = NA, stat = "identity",position = position_dodge())+
        geom_point(aes(x=Status, y=Fold_gene_expression, color=LEAF, shape=LEAF), size = 0.3, position=position_jitterdodge())+
        geom_errorbar(data=filter(sample, Cultivar=="SB"), aes(ymin=Fold_gene_expression-se, ymax=Fold_gene_expression+se), position = position_dodge(width = 0.5), alpha=0.9, size=0.5, width=0.4)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "t.test", map_signif_level = F)+
        facet_grid(variable~Cultivar,  scales = "free_y")+
        chartTheme+
        theme(axis.text.x = element_text(angle = 45))+
        scale_fill_manual(values = c("#FA4616", "#FA4616"))+
        scale_color_manual(values = c("#A9A9A9", "#6082B6", "black"))
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="Healthy", variable=="Dt_CaAALP")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="HLB+", variable=="Dt_CaAALP")$Fold_gene_expression)
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="Healthy", variable=="Dt_CsRD19")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="HLB+", variable=="Dt_CsRD19")$Fold_gene_expression)
      
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="Healthy", variable=="Dt_SRG1B")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="HLB+", variable=="Dt_SRG1B")$Fold_gene_expression)
      
      
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="Healthy", variable=="Dt_NPR1")$Fold_gene_expression)
      shapiro.test(filter(gene_expression_Dt_Dt, Cultivar=="SB", Status=="HLB+", variable=="Dt_NPR1")$Fold_gene_expression)
      


# 
#         #Save PPT
#         p_dml <- rvg::dml(ggobj = fig)
#         # initialize PowerPoint slide
#         officer::read_pptx() %>%
#           # add slide
#         officer::add_slide() %>%
#           # specify object and location of object
#         officer::ph_with(p_dml, ph_location()) %>%
#           # export slide -
#         base::print(
#           target = "C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/Expression_others_SB_ages_mixed 2022-12-26.pptx"
#         )
      
      #permutations "Dt_CaAALP"
      
      perm_CaAALP<- gene_expression_Dt_Dt %>%
        filter(variable=="Dt_CaAALP") %>%
        select(Cultivar, Fold_gene_expression, Status)
      average_cultivars_and_Status<- aggregate(perm_CaAALP$Fold_gene_expression, list(perm_CaAALP$Cultivar, perm_CaAALP$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_CaAALP$Cultivar, Status= perm_CaAALP$Status, Fold_gene_expression= c(sample(filter(perm_CaAALP, Cultivar=="PA")$Fold_gene_expression, replace = F), sample(filter(perm_CaAALP, Cultivar=="SB")$Fold_gene_expression, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$Fold_gene_expression, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      Dt_CaAALP<- data.frame(variable=rep("Dt_CaAALP", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      #permutations "Dt_CsRD19"
      
      perm_CsRD19<- gene_expression_Dt_Dt %>%
        filter(variable=="Dt_CsRD19") %>%
        select(Cultivar, Fold_gene_expression, Status)
      average_cultivars_and_Status<- aggregate(perm_CsRD19$Fold_gene_expression, list(perm_CsRD19$Cultivar, perm_CsRD19$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_CsRD19$Cultivar, Status= perm_CsRD19$Status, Fold_gene_expression= c(sample(filter(perm_CsRD19, Cultivar=="PA")$Fold_gene_expression, replace = F), sample(filter(perm_CsRD19, Cultivar=="SB")$Fold_gene_expression, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$Fold_gene_expression, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      Dt_CsRD19<- data.frame(variable=rep("Dt_CsRD19", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      
      
      #permutations "Dt_SRG1B"
      
      perm_Dt_SRG1B<- gene_expression_Dt_Dt %>%
        filter(variable=="Dt_SRG1B") %>%
        select(Cultivar, Fold_gene_expression, Status)
      average_cultivars_and_Status<- aggregate(perm_Dt_SRG1B$Fold_gene_expression, list(perm_Dt_SRG1B$Cultivar, perm_Dt_SRG1B$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_Dt_SRG1B$Cultivar, Status= perm_Dt_SRG1B$Status, Fold_gene_expression= c(sample(filter(perm_Dt_SRG1B, Cultivar=="PA")$Fold_gene_expression, replace = F), sample(filter(perm_Dt_SRG1B, Cultivar=="SB")$Fold_gene_expression, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$Fold_gene_expression, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      Dt_SRG1B<- data.frame(variable=rep("Dt_SRG1B", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      
      #permutations "Dt_NPR1"
      
      perm_NPR1<- gene_expression_Dt_Dt %>%
        filter(variable=="Dt_NPR1") %>%
        select(Cultivar, Fold_gene_expression, Status)
      average_cultivars_and_Status<- aggregate(perm_NPR1$Fold_gene_expression, list(perm_NPR1$Cultivar, perm_NPR1$Status), mean, na.rm=T)
      fold_change_PA_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
      fold_change_SB_Infected_VS_Healthy<- filter(average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
      observed_fold_GS<- fold_change_SB_Infected_VS_Healthy-fold_change_PA_Infected_VS_Healthy
      
      perm<- 5000
      vect<- c(rep(NA, perm))
      for (i in 1:perm) {
        temp_data<- data.frame(Cultivar= perm_NPR1$Cultivar, Status= perm_NPR1$Status, Fold_gene_expression= c(sample(filter(perm_NPR1, Cultivar=="PA")$Fold_gene_expression, replace = F), sample(filter(perm_NPR1, Cultivar=="SB")$Fold_gene_expression, replace = F)))
        temp_average_cultivars_and_Status<- aggregate(temp_data$Fold_gene_expression, list(temp_data$Cultivar, temp_data$Status), mean, na.rm=T)
        temp_fold_change_PA_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="PA" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="PA" & Group.2=="Healthy")$x
        temp_fold_change_SB_Infected_VS_Healthy<- filter(temp_average_cultivars_and_Status, Group.1=="SB" & Group.2=="HLB+")$x/filter(temp_average_cultivars_and_Status,Group.1=="SB" & Group.2=="Healthy")$x
        vect[i]<- temp_fold_change_SB_Infected_VS_Healthy-temp_fold_change_PA_Infected_VS_Healthy
      }
      
      hist(vect)
      abline(v=observed_fold_GS, col="red")
      p_value<- mean(vect>observed_fold_GS) 
      
      Dt_NPR1<- data.frame(variable=rep("Dt_NPR1", 2), Cultivar=c("PA", "SB"), value_inf_vs_healthy= c(fold_change_PA_Infected_VS_Healthy, fold_change_SB_Infected_VS_Healthy), p_value=p_value)
      
      # 
      # write.csv(rbind(Dt_CaAALP, Dt_CsRD19, Dt_SRG1B, Dt_NPR1), "C:/Users/jrobl/OneDrive - University of Florida/Desktop/res_senesce_and_NPR1_GPI.csv", row.names = F)
      # 
      # 
      
      #CT values ----
      
      
      library(ggplot2)
      library(dplyr)
      library(rvg)
      library(officer)
      setwd("C:/Users/jrobl/OneDrive - University of Florida/Documents/")
      #----chart theme
      chartTheme <- theme(plot.title=element_text(size=20, face = "bold", hjust = 0.5),
                          axis.title.x=element_text(size=9, face = "bold"), 
                          axis.title.y=element_text(size=9, face = "bold"),
                          axis.text.x=element_text(size=9, angle= 0, vjust=0.5),
                          axis.text.y=element_text(size=9),
                          panel.grid.minor.y=element_line(linetype="dotted"),
                          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                          plot.background=element_rect(linetype="blank"),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(), 
                          axis.line = element_line(colour = "black"),
                          legend.key = element_rect(fill = "white"))
      #----read Ct values
      ct_values<- read.csv("MASTER Thesis/Statistical Analysis/Data/Ct_VALUES_OBJ_1 2022_08_11.csv")
      tree_codes<- read.csv("MASTER Thesis/Statistical Analysis/Data/Tree codes.csv")
      colnames(tree_codes)<- c("Tree.Code", "Cultivar", "Status")
      #setting tree codes
      ct_values<-inner_join(ct_values,tree_codes)
      #calculating delta CT
      ct_values$delta_Ct<- ct_values$NTC-as.numeric(ct_values$Ct.Value)
      #Removing healthy plants and negative values (considered as negatives samples for CLas)
      ct_values<- filter(ct_values, delta_Ct>0, Status=="HLB+")
      
      figure_Ct_2<- ggplot(ct_values, aes(x=Cultivar, y=delta_Ct, fill= Cultivar))+
        geom_violin(trim = FALSE, color= "#C7C9C8")+
        geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
        geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size = 1.4, position=position_jitter(h=0.1, w=0.1), alpha=0.5)+
        stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.30, alpha=0.6)+
        geom_signif(map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",ifelse(p<0.01,"P < 0.01", paste("P= ",round(p,2))))},textsize = 2.5, test = "t.test", comparisons = list(c("PA", "SB")))+
        scale_fill_manual(values=c("#003074", "#F57330"))+
        chartTheme+
        guides(fill=FALSE)+
        theme(axis.title.x = element_blank())+
        ylab(bquote(atop(CLas~Genetic~Material~ phantom(), "("*Delta*Ct~")")))
      
      
      
      
      
      shapiro.test(filter(ct_values, Cultivar=="PA" )$delta_Ct)
      shapiro.test(filter(ct_values, Cultivar=="SB" )$delta_Ct)
    
      
      # p_dml <- rvg::dml(ggobj = figure_Ct_2)
      # officer::read_pptx() %>%
      #   # add slide 
      #   officer::add_slide() %>%
      #   # specify object and location of object
      #   officer::ph_with(p_dml, ph_location()) %>%
      #   # export slide
      #   base::print(
      #     target = "C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/Delta_Ct_2.pptx"
      #   )
      # 
      
      summary<-aggregate(ct_values$delta_Ct, list(ct_values$Cultivar), mean)
      colnames(summary)<- c("Cultivar", "value_inf_vs_healthy")
      summary$variable<- "Delta Ct"
      summary$p_value<- t.test(filter(ct_values, Cultivar=="PA" )$delta_Ct, filter(ct_values, Cultivar=="SB" )$delta_Ct)$p.value
      # 
      # write.csv(summary, "C:/Users/jrobl/OneDrive - University of Florida/Desktop/res_delta_CT.csv", row.names = F)
      # 
      # 
      #SYMPTOMS----
      library(dplyr)
      library(gplots)
      library(officer)
      library(rvg)
      library(ggplot2)
      library(ggpubr)
      #----chart theme
      chartTheme <- theme(plot.title=element_text(size=20, face = "bold", hjust = 0.5),
                          axis.title.x=element_text(size=9, face = "bold"), 
                          axis.title.y=element_text(size=9, face = "bold"),
                          axis.text.x=element_text(size=9, angle= 0, vjust=0.5),
                          axis.text.y=element_text(size=9),
                          panel.grid.minor.y=element_line(linetype="dotted"),
                          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                          plot.background=element_rect(linetype="blank"),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(), 
                          axis.line = element_line(colour = "black"),
                          legend.key = element_rect(fill = "white"))
      
      survey<- read.csv("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/SURVEY symptoms 2022-07-06.csv")
      tree_codes<- read.csv("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/Tree codes.csv")
      colnames(tree_codes)<- c("Tree.Code", "Cultivar", "Status")
      survey<- inner_join(survey, tree_codes)
      survey$int<- interaction(survey$Person, survey$Tree_Name)
      #sum of scores 
      survey<- survey %>%
        #To balace the observations of TIM (he did not evaluate all 5 SB) one observation of PA is deleted. The highest observation was selected to penalize this person 
        filter(!(int %in% c("TIM.Charlotte", "TIM.James")))
      
    
      
      
      
        sum_scores<- aggregate(survey$Score, list(survey$Person, survey$Cultivar), mean)
      symptoms_figure<- ggplot(sum_scores, aes(x=Group.2, y=log(x), fill=Group.2))+
        geom_violin(trim = FALSE, color= "#C7C9C8")+
        scale_fill_manual(values=c("#003074", "#F57330"))+
        geom_boxplot(width=0.1, fill= "white", outlier.shape = NA, color= "#C7C9C8")+
        geom_point(shape = 21, color= "#343741", fill = "#D8D4D7", size =1.4, position=position_jitter(h=0.05, w=0.05), alpha=0.5)+
        stat_summary(fun = mean, color="#F2A900", shape=17, size = 0.30,  alpha=0.6)+
        geom_line(aes(group=Group.1), position=position_jitter(h=0.01, w=0.01), col="#D8D4D7", size=0.25)+
        geom_signif(test = "t.test", comparisons = list(c("PA", "SB")), 
                    test.args=list(paired=T, var.equal = F), map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",ifelse(p<0.01,"P < 0.01", paste("P= ",round(p,2))))},textsize = 2.5)+
        chartTheme+
        guides(fill=FALSE)+
        theme(axis.title.x = element_blank())+
        ylab(bquote(atop(Mean~Severity~Score~of~ phantom(), HLB~Symptoms)))
      
      shapiro.test(log(filter(sum_scores, Group.2=="SB")$x))
      shapiro.test(log(filter(sum_scores, Group.2=="PA")$x))

    
      
      
      
# 
# 
#       p_dml <- rvg::dml(ggobj = ct_figure)
#       officer::read_pptx() %>%
#         # add slide
#       officer::add_slide() %>%
#         # specify object and location of object
#       officer::ph_with(p_dml, ph_location()) %>%
#         # export slide
#       base::print(
#         target = "C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/PAT_SYMPTOMS.pptx"
#       )
      symptoms<-aggregate(log(sum_scores$x), list(sum_scores$Group.2), mean)
      symptoms$x<- exp(symptoms$x)
      colnames(symptoms)<- c("Cultivar", "value_inf_vs_healthy")
      symptoms$variable<- "SYMPTOMS"
      symptoms$p_value<- t.test(log(filter(sum_scores, Group.2=="PA" )$x), log(filter(sum_scores, Group.2=="SB" )$x), paired=T)$p.value
      #write.csv(symptoms, "C:/Users/jrobl/OneDrive - University of Florida/Desktop/res_symptoms.csv", row.names = F)
      


      #Final pathology----
      #----chart theme
      chartTheme <- theme(plot.title=element_text(size=20, face = "bold", hjust = 0.5),
                          axis.title.x=element_text(size=18, face = "bold"), 
                          axis.title.y=element_text(size=18, face = "bold"),
                          axis.text.x=element_text(size=14, angle= 0, vjust=0.5),
                          axis.text.y=element_text(size=14),
                          panel.grid.minor.y=element_line(linetype="dotted"),
                          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                          plot.background=element_rect(linetype="blank"),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(), 
                          axis.line = element_line(colour = "black"),
                          legend.key = element_rect(fill = "white"))
      setwd("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/OBJ 1 Pathology Permutations/")
      library(tidyverse)
      df <-
        list.files(path = "./", pattern = "*.csv") %>% 
        map_df(~read_csv(.))
      df
      df$variable<- factor(df$variable, levels = c("SYMPTOMS","Delta Ct","Dt_NPR1", "Dt_CaAALP", "Dt_CsRD19", "Dt_SRG1B" ))
      final_pathology<- ggplot(df, aes(x=variable, y=log(value_inf_vs_healthy, base=2), fill=Cultivar))+
        geom_bar(stat = 'identity')+
        scale_fill_manual(values=c("#003074", "#F57330"))+
        geom_text(aes(label=p_value, y=2.2))+
        facet_grid(~Cultivar)+
        coord_cartesian(expand = T, ylim = c(-3,4))+
        coord_flip()+
        chartTheme

        # #Save PPT
        # p_dml <- rvg::dml(ggobj = final_pathology)
        # # initialize PowerPoint slide
        # officer::read_pptx() %>%
        #   # add slide
        # officer::add_slide() %>%
        #   # specify object and location of object
        # officer::ph_with(p_dml, ph_location()) %>%
        #   # export slide -
        # base::print(
        #   target = "C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/FINAL_Pathology.pptx"
        # )
#FIGURES Article----
      #1) Tolerant cultivar SB mandarin present a higher relative change in carbon fixation and starch accumulation but a mild reduction in carbon export----
      library(cowplot)
      figure_1<- plot_grid(figure_1_A, figure_1_B, figure_1_C, figure_1_D, 
                           rel_heights = c(1,1),
                           rel_widths = c(1,1.5), align = 'hv', labels = 'AUTO', label_size = 12)
      pdf("C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/FIG_ART_1_Carbon_P_values_numbers.pdf",
          width = 8.5,
          height = 5.5)
      print(figure_1)
      dev.off()
      
      
      
      #2) Although callose deposition and callose synthesis genes are more abundant in SB, callose distribution is less disperse in the phloem of this tolerant cultivar----
      library(cowplot)
      figure_2_1<- plot_grid(f_counts_status, f_meansize_status, f_area_status, nndist_fig_Status, ncol = 1, labels = 'AUTO', label_size = 12)
      names(gene_expression_CALLOSE)[names(gene_expression_CALLOSE)=="LEAF"]<- "Leaf age"
      #gene_expression_CALLOSE<-gene_expression_CALLOSE[,-5]
      gene_expression_Dt_Dt<- rbind(gene_expression_CALLOSE, gene_expression_GLU)
      gene_expression_Dt_Dt<- filter(gene_expression_Dt_Dt, variable!="Dt_Ferr")
      sample<- rbind(sample_glu, SAMPLE_CALLOSE)
      sample<- filter(sample, variable!="Dt_Ferr")
      levels(gene_expression_Dt_Dt$variable)<- list(CsCal3= "Dt_CsCALS3", CsCal7="Dt_CsCALS7", CsCal9="Dt_CsCALS9", CsCal11="Dt_CsCALS11", CsCal12="Dt_CsCALS12", GHf17="Dt_GPI", CsBP= "GLU.1..41.42.", Csβg1="GLU.2..65.66.")
      levels(sample$variable)<- list(CsCal3= "Dt_CsCALS3", CsCal7="Dt_CsCALS7", CsCal9="Dt_CsCALS9", CsCal11="Dt_CsCALS11", CsCal12="Dt_CsCALS12", GHf17="Dt_GPI", CsBP= "GLU.1..41.42.", Csβg1="GLU.2..65.66.")
      
      figure_2_E<-ggplot(gene_expression_Dt_Dt, aes(x=Status, y=Fold_gene_expression))+
        geom_bar(data=sample, aes(x=Status, y=Fold_gene_expression, fill=Cultivar), color= "#343741",outlier.shape = NA, stat = "identity",position = position_dodge())+
        geom_point(aes(x=Status, y=Fold_gene_expression, color=`Leaf age`, shape=`Leaf age`), size = 1.4, position=position_jitterdodge())+
        geom_errorbar(data=sample, aes(ymin=Fold_gene_expression-se, ymax=Fold_gene_expression+se), position = position_dodge(width = 0.5), alpha=0.9, size=0.5, width=0.1)+
        geom_signif(comparisons = list(c("Healthy", "HLB+")), test = "wilcox.test", map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",ifelse(p<0.01,"P < 0.01", paste("P= ",round(p,2))))},textsize = 2.5, margin_top = -0.2)+
        facet_grid(variable~Cultivar, scale="free_y")+
        theme(legend.text = element_text(size=7))+
        chartTheme+
        scale_color_manual(values = c("#A9A9A9", "#6082B6", "black"))+
        scale_fill_manual(values=c("#003074", "#F57330"))+
        guides(fill=FALSE)+
        ylab(bquote(Relative~Expression~"("*Fold-change*")"~"("*Infected/Healthy*")"))+
        chartTheme+
        theme(axis.title.x = element_blank())
      white_plot<- ggplot()+theme_classic()+theme(axis.line = element_blank())
      white<- plot_grid(white_plot, white_plot, ncol = 1, labels = c("F", "G"))
      Figure_2_Final<- plot_grid(figure_2_1, figure_2_E, white, rel_widths= c(1,1.4,0.55), ncol = 3, labels = c("","E",""))
      pdf("C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/FIG_ART_2_Calllose_P_values.pdf",
          width = 8.5,
          height = 11)
      print(Figure_2_Final)
      dev.off()
      
      # Figure_2_Final<- plot_grid(figure_2_1, figure_2_E, rel_widths= c(1,1.4), ncol = 2, labels = c("","E"))
      # pdf("C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/FIG_ART_2_Calllose_alone.pdf",
      #     width = 7,
      #     height = 11)
      # print(Figure_2_Final)
      # dev.off()
      # 
      # 
      #3) Tolerant SB mandarin was characterized for having more abundance of CLas genetic material, higher symptom severity in leaves, and greater relative expression of senescence related genes ----
      figure_3<- plot_grid(plot_grid(symptoms_figure, figure_Ct_2, ncol = 1, labels = "AUTO"), figure_3_C, rel_widths = c(1,1.5), labels = c("", "C"))
      pdf("C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/FIG_ART_3_PATH_P_values.pdf",
          width = 8.5,
          height = 5.5)
      print(figure_3)
      dev.off()
      
      #4) Tolerant SB mandarin exhibited less phloem size and a milder increase in xylem and pith size that affect the proportion of xylem area----
      library(cowplot)
      figure_4<- plot_grid(units, percentage, ncol = 2,
                          rel_widths = c(1.3,1), align = 'hv', labels = 'AUTO', label_size = 12)
      pdf("C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/FIG_ART_4_ANATOMY_P_Values.pdf",
          width = 8.5,
          height = 3.8)
      print(figure_4)
      dev.off()
      
      #5) Final figure comparison----
      
      #map_signif_level =function(p){ifelse(p<0.001,"P < 0.001",paste("P= ",round(p,2)))},textsize = 2.5
      
      
      
      
      library(tidyverse)
      chartTheme <- theme(plot.title=element_text(size=20, face = "bold", hjust = 0.5),
                          axis.title.x=element_text(size=9, face = "bold"), 
                          axis.title.y=element_text(size=9, face = "bold"),
                          axis.text.x=element_text(size=9, angle= 0, vjust=0.5),
                          axis.text.y=element_text(size=9),
                          panel.grid.minor.y=element_line(linetype="dotted"),
                          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                          plot.background=element_rect(linetype="blank"),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(), 
                          axis.line = element_line(colour = "black"),
                          legend.key = element_rect(fill = "white"))
      setwd("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/OBJ 1 Physiology Permutations/")
      
      physiology  <-
        list.files(path = "./", pattern = "*.csv") %>% 
        map_df(~read_csv(.))
      physiology
      physiology$variable<- factor(physiology$variable, levels = c("fix", "Ferr", "diff","Stomatal Conductance", "Starch", "CALL counts","CALL mean size", "CALL area", "CALL distribution", "CALS3", "CALS7", "CALS9", "CALS11", 
                                                   "CALS12", "GLU.1..41.42.", "GLU_2","GPI"))
      physiology$Type<- "Physiology"

      setwd("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/OBJ 1 Anatomy Permutations/")
      anatomy <-
        list.files(path = "./", pattern = "*.csv") %>% 
        map_df(~read_csv(.))
      anatomy
      anatomy$Type<-  "Anatomy"
      anatomy$variable<- factor(anatomy$variable, levels =   c("pith_midrib", "xylem_midrib", "phloem_midrib", "xylem_midrib_per", 
                                                     "phloem_midrib_per", "pith_petiole", "xylem_petiole", "phloem_petiole", 
                                                     "xylem_petiole_percent", "phloem_petiole_percent"))
      
      setwd("C:/Users/jrobl/OneDrive - University of Florida/Documents/MASTER Thesis/Statistical Analysis/Data/OBJ 1 Pathology Permutations/")
      library(tidyverse)
      pathology <-
        list.files(path = "./", pattern = "*.csv") %>% 
        map_df(~read_csv(.))
      pathology
      pathology$variable<- factor(pathology$variable, levels = c("SYMPTOMS","Delta Ct","Dt_NPR1", "Dt_CaAALP", "Dt_CsRD19", "Dt_SRG1B" ))
      pathology$Type<- "Pathology"
      df<- rbind(physiology,pathology, anatomy)
      df<- filter(df,variable %in% c("fix", 
                                     "Ferr", "diff", "Starch", "CALL counts", 
                                     "CALL mean size", "CALL area", "CALL distribution", "CALS3", 
                                     "CALS7", "CALS9", "CALS11", "CALS12", "GLU.1..41.42.", "GLU_2", 
                                     "GPI", "SYMPTOMS", "Delta Ct", "Dt_NPR1", "Dt_CaAALP", "Dt_CsRD19", 
                                     "Dt_SRG1B", "pith_midrib", "xylem_midrib", "phloem_midrib", "xylem_midrib_per", 
                                     "phloem_midrib_per"))
      
      levels(df$variable)<- list('Carbon Fixation*'= "fix", 
                                 'Ferredoxin - NADP'= "Ferr", 
                                 'Carbon Eport*'= "diff", 
                                 'gs'= "Stomatal Conductance", 
                                 'Starch Content'= "Starch",
                                 'Number of Callose Deposit'= "CALL counts", 
                                 'Mean Size of Callose Deposits'= "CALL mean size", 
                                 'Mean Area of Callose Deposits'="CALL area", 
                                 'Closest neighbor to Callose Deposits'="CALL distribution", 
                                 'CsCalS3'= "CALS3", 
                                 'CsCalS7'= "CALS7", 
                                 'CsCalS9'= "CALS9", 
                                 'CsCalS11'= "CALS11", 
                                 'CsCalS12'= "CALS12", 
                                 CsBP= "GLU.1..41.42.", 
                                 Csβg= "GLU_2", 
                                 GHf17= "GPI", 
                                 'Symptoms**'= "SYMPTOMS", 
                                 'Delta Ct*' = "Delta Ct", 
                                 CsNPR1= "Dt_NPR1", 
                                 CaAALP= "Dt_CaAALP", 
                                 CsRD19= "Dt_CsRD19", 
                                 SRG1B= "Dt_SRG1B", 
                                 'Midrib Pith Total Area'="pith_midrib", 
                                 'Midrib Xylem Total Area'="xylem_midrib", 
                                 'Midrib Phloem Total Area'="phloem_midrib", 
                                 'Midrib Xylem Proportional Area'= "xylem_midrib_per", 
                                 'Midrib Phloem Proportional Area'="phloem_midrib_per", 
                                 pith_petiole= "pith_petiole", 
                                 xylem_petiole="xylem_petiole", 
                                 phloem_petiole="phloem_petiole", 
                                 xylem_petiole_percent="xylem_petiole_percent", 
                                 phloem_petiole_percent="phloem_petiole_percent")
      df$p_value_char<- NA
      
      df$p_value[which(df$p_value>0.994)]<- 1-df$p_value[which(df$p_value>0.994)]
      df$p_value[which(df$p_value>=0.001)]<- round(df$p_value[which(df$p_value>=0.001)],2)
      df$p_value_char[which(df$p_value>=0.001)]<- paste("P=", df$p_value[which(df$p_value>=0.001)])
      df$p_value_char[which(df$p_value<0.001)]<- "P< 0.001"
      df$p_value_char[which(df$p_value==0.712)]<- "P=0.7"
      df$p_value_char[which(df$p_value>=0.001)]<- paste("P=", df$p_value[which(df$p_value>=0.001)])
      
      
      final_physiology<- ggplot(df, aes(x=variable, y=log(value_inf_vs_healthy, base=2), fill=Cultivar))+
        geom_bar(stat = 'identity')+
        xlab("Fold-Change (Infected/Healthy)")+
        scale_fill_manual(values=c("#003074", "#F57330"))+
        facet_grid(cols = vars(Cultivar), rows = vars(Type), scales = "free_y", space = "free")+
        geom_text(aes(label=p_value_char, y=4), data= filter(df, Cultivar=="SB"), hjust=1, size=2.3)+
        chartTheme+
        #coord_cartesian(expand = T, ylim = c(-5,5))+
        coord_flip()+
        theme(axis.title.y = element_blank())+
        ylab(bquote(Log[2]~Fold-Change~(Infected/Healthy)))
      pdf("C:/Users/jrobl/OneDrive - University of Florida/Desktop/Figures/FIG_5_Final.pdf",
          width = 7,
          height = 7)
      print(final_physiology)
      dev.off()
        
      
    
    
      
      
      
      