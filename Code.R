### SET WORKING DIRECTORY
setwd("/home/fivankov/DRIVE/ABCD/WorkDir")

# LOAD NECESSARY LIBRARIES
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(boot))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(caret))

### PROPORTION TEST FUNCTION
fprop <- function(N=N, D=D, Dx=Dx){
    
    fEST <- N/D
    fUP <- fEST + 1.96 * sqrt((fEST*(1-fEST))/D)
    fDO <- fEST - 1.96 * sqrt((fEST*(1-fEST))/D)
    
    cat(
        Dx,"\t",
        round(fEST*100, digits = 2), " (",
        round(fUP*100, digits = 2), ", " ,
        round(fDO*100, digits = 2), ")", "\n",
        sep = ""
    )
}

### DATA LOAD

# Datasets
KSAD1Y <- read_delim("./abcd_ksad501.txt", delim = "\t", escape_double = FALSE, col_types = "c",
    trim_ws = TRUE)[-1,]
KSAD1P <- read_delim("./abcd_ksad01.txt", delim = "\t", escape_double = FALSE, col_types = "c",
    trim_ws = TRUE)[-1,]
KSAD2Y <- read_delim("./ksads2daic_use_only01.txt", delim = "\t", escape_double = FALSE, col_types = "c",
    trim_ws = TRUE)[-1,]
KSAD2P <- read_delim("./ksads2daic_use_only_p01.txt", delim = "\t", escape_double = FALSE, col_types = "c",
    trim_ws = TRUE)[-1,]
CBCLR <- read_delim("./abcd_cbcl01.txt", delim = "\t", escape_double = FALSE, col_types = "c",
    trim_ws = TRUE)[-1,]
CBCL <- read_delim("./abcd_cbcls01.txt", delim = "\t", escape_double = FALSE, col_types = "c",
    trim_ws = TRUE)[-1,]

# Dictionaries
DKSAD1Y <- read_delim("./abcd_ksad501_definitions.csv", delim = ",", escape_double = FALSE, col_types = "c",
    trim_ws = TRUE)
DKSAD1P <- read_delim("./abcd_ksad01_definitions.csv", delim = ",", escape_double = FALSE, col_types = "c",
    trim_ws = TRUE)
DKSAD2Y <- read_delim("./ksads2daic_use_only01_definitions.csv", delim = ",", escape_double = FALSE, col_types = "c",
    trim_ws = TRUE)
DKSAD2P <- read_delim("./ksads2daic_use_only_p01_definitions.csv", delim = ",", escape_double = FALSE, col_types = "c",
    trim_ws = TRUE)

### RECODE VALUES

# KSAD1Y
KSAD1Y[KSAD1Y == "555"] <- "N"
KSAD1Y[KSAD1Y == "777"] <- "D"
KSAD1Y[KSAD1Y == "888"] <- "0"
KSAD1Y[KSAD1Y == "999"] <- "K"

# KSAD1P
KSAD1P[KSAD1P == "555"] <- "N"
KSAD1P[KSAD1P == "777"] <- "D"
KSAD1P[KSAD1P == "888"] <- "0"
KSAD1P[KSAD1P == "999"] <- "K"

# KSAD2Y
KSAD2Y[KSAD2Y == "555"] <- "N"
KSAD2Y[KSAD2Y == "777"] <- "D"
KSAD2Y[KSAD2Y == "888"] <- "0"
KSAD2Y[KSAD2Y == "999"] <- "K"

# KSAD2P
KSAD2P[KSAD2P == "555"] <- "N"
KSAD2P[KSAD2P == "777"] <- "D"
KSAD2P[KSAD2P == "888"] <- "0"
KSAD2P[KSAD2P == "999"] <- "K"

#CBCLR
CBCLR[CBCLR == "NA"] <- NA

#CBCL
CBCL[CBCL == "NA"] <- NA

### 01 Depressive disorders (Removed from dataset, SKIP)

### 02 Mania

# KSAD1Y
BD1Ys <- as.character(paste0("ksads_2_",seq(830, 839, 1),"_t", sep = ""))
BD1Y <- KSAD1Y %>% dplyr::select(any_of(c(DKSAD1Y$ElementName[c(1:4)],BD1Ys)))

# KSAD1P
BD1Ps <-  as.character(paste0("ksads_2_",seq(830, 839, 1),"_p", sep = ""))
BD1P <- KSAD1P %>% dplyr::select(any_of(c(DKSAD1P$ElementName[c(1:4)],BD1Ps)))

# Remove instances with non-administered modules
BD1Y <- BD1Y %>% 
  unite(TEMP, ksads_2_830_t:ksads_2_839_t, remove = F, sep = "") %>%
  filter(!(TEMP == "NNNNNNNNNN")) %>%
  select(-TEMP)

BD1P <- BD1P %>% 
  unite(TEMP, ksads_2_830_p:ksads_2_839_p, remove = F, sep = "") %>%
  filter(!(TEMP == "NNNNNNNNNN")) %>%
  select(-TEMP)

# Count visits for each individual
BD1Y_V <- BD1Y %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

BD1P_V <- BD1P %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

BD_V <- full_join(BD1Y_V, BD1P_V, by = c("subjectkey" = "subjectkey"))

BD_V[is.na(BD_V)] <- 0

BD_V <- BD_V %>%
  rowwise() %>%
  mutate(Assessments = sum(Visits.x, Visits.y, na.rm = TRUE), .keep = "unused")

BD1Y_S <- BD1Y %>%
  select(c(subjectkey, ksads_2_830_t:ksads_2_837_t)) %>%
  mutate(LifetimeBD1 = as.numeric(ksads_2_830_t) | as.numeric(ksads_2_831_t) | as.numeric(ksads_2_832_t)
         | as.numeric(ksads_2_833_t) | as.numeric(ksads_2_834_t),
         LifetimeBD2 = as.numeric(ksads_2_835_t) | as.numeric(ksads_2_836_t) | as.numeric(ksads_2_837_t),
         .keep = "unused") %>%
  mutate(LifetimeBD = as.numeric(LifetimeBD1|LifetimeBD2)) %>%
  group_by(subjectkey) %>%
  summarise(LifetimeBD = sum(as.numeric(LifetimeBD), na.rm = TRUE),
            LifetimeBD1 = sum(as.numeric(LifetimeBD1), na.rm = TRUE),
            LifetimeBD2 = sum(as.numeric(LifetimeBD2), na.rm = TRUE))

BD1P_S <- BD1P %>%
  select(c(subjectkey, ksads_2_830_p:ksads_2_837_p)) %>%
  mutate(LifetimeBD1 = as.numeric(ksads_2_830_p) | as.numeric(ksads_2_831_p) | as.numeric(ksads_2_832_p)
         | as.numeric(ksads_2_833_p) | as.numeric(ksads_2_834_p),
         LifetimeBD2 = as.numeric(ksads_2_835_p) | as.numeric(ksads_2_836_p) | as.numeric(ksads_2_837_p),
         .keep = "unused") %>%
  mutate(LifetimeBD = as.numeric(LifetimeBD1|LifetimeBD2)) %>%
  group_by(subjectkey) %>%
  summarise(LifetimeBD = sum(as.numeric(LifetimeBD), na.rm = TRUE),
            LifetimeBD1 = sum(as.numeric(LifetimeBD1), na.rm = TRUE),
            LifetimeBD2 = sum(as.numeric(LifetimeBD2), na.rm = TRUE))

BD <- full_join(BD1Y_S, BD1P_S, by = c("subjectkey" = "subjectkey"), suffix = c(".p", ".t")) %>%
  rowwise() %>%
  mutate(LifetimeBD = sum(LifetimeBD.p, LifetimeBD.t, na.rm = TRUE),
         LifetimeBD1 = sum(LifetimeBD1.p, LifetimeBD1.t, na.rm = TRUE),
         LifetimeBD2 = sum(LifetimeBD2.p, LifetimeBD2.t, na.rm = TRUE),
         .keep = "unused") %>%
  full_join(BD_V, by = c("subjectkey" = "subjectkey")) %>%
  mutate(Criterion = case_when(
      Assessments < 3 ~ 2,
      Assessments >= 3 ~ ceiling((Assessments+0.05)/2),
      TRUE ~ 0))

# Create Final Table
BD[is.na(BD)] <- 0

BD_FIN <- BD %>%
    mutate(nBD1 = as.numeric(LifetimeBD1  >= Criterion),
           bnBD1 = as.numeric(LifetimeBD1 > 0),
           nBD2 = as.numeric(LifetimeBD2 >= Criterion),
           bnBD2 = as.numeric(LifetimeBD2 > 0),
           nBD = as.numeric(LifetimeBD >= Criterion),
           bnBD = as.numeric(LifetimeBD > 0),
          .keep = "unused")

# Clear Dictionaries
DKSAD1Y <- DKSAD1Y %>% filter(!(ElementName %in% BD1Ys))
DKSAD1P <- DKSAD1P %>% filter(!(ElementName %in% BD1Ps))

# Clear space
rm(BD1Ys, BD1Ps, BD1Y, BD1P, BD1Y_V, BD1P_V, BD_V, BD1Y_S, BD1P_S)

### 03 Disruptive Mood Dysregulation Disorder (Removed from dataset, SKIP)

### 04 Psychosis

# KSAD1P
PSY1Ps <-  as.character(c(paste0("ksads_4_",seq(826, 829, 1),"_p", sep = ""), paste0("ksads_4_",seq(849, 852, 1),"_p", sep = "")))
PSY1P <- KSAD1P %>% dplyr::select(any_of(c(DKSAD1P$ElementName[c(1:4)],PSY1Ps)))

# KSAD2P
PSY2Ps <-  as.character(c(paste0("ksads2_4_",seq(805, 808, 1),"_p", sep = "")))
PSY2P <- KSAD2P %>% dplyr::select(any_of(c(DKSAD2P$ElementName[c(1:4)],PSY2Ps)))

# Remove instances with non-administered modules
PSY1P <- PSY1P %>% 
  unite(TEMP, ksads_4_826_p:ksads_4_850_p, remove = F, sep = "") %>%
  filter(!(TEMP == "NNNNNN")) %>%
  select(-TEMP)

PSY2P <- PSY2P %>% 
  unite(TEMP, ksads2_4_805_p:ksads2_4_807_p, remove = F, sep = "") %>%
  filter(!(TEMP == "NNN")) %>%
  select(-TEMP)

# Count visits for each individual
PSY1P_V <- PSY1P %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

PSY2P_V <- PSY2P %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

PSY_V <- full_join(PSY1P_V, PSY2P_V, by = c("subjectkey" = "subjectkey"))

PSY_V[is.na(PSY_V)] <- 0

PSY_V <- PSY_V %>%
  rowwise() %>%
  mutate(Assessments = sum(Visits.x, Visits.y, na.rm = TRUE),
         .keep = "unused")

PSY1P_S <- PSY1P %>%
  select(c(subjectkey, ksads_4_826_p:ksads_4_850_p)) %>%
  mutate(LifetimePSY = as.numeric(ksads_4_826_p) | as.numeric(ksads_4_827_p) | as.numeric(ksads_4_828_p)
         | as.numeric(ksads_4_829_p) | as.numeric(ksads_4_849_p) | as.numeric(ksads_4_850_p),
         .keep = "unused")

PSY2P_S <- PSY2P %>%
  select(c(subjectkey, ksads2_4_805_p:ksads2_4_807_p)) %>%
  mutate(LifetimePSY = as.numeric(ksads2_4_805_p) | as.numeric(ksads2_4_806_p) | as.numeric(ksads2_4_807_p),
         .keep = "unused")

PSY_S <- rbind(select(PSY1P_S, subjectkey, LifetimePSY), 
               select(PSY2P_S, subjectkey, LifetimePSY)) %>%
  group_by(subjectkey) %>%
  summarise(LifetimePSY = sum(as.numeric(LifetimePSY), na.rm = TRUE))

PSY <- PSY_S %>%
  full_join(PSY_V, by = c("subjectkey" = "subjectkey"))%>%
  mutate(Criterion = case_when(
      Assessments < 3 ~ 2,
      Assessments >= 3 ~ ceiling((Assessments+0.05)/2),
      TRUE ~ 0))

# Create Final Table
PSY[is.na(PSY)] <- 0

PSY_FIN <- PSY %>%
    mutate(nPSY = as.numeric(LifetimePSY >= Criterion),
           bnPSY = as.numeric(LifetimePSY > 0),
          .keep = "unused")

# Clear Dictionaries
DKSAD1P <- DKSAD1P %>% filter(!(ElementName %in% PSY1Ps))
DKSAD2P <- DKSAD2P %>% filter(!(ElementName %in% PSY2Ps))

# Clear space
rm(PSY1Ps, PSY2Ps, PSY1P, PSY2P, PSY1P_S, PSY2P_S, PSY1P_V, PSY2P_V, PSY_V, PSY_S)

### 05 Panic disorder

PD1Ps <- c("ksads_5_857_p", "ksads_5_858_p", "ksads_5_906_p", "ksads_5_907_p")
PD1P <- KSAD1P %>% dplyr::select(any_of(c(DKSAD1P$ElementName[c(1:4)],PD1Ps)))

# Remove instances with non-administered modules
PD1P <- PD1P %>% 
  unite(TEMP, ksads_5_857_p, ksads_5_858_p, ksads_5_906_p, ksads_5_907_p, remove = F, sep = "") %>%
  filter(!(TEMP == "NNNN")) %>%
  select(-TEMP)

# Count visits for each individual
PD1P_V <- PD1P %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

PD1P_S <- PD1P %>%
  select(c(subjectkey, ksads_5_857_p, ksads_5_858_p)) %>%
  mutate(LifetimePD = as.numeric(ksads_5_857_p) | as.numeric(ksads_5_858_p), .keep = "unused")  %>%
  group_by(subjectkey) %>%
  summarise(LifetimePD = sum(as.numeric(LifetimePD), na.rm = TRUE))

PD <- PD1P_S %>%
  full_join(PD1P_V, by = c("subjectkey" = "subjectkey")) %>%
  mutate(Assessments = Visits, 
        .keep = "unused") %>%
  mutate(Criterion = case_when(
      Assessments < 3 ~ 2,
      Assessments >= 3 ~ ceiling((Assessments+0.05)/2),
      TRUE ~ 0))

# Create Final Table
PD[is.na(PD)] <- 0

PD_FIN <- PD %>%
    mutate(nPD = as.numeric(LifetimePD >= Criterion),
           bnPD = as.numeric(LifetimePD > 0),
          .keep = "unused")

# Clear Dictionaries
DKSAD1P <- DKSAD1P %>% filter(!(ElementName %in% PD1Ps))

# Clear space
rm(PD1Ps, PD1P, PD1P_V, PD1P_S)

### 06 Agoraphobia (Removed from dataset, SKIP)

### 07 Separation anxiety disorder

# KSAD1P
SeAD1Ps <- c("ksads_7_861_p", "ksads_7_862_p", "ksads_7_909_p", "ksads_7_910_p")
SeAD1P <- KSAD1P %>% dplyr::select(any_of(c(DKSAD1P$ElementName[c(1:4)],SeAD1Ps)))

# Remove instances with non-administered modules
SeAD1P <- SeAD1P %>% 
  unite(TEMP, ksads_7_861_p, ksads_7_862_p, ksads_7_909_p, ksads_7_910_p, remove = F, sep = "") %>%
  filter(!(TEMP == "NNNN")) %>%
  select(-TEMP)

# Count visits for each individual
SeAD1P_V <- SeAD1P %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

SeAD1P_S <- SeAD1P %>%
  select(c(subjectkey, ksads_7_861_p, ksads_7_862_p)) %>%
  mutate(LifetimeSeAD = as.numeric(ksads_7_861_p) | as.numeric(ksads_7_862_p), .keep = "unused")  %>%
  group_by(subjectkey) %>%
  summarise(LifetimeSeAD = sum(as.numeric(LifetimeSeAD), na.rm = TRUE))

SeAD <- SeAD1P_S %>%
  full_join(SeAD1P_V, by = c("subjectkey" = "subjectkey")) %>%
  mutate(Assessments = Visits, 
        .keep = "unused") %>%
  mutate(Criterion = case_when(
      Assessments < 3 ~ 2,
      Assessments >= 3 ~ ceiling((Assessments+0.05)/2),
      TRUE ~ 0))

# Create Final Table
SeAD[is.na(SeAD)] <- 0

SeAD_FIN <- SeAD %>%
    mutate(nSeAD = as.numeric(LifetimeSeAD >= Criterion),
           bnSeAD = as.numeric(LifetimeSeAD > 0),
          .keep = "unused")

# Clear Dictionaries
DKSAD1P <- DKSAD1P %>% filter(!(ElementName %in% SeAD1Ps))

# Clear space
rm(SeAD1Ps, SeAD1P, SeAD1P_V, SeAD1P_S)

### 08 Social anxiety disorder / Selective mutism

# KSAD1Y
SoAD1Ys <- c("ksads_8_863_t", "ksads_8_864_t", "ksads_8_911_t", "ksads_8_912_t")
SoAD1Y <- KSAD1Y %>% dplyr::select(any_of(c(DKSAD1Y$ElementName[c(1:4)],SoAD1Ys)))

# KSAD1P
SoAD1Ps <- c("ksads_8_863_p", "ksads_8_864_p", "ksads_8_911_p", "ksads_8_912_p")
SoAD1P <- KSAD1P %>% dplyr::select(any_of(c(DKSAD1P$ElementName[c(1:4)],SoAD1Ps)))

# Remove instances with non-administered modules
SoAD1Y <- SoAD1Y %>% 
  unite(TEMP, ksads_8_863_t, ksads_8_864_t, ksads_8_911_t, ksads_8_912_t, remove = F, sep = "") %>%
  filter(!(TEMP == "NNNN")) %>%
  select(-TEMP)

SoAD1P <- SoAD1P %>% 
  unite(TEMP, ksads_8_863_p, ksads_8_864_p, ksads_8_911_p, ksads_8_912_p, remove = F, sep = "") %>%
  filter(!(TEMP == "NNNN")) %>%
  select(-TEMP)

# Count visits for each individual
SoAD1Y_V <- SoAD1Y %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

SoAD1P_V <- SoAD1P %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

SoAD_V <- full_join(SoAD1Y_V, SoAD1P_V, by = c("subjectkey" = "subjectkey"))

SoAD_V[is.na(SoAD_V)] <- 0

SoAD_V <- SoAD_V %>%
  rowwise() %>%
  mutate(Assessments = sum(Visits.x, Visits.y, na.rm = TRUE),
         .keep = "unused")

SoAD1Y_S <- SoAD1Y %>%
  select(c(subjectkey, ksads_8_863_t, ksads_8_864_t)) %>%
  mutate(LifetimeSoAD = as.numeric(ksads_8_863_t) | as.numeric(ksads_8_864_t), .keep = "unused")  %>%
  group_by(subjectkey) %>%
  summarise(LifetimeSoAD = sum(as.numeric(LifetimeSoAD), na.rm = TRUE))

SoAD1P_S <- SoAD1P %>%
  select(c(subjectkey, ksads_8_863_p, ksads_8_864_p)) %>%
  mutate(LifetimeSoAD = as.numeric(ksads_8_863_p) | as.numeric(ksads_8_864_p), .keep = "unused")  %>%
  group_by(subjectkey) %>%
  summarise(LifetimeSoAD = sum(as.numeric(LifetimeSoAD), na.rm = TRUE))

SoAD <- SoAD1P_S %>%
  full_join(SoAD1Y_S, by = c("subjectkey" = "subjectkey"), suffix = c(".p", ".t")) %>%
  rowwise() %>%
  mutate(LifetimeSoAD = sum(LifetimeSoAD.p, LifetimeSoAD.t, na.rm = TRUE),
         .keep = "unused") %>%
  full_join(SoAD_V, by = c("subjectkey" = "subjectkey")) %>%
  mutate(Criterion = case_when(
      Assessments < 3 ~ 2,
      Assessments >= 3 ~ ceiling((Assessments+0.05)/2),
      TRUE ~ 0))

# Create Final Table
SoAD[is.na(SoAD)] <- 0

SoAD_FIN <- SoAD %>%
    mutate(nSoAD = as.numeric(LifetimeSoAD >= Criterion),
           bnSoAD = as.numeric(LifetimeSoAD > 0),
          .keep = "unused")

# Clear Dictionaries
DKSAD1Y <- DKSAD1Y %>% filter(!(ElementName %in% SoAD1Ys))
DKSAD1P <- DKSAD1P %>% filter(!(ElementName %in% SoAD1Ps))

# Clear space
rm(SoAD1Ys, SoAD1Ps, SoAD1Y, SoAD1P, SoAD1Y_V, SoAD1P_V, SoAD_V, SoAD1Y_S, SoAD1P_S)

### 09 Specific phobias

# KSAD1P
SP1Ps <- c("ksads_9_867_p", "ksads_9_868_p")
SP1P <- KSAD1P %>% dplyr::select(any_of(c(DKSAD1P$ElementName[c(1:4)],SP1Ps)))

# Remove instances with non-administered modules
SP1P <- SP1P %>% 
  unite(TEMP, ksads_9_867_p, ksads_9_868_p, remove = F, sep = "") %>%
  filter(!(TEMP == "NN")) %>%
  select(-TEMP)

# Count visits for each individual
SP1P_V <- SP1P %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

SP1P_S <- SP1P %>%
  select(c(subjectkey, ksads_9_867_p, ksads_9_868_p)) %>%
  mutate(LifetimeSP = as.numeric(ksads_9_867_p) | as.numeric(ksads_9_868_p), .keep = "unused")  %>%
  group_by(subjectkey) %>%
  summarise(LifetimeSP = sum(as.numeric(LifetimeSP), na.rm = TRUE))

SP <- SP1P_S %>%
  full_join(SP1P_V, by = c("subjectkey" = "subjectkey")) %>%
  mutate(Assessments = Visits, 
        .keep = "unused") %>%
  mutate(Criterion = case_when(
      Assessments < 3 ~ 2,
      Assessments >= 3 ~ ceiling((Assessments+0.05)/2),
      TRUE ~ 0))

# Create Final Table
SP[is.na(SP)] <- 0

SP_FIN <- SP %>%
    mutate(nSP = as.numeric(LifetimeSP >= Criterion),
           bnSP = as.numeric(LifetimeSP > 0),
          .keep = "unused")

# Clear Dictionaries
DKSAD1P <- DKSAD1P %>% filter(!(ElementName %in% SP1Ps))

# Clear space
rm(SP1Ps, SP1P, SP1P_V, SP1P_S)

### 10 Generalized anxiety disorder

# KSAD1Y
GAD1Ys <- c("ksads_10_869_t", "ksads_10_870_t", "ksads_10_913_t", "ksads_10_914_t")
GAD1Y <- KSAD1Y %>% dplyr::select(any_of(c(DKSAD1Y$ElementName[c(1:4)],GAD1Ys)))

# KSAD1P
GAD1Ps <- c("ksads_10_869_p", "ksads_10_870_p", "ksads_10_913_p", "ksads_10_914_p")
GAD1P <- KSAD1P %>% dplyr::select(any_of(c(DKSAD1P$ElementName[c(1:4)],GAD1Ps)))

# Remove instances with non-administered modules
GAD1Y <- GAD1Y %>% 
  unite(TEMP, ksads_10_869_t, ksads_10_870_t, ksads_10_913_t, ksads_10_914_t, remove = F, sep = "") %>%
  filter(!(TEMP == "NNNN")) %>%
  select(-TEMP)

GAD1P <- GAD1P %>% 
  unite(TEMP, ksads_10_869_p, ksads_10_870_p, ksads_10_913_p, ksads_10_914_p, remove = F, sep = "") %>%
  filter(!(TEMP == "NNNN")) %>%
  select(-TEMP)

# Count visits for each individual
GAD1Y_V <- GAD1Y %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

GAD1P_V <- GAD1P %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

GAD_V <- full_join(GAD1Y_V, GAD1P_V, by = c("subjectkey" = "subjectkey"))

GAD_V[is.na(GAD_V)] <- 0

GAD_V <- GAD_V %>%
  rowwise() %>%
  mutate(Assessments = sum(Visits.x, Visits.y, na.rm = TRUE),
         .keep = "unused")

GAD1Y_S <- GAD1Y %>%
  select(c(subjectkey, ksads_10_869_t, ksads_10_870_t)) %>%
  mutate(LifetimeGAD = as.numeric(ksads_10_869_t) | as.numeric(ksads_10_870_t), .keep = "unused")  %>%
  group_by(subjectkey) %>%
  summarise(LifetimeGAD = sum(as.numeric(LifetimeGAD), na.rm = TRUE))

GAD1P_S <- GAD1P %>%
  select(c(subjectkey, ksads_10_869_p, ksads_10_870_p)) %>%
  mutate(LifetimeGAD = as.numeric(ksads_10_869_p) | as.numeric(ksads_10_870_p), .keep = "unused")  %>%
  group_by(subjectkey) %>%
  summarise(LifetimeGAD = sum(as.numeric(LifetimeGAD), na.rm = TRUE))

GAD <- full_join(GAD1Y_S, GAD1P_S, by = c("subjectkey" = "subjectkey"), suffix = c(".p", ".t")) %>%
  rowwise() %>%
  mutate(LifetimeGAD = sum(LifetimeGAD.p, LifetimeGAD.t, na.rm = TRUE),
         .keep = "unused") %>%
  full_join(GAD_V, by = c("subjectkey" = "subjectkey")) %>%
  mutate(Criterion = case_when(
      Assessments < 3 ~ 2,
      Assessments >= 3 ~ ceiling((Assessments+0.05)/2),
      TRUE ~ 0))

# Create Final Table
GAD[is.na(GAD)] <- 0

GAD_FIN <- GAD %>%
    mutate(nGAD = as.numeric(LifetimeGAD >= Criterion),
           bnGAD = as.numeric(LifetimeGAD > 0),
          .keep = "unused")

# Clear Dictionaries
DKSAD1Y <- DKSAD1Y %>% filter(!(ElementName %in% GAD1Ys))
DKSAD1P <- DKSAD1P %>% filter(!(ElementName %in% GAD1Ps))

# Clear space
rm(GAD1Ys, GAD1Ps, GAD1Y, GAD1P, GAD1Y_V, GAD1P_V, GAD_V, GAD1Y_S, GAD1P_S)

### 11 Obsessive-compulsive disorder

OCD1Ps <- c("ksads_11_917_p", "ksads_11_918_p", "ksads_11_919_p", "ksads_11_920_p")
OCD1P <- KSAD1P %>% dplyr::select(any_of(c(DKSAD1P$ElementName[c(1:4)],OCD1Ps)))

OCD1P <- OCD1P %>% 
  unite(TEMP, ksads_11_917_p:ksads_11_920_p, remove = F, sep = "") %>%
  filter(!(TEMP == "NNNN")) %>%
  select(-TEMP)

# Count visits for each individual
OCD1P_V <- OCD1P %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

# Split into 2-visit and 1-visit cohorts
OCD1P_S <- OCD1P %>%
  select(c(subjectkey, ksads_11_917_p, ksads_11_918_p)) %>%
  mutate(LifetimeOCD = as.numeric(ksads_11_917_p) | as.numeric(ksads_11_918_p), .keep = "unused")  %>%
  group_by(subjectkey) %>%
  summarise(LifetimeOCD = sum(as.numeric(LifetimeOCD), na.rm = TRUE))

OCD <- OCD1P_S %>%
  full_join(OCD1P_V, by = c("subjectkey" = "subjectkey")) %>%
  mutate(Assessments = Visits, 
        .keep = "unused") %>%
  mutate(Criterion = case_when(
      Assessments < 3 ~ 2,
      Assessments >= 3 ~ ceiling((Assessments+0.05)/2),
      TRUE ~ 0))

# Create Final Table
OCD[is.na(OCD)] <- 0

OCD_FIN <- OCD %>%
    mutate(nOCD = as.numeric(LifetimeOCD >= Criterion),
           bnOCD = as.numeric(LifetimeOCD > 0),
          .keep = "unused")

# Clear Dictionaries
DKSAD1P <- DKSAD1P %>% filter(!(ElementName %in% OCD1Ps))

# Clear space
rm(OCD1Ps, OCD1P, OCD1P_V, OCD1P_S)

### 12 Enurisis (Removed from dataset, SKIP)

### 13 Encopresis (Removed from dataset, SKIP)

### 14 Eating or feeding disorders (Anorexia nervosa data missing, SKIP AN only)

# KSAD1P
EFD1Ps <-  as.character(c(paste0("ksads_13_",seq(935, 944, 1),"_p", sep = "")))
EFD1P <- KSAD1P %>% dplyr::select(any_of(c(DKSAD1P$ElementName[c(1:4)],EFD1Ps)))

# KSAD1Y
EFD1Ys <-  as.character(c(paste0("ksads_13_",seq(935, 944, 1),"_t", sep = "")))
EFD1Y <- KSAD1Y %>% dplyr::select(any_of(c(DKSAD1Y$ElementName[c(1:4)],EFD1Ys)))

# KSAD2P
EFD2Ps <-  as.character(c(paste0("ksads2_13_",seq(896, 904, 1),"_p", sep = "")))
EFD2P <- KSAD2P %>% dplyr::select(any_of(c(DKSAD2P$ElementName[c(1:4)],EFD2Ps)))

# Remove instances with non-administered modules
EFD1P <- EFD1P %>% 
  unite(TEMP, ksads_13_935_p:ksads_13_944_p, remove = F, sep = "") %>%
  filter(!(TEMP == "NNNNNNNNNN")) %>%
  select(-TEMP)

EFD1Y  <- EFD1Y %>% 
  unite(TEMP, ksads_13_935_t:ksads_13_944_t, remove = F, sep = "") %>%
  filter(!(TEMP == "NNNNNNNNNN")) %>%
  select(-TEMP)

EFD2P <- EFD2P %>% 
  unite(TEMP, ksads2_13_896_p:ksads2_13_904_p, remove = F, sep = "") %>%
  filter(!(TEMP == "NNNNNNNNN")) %>%
  select(-TEMP)

# Count visits for each individual
EFD1P_V <- EFD1P %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

EFD1Y_V <- EFD1Y %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

EFD2P_V <- EFD2P %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

EFD_V <- full_join(EFD1P_V, EFD1Y_V, EFD2P_V, by = c("subjectkey" = "subjectkey"))

EFD_V[is.na(EFD_V)] <- 0

EFD_V <- EFD_V %>%
  rowwise() %>%
  mutate(Assessments = sum(Visits.x, Visits.y, na.rm = TRUE),
         .keep = "unused")

EFD1P_S <- EFD1P %>%
  select(c(subjectkey, ksads_13_935_p:ksads_13_940_p)) %>%
  mutate(LifetimeBN = as.numeric(ksads_13_935_p) | as.numeric(ksads_13_936_p) | as.numeric(ksads_13_937_p),
         LifetimeBED = as.numeric(ksads_13_938_p) | as.numeric(ksads_13_939_p) | as.numeric(ksads_13_940_p),
        .keep = "unused") %>%
  mutate(LifetimeEFD = as.numeric(LifetimeBN | LifetimeBED))

EFD1Y_S <- EFD1Y %>%
  select(c(subjectkey, ksads_13_935_t:ksads_13_940_t)) %>%
  mutate(LifetimeBN = as.numeric(ksads_13_935_t) | as.numeric(ksads_13_936_t) | as.numeric(ksads_13_937_t),
         LifetimeBED = as.numeric(ksads_13_938_t) | as.numeric(ksads_13_939_t) | as.numeric(ksads_13_940_t),
        .keep = "unused") %>%
  mutate(LifetimeEFD = as.numeric(LifetimeBN | LifetimeBED))

EFD2P_S <- EFD2P %>%
  select(c(subjectkey, ksads2_13_896_p:ksads2_13_900_p)) %>%
  mutate(LifetimeBN = as.numeric(ksads2_13_896_p) | as.numeric(ksads2_13_897_p) | as.numeric(ksads2_13_898_p),
         LifetimeBED = as.numeric(ksads2_13_899_p) | as.numeric(ksads2_13_900_p),
        .keep = "unused") %>%
  mutate(LifetimeEFD = as.numeric(LifetimeBN | LifetimeBED))

EFD_PS <- rbind(select(EFD1P_S, subjectkey, LifetimeBED, LifetimeBN, LifetimeEFD), 
               select(EFD2P_S, subjectkey, LifetimeEFD, LifetimeBED, LifetimeBN)) %>%
  group_by(subjectkey) %>%
  summarise(LifetimeEFD = sum(as.numeric(LifetimeEFD), na.rm = TRUE),
            LifetimeBED = sum(as.numeric(LifetimeBED), na.rm = TRUE),
            LifetimeBN = sum(as.numeric(LifetimeBN), na.rm = TRUE))

EFD_P <- EFD_PS %>%
  full_join(EFD_V, by = c("subjectkey" = "subjectkey")) 

EFD <- EFD_P %>%
  full_join(EFD1Y_S, by = c("subjectkey" = "subjectkey"), suffix = c(".p", ".t")) %>%
  rowwise() %>%
  mutate(LifetimeBED = sum(LifetimeBED.p, LifetimeBED.t, na.rm = TRUE),
           LifetimeBN = sum(LifetimeBN.p, LifetimeBN.t, na.rm = TRUE),
           LifetimeEFD = sum(LifetimeEFD.p, LifetimeEFD.t, na.rm = TRUE),
           .keep = "unused") %>%
  mutate(Criterion = case_when(
      Assessments < 3 ~ 2,
      Assessments >= 3 ~ ceiling((Assessments+0.05)/2),
      TRUE ~ 0))

# Create Final Table
EFD[is.na(EFD)] <- 0

EFD_FIN <- EFD %>%
    mutate(nBED = as.numeric(LifetimeBED >= Criterion),
           bnBED = as.numeric(LifetimeBED > 0),
           nBN = as.numeric(LifetimeBN >= Criterion),
           bnBN = as.numeric(LifetimeBN > 0),
           nEFD = as.numeric(LifetimeEFD >= Criterion),
           bnEFD = as.numeric(LifetimeEFD > 0),
          .keep = "unused")

# Clear Dictionaries
DKSAD1P <- DKSAD1P %>% filter(!(ElementName %in% EFD1Ps))
DKSAD2P <- DKSAD2P %>% filter(!(ElementName %in% EFD2Ps))
DKSAD1Y <- DKSAD2P %>% filter(!(ElementName %in% EFD1Ys))

# Clear space
rm(EFD1Ps, EFD2Ps, EFD1P, EFD2P, EFD1P_S, EFD2P_S, EFD1P_V, EFD2P_V, EFD_V, EFD_PS, EFD_P, EFD1Y, EFD1Y_S, EFD1Y_V)

### 15 Attention Deficit/Hyperactivity Disorder (Removed from dataset, SKIP)

### 16 Oppositional Defiant Disorder

# KSAD1P
ODD1Ps <- c("ksads_15_901_p", "ksads_15_902_p")
ODD1P <- KSAD1P %>% dplyr::select(any_of(c(DKSAD1P$ElementName[c(1:4)],ODD1Ps)))

# Remove instances with non-administered modules
ODD1P <- ODD1P %>% 
  unite(TEMP, ksads_15_901_p, ksads_15_902_p, remove = F, sep = "") %>%
  filter(!(TEMP == "NN")) %>%
  select(-TEMP)

# Count visits for each individual
ODD1P_V <- ODD1P %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

ODD1P_S <- ODD1P %>%
  select(c(subjectkey, ksads_15_901_p, ksads_15_902_p)) %>%
  mutate(LifetimeODD = as.numeric(ksads_15_901_p) | as.numeric(ksads_15_902_p), .keep = "unused")  %>%
  group_by(subjectkey) %>%
  summarise(LifetimeODD = sum(as.numeric(LifetimeODD), na.rm = TRUE))

ODD <- ODD1P_S %>%
  full_join(ODD1P_V, by = c("subjectkey" = "subjectkey")) %>%
  mutate(Assessments = Visits, 
        .keep = "unused") %>%
  mutate(Criterion = case_when(
      Assessments < 3 ~ 2,
      Assessments >= 3 ~ ceiling((Assessments+0.05)/2),
      TRUE ~ 0))

# Create Final Table
ODD[is.na(ODD)] <- 0

ODD_FIN <- ODD %>%
    mutate(nODD = as.numeric(LifetimeODD >= Criterion),
           bnODD = as.numeric(LifetimeODD > 0),
          .keep = "unused")

# Clear Dictionaries
DKSAD1P <- DKSAD1P %>% filter(!(ElementName %in% ODD1Ps))
# Clear space
rm(ODD1Ps, ODD1P, ODD1P_V, ODD1P_S)

### 17 Conduct disorder

# KSAD1P
CD1Ps <-  as.character(c(paste0("ksads_16_",seq(897, 900, 1),"_p", sep = "")))
CD1P <- KSAD1P %>% dplyr::select(any_of(c(DKSAD1P$ElementName[c(1:4,6)],CD1Ps)))

# KSAD1Y
CD1Ys <-  as.character(c(paste0("ksads_16_",seq(897, 900, 1),"_t", sep = "")))
CD1Y <- KSAD1Y %>% dplyr::select(any_of(c(DKSAD1Y$ElementName[c(1:4,6)],CD1Ys)))

# KSAD2P
CD2Ps <-  as.character(c(paste0("ksads2_16_",seq(855, 858, 1),"_p", sep = "")))
CD2P <- KSAD2P %>% dplyr::select(any_of(c(DKSAD2P$ElementName[c(1:4,6)],CD2Ps)))

# Remove instances with non-administered modules
CD1P <- CD1P %>% 
  unite(TEMP, ksads_16_897_p:ksads_16_900_p, remove = F, sep = "") %>%
  filter(!(TEMP == "NNNN")) %>%
  select(-TEMP)

CD1Y  <- CD1Y %>% 
  unite(TEMP, ksads_16_897_t:ksads_16_900_t, remove = F, sep = "") %>%
  filter(!(TEMP == "NNNN")) %>%
  select(-TEMP)

CD2P <- CD2P %>% 
  unite(TEMP, ksads2_16_855_p:ksads2_16_858_p, remove = F, sep = "") %>%
  filter(!(TEMP == "NNNN")) %>%
  select(-TEMP)

# Count visits for each individual
CD1P_V <- CD1P %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

CD1Y_V <- CD1Y %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

CD2P_V <- CD2P %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

CD_V <- full_join(CD1P_V, CD1Y_V, CD2P_V, by = c("subjectkey" = "subjectkey"))

CD_V[is.na(CD_V)] <- 0

CD_V <- CD_V %>%
  rowwise() %>%
  mutate(Assessments = sum(Visits.x, Visits.y, na.rm = TRUE),
         .keep = "unused")

CD1P_S <- CD1P %>%
  select(c(subjectkey, ksads_16_897_p:ksads_16_900_p)) %>%
  mutate(LifetimeCD = as.numeric(ksads_16_897_p) | as.numeric(ksads_16_898_p) | as.numeric(ksads_16_899_p)| as.numeric(ksads_16_900_p),
         .keep = "unused")

CD1Y_S <- CD1Y %>%
  select(c(subjectkey, ksads_16_897_t:ksads_16_900_t)) %>%
  mutate(LifetimeCD = as.numeric(ksads_16_897_t) | as.numeric(ksads_16_898_t) | as.numeric(ksads_16_899_t)| as.numeric(ksads_16_900_t),
         .keep = "unused") %>%
  group_by(subjectkey) %>%
  summarise(LifetimeCD = sum(as.numeric(LifetimeCD), na.rm = TRUE)) 

CD2P_S <- CD2P %>%
  select(c(subjectkey, ksads2_16_855_p:ksads2_16_858_p)) %>%
  mutate(LifetimeCD = as.numeric(ksads2_16_855_p) | as.numeric(ksads2_16_856_p) | as.numeric(ksads2_16_857_p)| as.numeric(ksads2_16_858_p),
         .keep = "unused")

CD_PS <- rbind(select(CD1P_S, subjectkey, LifetimeCD), 
               select(CD2P_S, subjectkey, LifetimeCD)) %>%
  group_by(subjectkey) %>%
  summarise(LifetimeCD = sum(as.numeric(LifetimeCD), na.rm = TRUE))

CD_P <- CD_PS %>%
  full_join(CD_V, by = c("subjectkey" = "subjectkey")) 

CD <- CD_P %>%
  full_join(CD1Y_S, by = c("subjectkey" = "subjectkey"), suffix = c(".p", ".t")) %>%
  rowwise() %>%
  mutate(LifetimeCD = sum(LifetimeCD.p, LifetimeCD.t, na.rm = TRUE),
           .keep = "unused")  %>%
  mutate(Criterion = case_when(
      Assessments < 3 ~ 2,
      Assessments >= 3 ~ ceiling((Assessments+0.05)/2),
      TRUE ~ 0))

# Create Final Table
CD[is.na(CD)] <- 0

CD_FIN <- CD %>%
    mutate(nCD = as.numeric(LifetimeCD >= Criterion),
           bnCD = as.numeric(LifetimeCD > 0),
          .keep = "unused")


# Clear Dictionaries
DKSAD1P <- DKSAD1P %>% filter(!(ElementName %in% CD1Ps))
DKSAD2P <- DKSAD2P %>% filter(!(ElementName %in% CD2Ps))
DKSAD1Y <- DKSAD2P %>% filter(!(ElementName %in% CD1Ys))

# Clear space
rm(CD1Ps, CD2Ps, CD1P, CD2P, CD1P_S, CD2P_S, CD1P_V, CD2P_V, CD_V, CD_PS, CD_P, CD1Y, CD1Y_S, CD1Y_V)

### 18 Tic disorders (FOLLOWS ALTERNATE DIAGNOSTIC SCHEMA)

# KSAD2P
TicsP <- KSAD2P %>% 
  select(c("subjectkey", "interview_age", "sex", "eventname", "ksads2_17_99_p",
           "ksads2_17_100_p", "ksads2_17_101_p", "ksads2_17_102_p", 
           "ksads2_17_864_p", "ksads2_17_865_p"))

TicsP[TicsP == "888"] <- "0"

TicsP <- TicsP %>% transmute(Motor_X = ifelse(as.numeric(ksads2_17_99_p) == 1 | as.numeric(ksads2_17_100_p) == 1, 1, 0),
                            Phonic_X = ifelse(as.numeric(ksads2_17_101_p) == 1 | as.numeric(ksads2_17_102_p) == 1, 1, 0),
                            subjectkey = subjectkey)

TicsP <- TicsP %>% mutate(Narrow_CTD = case_when(
  Phonic_X == 1 & Motor_X == 1 ~ 2,
  Phonic_X == 0 & Motor_X == 1 ~ 1,
  Phonic_X == 1 & Motor_X == 0 ~ 1,
  Phonic_X == 0 & Motor_X == 0 ~ 0))

TicsP[is.na(TicsP)] <- 0

TD_FIN <- TicsP %>%
    mutate(nTD = as.numeric(Narrow_CTD == 2),
           bnTD = as.numeric(Narrow_CTD > 0)) %>%
    select(subjectkey, nTD, bnTD)

### 19 Autism Spectrum Disorder (Removed from dataset, SKIP)

### 20 Tobacco use disorder (Removed from dataset, SKIP)

### 21 Alcohol use disorder (Out of purview, SKIP)

### 22 Substance use disorder (Out of purview, SKIP)

### 23 Post traumatic stress disorder

# KSAD1P
PTSD1Ps <- c("ksads_21_921_p", "ksads_21_922_p", "ksads_21_923_p", "ksads_21_924_p")
PTSD1P <- KSAD1P %>% dplyr::select(any_of(c(DKSAD1P$ElementName[c(1:4)],PTSD1Ps)))

# Remove instances with non-administered modules
PTSD1P <- PTSD1P %>% 
  unite(TEMP, ksads_21_921_p, ksads_21_922_p, ksads_21_923_p, ksads_21_924_p, remove = F, sep = "") %>%
  filter(!(TEMP == "NNNN")) %>%
  select(-TEMP)

# Count visits for each individual
PTSD1P_V <- PTSD1P %>% 
  group_by(subjectkey) %>%
  summarise(Visits = n()) %>%
  ungroup()

PTSD1P_S <- PTSD1P %>%
  select(c(subjectkey, ksads_21_921_p, ksads_21_922_p)) %>%
  mutate(LifetimePTSD = as.numeric(ksads_21_921_p) | as.numeric(ksads_21_922_p), .keep = "unused")  %>%
  group_by(subjectkey) %>%
  summarise(LifetimePTSD = sum(as.numeric(LifetimePTSD), na.rm = TRUE))

PTSD <- PTSD1P_S %>%
  full_join(PTSD1P_V, by = c("subjectkey" = "subjectkey")) %>%
  mutate(Assessments = Visits, 
        .keep = "unused") %>%
  mutate(Criterion = case_when(
      Assessments < 3 ~ 2,
      Assessments >= 3 ~ ceiling((Assessments+0.05)/2),
      TRUE ~ 0))

# Create Final Table
PTSD[is.na(PTSD)] <- 0

PTSD_FIN <- PTSD %>%
    mutate(nPTSD = as.numeric(LifetimePTSD >= Criterion),
           bnPTSD = as.numeric(LifetimePTSD > 0),
          .keep = "unused")

# Clear Dictionaries
DKSAD1P <- DKSAD1P %>% filter(!(ElementName %in% PTSD1Ps))

# Clear space
rm(PTSD1Ps, PTSD1P, PTSD1P_V, PTSD1P_S)

### 24 Sleep problems (Out of purview, SKIP)

### 25 Self injourious behavior and suicide (Out of purview, SKIP)

### 26 Homicidal ideation (Out of purview, SKIP)

FullL <- list(BD_FIN, PSY_FIN, PD_FIN, SeAD_FIN, SoAD_FIN, SP_FIN, GAD_FIN, 
              OCD_FIN, EFD_FIN, ODD_FIN, CD_FIN, TD_FIN, PTSD_FIN)

Full <- FullL %>% 
    reduce(full_join, by = "subjectkey") %>%
    select(-starts_with("Assessment"))

Broad <- Full %>%
    select(c(subjectkey, starts_with("b")))

Narrow <- Full %>%
    select(c(subjectkey, starts_with("n")))

Narrow %>% filter(!is.na(nTD)) %>% select(c(nOCD,nTD)) %>% table(.)
Narrow %>% filter(!is.na(nTD)) %>% nrow(.)

### CREATING DATA FRAMES
NarrowNames <- colnames(Narrow[,-1])
BroadNames <- colnames(Broad[,-1])
Types <- c("Narrow", "Broad")

Prevs <- data.frame(
    Disorder=as.character(NULL),
    Type=as.character(NULL),
    Prevalence=as.numeric(NULL),
    PrevalenceU95=as.numeric(NULL),
    PrevalenceL95=as.numeric(NULL),
    stringsAsFactors=FALSE)

Comos <- data.frame(
    Primary=as.character(NULL),
    Secondary=as.character(NULL),
    Type=as.character(NULL),
    Comorbidity=as.numeric(NULL),
    ComorbidityU95=as.numeric(NULL),
    ComorbidityL95=as.numeric(NULL),
    stringsAsFactors=FALSE)

for(i in Types){
    
    if(i=="Narrow") {
    Names <- NarrowNames
    Data <- Narrow
    } else if(i=="Broad") {
    Names <- BroadNames
    Data <- Broad
    }
    
    for(j in Names){
        
        DatA <- pull(Data, j)
        
        VecT <- table(Primary=DatA, useNA="always")
        
        N <- sum(VecT[1:2])
        T <- sum(VecT[2])
        
        pEST <- T / N
        pUP <- pEST + 1.96 * sqrt((pEST*(1-pEST))/N)
        pDO <- pEST - 1.96 * sqrt((pEST*(1-pEST))/N)
        
        pRow <- data.frame(
            Disorder=j,
            Type=i,
            Prevalence=round(pEST*100, digits=2),
            PrevalenceU95=round(pUP*100, digits=2),
            PrevalenceL95=round(pDO*100, digits=2),
            stringsAsFactors=FALSE)
        
        Prevs <- rbind(Prevs, pRow)
        
        for(k in Names){
            
            DatB <- pull(Data, k)
            
            ConT <- table(Primary=DatA, Secondary=DatB, useNA="always")
            
            M <- sum(ConT[2,1:2])
            D <- sum(ConT[2,2])

            cEST <- D / M
            cUP <- cEST + 1.96 * sqrt((cEST*(1-cEST))/M)
            cDO <- cEST - 1.96 * sqrt((cEST*(1-cEST))/M) 
        
            cRow <- data.frame(
                Primary=j,
                Secondary=k,
                Type=i,
                Comorbidity=round(cEST*100, digits=2),
                ComorbidityU95=round(cUP*100, digits=2),
                ComorbidityL95=round(cDO*100, digits=2),
                stringsAsFactors=FALSE)
            
            Comos <- rbind(Comos, cRow)
        }
    }
}

filter(Prevs, Type=="Narrow")
filter(Prevs, Type=="Broad")

filter(Comos, Type=="Narrow" & Primary=="nOCD")
filter(Comos, Type=="Narrow" & Secondary=="nOCD")

### PLOTTING DATA

# Reference Data
pRefs <- data.frame(
    Disorder=c(       "BED",  "BD",   "BD1",  "BD2",  "CD",   "GAD",  "OCD",  "ODD",  "PD",   "PSY",  "PTSD",   "SP",     "SeAD",   "SoAD",   "TD"),
    Prevalence=c(     1.32,   1.90,   NA,     NA,     4.40,   1.00,   2.30,   12.00,  1.80,   1.20,   3.70,     21.60,    7.80,     7.70,     0.86),
    PrevalenceL95=c(  0.88,   1.31,   NA,     NA,     2.05,   0.41,   0.93,   9.65,   1.02,   0.79,   2.72,     18.46,    6.62,     6.52,     0.85),
    PrevalenceU95=c(  1.83,   2.49,   NA,     NA,     6.75,   1.59,   3.67,   14.35,  2.58,   1.75,   4.68,     24.74,    8.97,     8.88,     0.87),
    Type="Reference",
    stringsAsFactors=FALSE)

cpRefs <- data.frame(
    Secondary=c(    "BED",  "BD",   "BD1",  "BD2",  "CD",   "GAD",  "ODD",  "PD",   "PSY",  "PTSD",   "SP",   "SeAD",   "SoAD",   "TD"),
    Primary="OCD",
    Comorbidity=c(   7.9,   5.65,   2.31,   3.34,   6.67,   26.60,  43.33,  6.1,    1.7,    10.38,    12.8,   33.33,    13.6,     8.44),
    ComorbidityL95=c(6.0,   1.39,   0,      0.03,   1.85,   19.30,  27.38,  0.5,    0.78,   4.76,     4.7,    19.23,    10.3,     7.17),
    ComorbidityU95=c(9.8,   9.91,   5.07,   6.66,   21.32,  34.70,  60.8,   16.40,  2.62,   16,       23.9,   51.22,    17.1,     9.92),
    Type="Reference",
    stringsAsFactors=FALSE)

csRefs <- data.frame(
    Primary=c(      "BED",  "BD",   "BD1",  "BD2",  "CD",   "GAD",  "ODD",  "PD",   "PSY",  "PTSD",   "SP",   "SeAD",   "SoAD",   "TD"),
    Secondary="OCD",
    Comorbidity=c(  NA,     44.78,  NA,     NA,     NA,     10.38,  NA,     14.29,   15,     4.23,    NA,     9.2,      10.8,     13.89),
    ComorbidityL95=c(NA,    32.87,  NA,     NA,     NA,     8,      NA,     7.1,     8.42,   2.24,    NA,     7.79,     7.3,      8.83),
    ComorbidityU95=c(NA,    56.68,  NA,     NA,     NA,     13.36,  NA,     26.67,   21.58,  7.83,    NA,     10.83,    15.68,    18.94),
    Type="Reference",
    stringsAsFactors=FALSE)

cRefs <- rbind(cpRefs, csRefs)

PlotPrev <- Prevs %>%
    mutate(Disorder=gsub("n|bn", "", Disorder)) %>%
    mutate(Type=ifelse(Type=="Broad","Broad + Narrow",Type)) %>%
    filter(!Disorder %in% c("BN", "EFD")) %>%
    bind_rows(pRefs) %>%
    mutate(PrevalenceL95=ifelse(PrevalenceL95<0,0,PrevalenceL95))

PlotComo <- Comos %>%
    filter(Type=="Narrow" & (Primary=="nOCD" | Secondary=="nOCD") & !(Primary=="nOCD" & Secondary=="nOCD")) %>%
    mutate(Primary=gsub("n|bn", "", Primary),
          Secondary=gsub("n|bn", "", Secondary)) %>%
    filter(!Primary %in% c("BN", "EFD")) %>%
    filter(!Secondary %in% c("BN", "EFD")) %>%
    bind_rows(cRefs) %>%
    mutate(ComorbidityL95=ifelse(ComorbidityL95<0,0,ComorbidityL95))

# Plot Prevalences, aka Figure 5a-b

PlotPrevBN <- PlotPrev %>%
    filter(Type != "Narrow") %>%
    ggplot(aes(fill=Type, y=Prevalence, x=Disorder)) +
        geom_bar(position=position_dodge2(preserve="single"), stat="identity") + 
        geom_errorbar(aes(ymin=PrevalenceL95, ymax=PrevalenceU95), width=0.8, position=position_dodge(0.9)) +
        scale_fill_brewer(palette="Set1") +
        scale_y_continuous(breaks=seq(0, 40, by=5), limits=c(0, 40)) +
        aes(stringr::str_wrap(Disorder, 15)) +
        labs(x=NULL, y="Prevalence (%)", title=NULL, fill=NULL) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=1, size=15),
            axis.title = element_text(size=15),
            axis.text.y = element_text(angle=90, hjust=0.5, size=15),
            strip.background = element_rect(fill="transparent", color="transparent"),
            legend.position = "bottom",
            legend.title = element_text(vjust=1, size=18),
            legend.text = element_text(size=15, margin=margin(r=30, unit="pt")))

PlotPrevN <- PlotPrev %>%
    filter(Type != "Broad + Narrow") %>%
    ggplot(aes(fill=Type, y=Prevalence, x=Disorder)) +
        geom_bar(position=position_dodge2(preserve="single"), stat="identity") + 
        geom_errorbar(aes(ymin=PrevalenceL95, ymax=PrevalenceU95), width=0.8, position=position_dodge(0.9)) +
        scale_fill_brewer(palette="Set1") +
        scale_y_continuous(breaks=seq(0, 25, by=5), limits=c(0, 25)) +
        aes(stringr::str_wrap(Disorder, 15)) +
        labs(x=NULL, y="Prevalence (%)", title=NULL, fill=NULL) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=1, size=15),
            axis.title = element_text(size=15),
            axis.text.y = element_text(angle=90, hjust=0.5, size=15),
            legend.position = "bottom",
            legend.title = element_text(vjust=1, size=18),
            legend.text = element_text(size=15, margin=margin(r=30, unit="pt")))

plot_grid(PlotPrevBN, PlotPrevN, labels=c("A", "B"), ncol=1, label_size = 18)

ggsave("./Prevalences.png", height=12, width=10, 
       device="png", units="in", dpi=300, plot=last_plot(), 
       bg="white")

# Plot Comorbidities, aka Figure 5
PlotComoP <- PlotComo %>%
    filter(Primary=="OCD") %>%
    ggplot(aes(fill=Type, y=Comorbidity, x=Secondary)) + 
        geom_bar(position=position_dodge2(preserve="single"), stat="identity") + 
        geom_errorbar(aes(ymin=ComorbidityL95, ymax=ComorbidityU95), width=0.8, position=position_dodge(0.9)) +
        scale_fill_brewer(palette="Set1") +
        aes(stringr::str_wrap(Secondary, 15)) +
        labs(x = NULL, y = "Cormorbidity Rate (%)", title = "OCD Primary", fill="OCD diagnosis") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=1, size=15),
            plot.title = element_text(face="bold.italic", size=15, hjust=0),
            axis.title = element_text(size=15),
            axis.text.y = element_text(angle=90, hjust=0.5, size=15),
            legend.position = "none")

PlotComoS <- PlotComo %>%
    filter(Secondary=="OCD") %>%
    ggplot(aes(fill=Type, y=Comorbidity, x=Primary)) + 
        geom_bar(position=position_dodge2(preserve="single"), stat="identity") + 
        geom_errorbar(aes(ymin=ComorbidityL95, ymax=ComorbidityU95), width=0.8, position=position_dodge(0.9)) +
        scale_fill_brewer(palette="Set1") +
        coord_cartesian(ylim=c(0,100)) +
        aes(stringr::str_wrap(Primary, 15)) +
        labs(x = NULL, y = "Cormorbidity Rate (%)", title = "OCD Secondary", fill="OCD diagnosis") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=1, size=15),
            plot.title = element_text(face="bold.italic", size=15, hjust=0),
            axis.title = element_text(size=15),
            axis.text.y = element_text(angle=90, hjust=0.5, size=15),
            legend.position = "bottom",
            legend.title = element_text(vjust=1, size=18, margin=margin(r=15, unit="pt")),
            legend.text = element_text(size=15, margin=margin(r=15, unit="pt")))

plot_grid(PlotComoP, PlotComoS, labels=c("A", "B"), ncol=1, label_size = 18)

ggsave("./Comorbidities.png", height=12, width=10, 
       device="png", units="in", dpi=300, plot=last_plot(), 
       bg="white")
     
### CBCL ANALYSES

CBCL_OC <- CBCLR %>%
    filter(eventname %in% c("baseline_year_1_arm_1", "1_year_follow_up_y_arm_1", "2_year_follow_up_y_arm_1")) %>%
    select(subjectkey, sex, cbcl_q09_p, cbcl_q31_p, cbcl_q52_p, cbcl_q66_p, cbcl_q85_p, cbcl_q112_p) %>%
    rowwise() %>%
    mutate(OCS = sum(c(as.numeric(cbcl_q09_p), as.numeric(cbcl_q66_p)), na.rm=TRUE),
           OCP = sum(c(as.numeric(cbcl_q09_p), as.numeric(cbcl_q31_p), as.numeric(cbcl_q52_p), 
                       as.numeric(cbcl_q66_p), as.numeric(cbcl_q85_p), as.numeric(cbcl_q112_p)), na.rm=TRUE),
           OBS = as.numeric(cbcl_q09_p),
           COM = as.numeric(cbcl_q66_p),
           .keep="unused") %>%
    group_by(subjectkey, sex) %>%
    summarise(OCS = sum(as.numeric(OCS), na.rm = TRUE),
              OCP = sum(as.numeric(OCP), na.rm = TRUE),
              OBS = sum(as.numeric(OBS), na.rm = TRUE),
              COM = sum(as.numeric(COM), na.rm = TRUE),) %>%
    left_join(select(OCD, subjectkey, LifetimeOCD), by = c("subjectkey"="subjectkey")) %>%
    left_join(select(TicsP, subjectkey, Narrow_CTD), by = c("subjectkey"="subjectkey")) %>%
    rename("OCD"="LifetimeOCD", CTD="Narrow_CTD") %>%
    mutate(OCD_Text = case_when(
        OCD == 0 ~ "OCD Negative",
        OCD == 1 ~ "Broad OCD",
        OCD == 2 ~ "Narrow OCD"),
          CTD_Text = case_when(
        CTD == 0 ~ "TD Negative",
        CTD == 1 ~ "Broad TD",
        CTD == 2 ~ "Narrow TD"))

CBCL_OC$OCD_Text <- factor(CBCL_OC$OCD_Text, levels=c("OCD Negative", "Broad OCD", "Narrow OCD"))
CBCL_OC$CTD_Text <- factor(CBCL_OC$CTD_Text, levels=c("TD Negative", "Broad TD", "Narrow TD"))

CBCLHistOCS <- CBCL_OC %>%
    ggplot(aes(x=OCS)) +
        geom_histogram(binwidth=1, fill="firebrick3", color="white") +
        theme_bw() +
        labs(y=NULL, x=NULL, title="OCS") +
        theme(axis.text.x = element_text(size=15),
            plot.title = element_text(hjust = 0, size=15, face="plain"),
            axis.title = element_text(size=15),
            axis.text.y = element_text(angle=45, hjust=1, size=15),
            strip.background = element_blank(),
            strip.text = element_blank(),
            legend.position = "bottom",
            legend.title = element_text(vjust=1, size=15),
            legend.text = element_text(size=15)) +
        facet_wrap(. ~ OCD_Text, ncol=3, scales="free_y", strip.position="bottom")

CBCLHistOCP <- CBCL_OC %>%
    ggplot(aes(x=OCP)) +
        geom_histogram(binwidth=3, fill="firebrick3", color="white") +
        theme_bw() +
        labs(y=NULL, x=NULL, title="OCP") +
        theme(axis.text.x = element_text(size=15),
            plot.title = element_text(hjust = 0, size=15, face="plain"),
            axis.title = element_text(size=15),
            axis.text.y = element_text(angle=45, hjust=1, size=15),
            strip.background = element_blank(),
            strip.text = element_blank(),
            legend.position = "bottom",
            legend.title = element_text(vjust=1, size=15),
            legend.text = element_text(size=15)) +
        facet_wrap(. ~ OCD_Text, ncol=3, scales="free_y", strip.position="bottom")

CBCLHistCOM <- CBCL_OC %>%
    ggplot(aes(x=COM)) +
        geom_histogram(binwidth=1, fill="firebrick3", color="white") +
        theme_bw() +
        labs(y=NULL, x=NULL, title="Compulsions") +
        theme(axis.text.x = element_text(size=15),
            plot.title = element_text(hjust = 0, size=15, face="plain"),
            axis.title = element_text(size=15),
            axis.text.y = element_text(angle=45, hjust=1, size=15),
            strip.background = element_blank(),
            strip.text = element_blank(),
            legend.position = "bottom",
            legend.title = element_text(vjust=1, size=15),
            legend.text = element_text(size=15)) +
        facet_wrap(. ~ OCD_Text, ncol=3, scales="free_y", strip.position="bottom")

CBCLHistOBS <- CBCL_OC %>%
    ggplot(aes(x=OBS)) +
        geom_histogram(binwidth=1, fill="firebrick3", color="white") +
        theme_bw() +
        labs(y=NULL, x=NULL, title="Obsessions") +
        theme(axis.text.x = element_text(size=15),
            plot.title = element_text(hjust = 0, size=15, face="plain"),
            axis.title = element_text(size=15),
            axis.text.y = element_text(angle=45, hjust=1, size=15),
            strip.background = element_rect(fill="transparent", color="transparent"),
            strip.text = element_text(face="plain", size=15, hjust=0),
            strip.placement = "outside",
            legend.position = "bottom",
            legend.title = element_text(vjust=1, size=15),
            legend.text = element_text(size=15, face="plain")) +
        facet_wrap(. ~ OCD_Text, ncol=3, scales="free_y", strip.position="bottom")

CBCLHist <- plot_grid(CBCLHistOCS, CBCLHistOCP, CBCLHistCOM, CBCLHistOBS, ncol=1, label_size = 18)

CBCL_OC <- CBCL_OC %>%
    mutate(bnOCD = as.numeric(OCD > 0),
           nOCD = as.numeric(OCD > 1),
           bnTD = as.numeric(CTD > 0),
           nTD = as.numeric(CTD > 1))

OCS_OCD_B <- glm(as.factor(bnOCD) ~ OCS + sex, family=binomial(link='logit'), data=CBCL_OC)
OCS_OCD_N <- glm(as.factor(nOCD) ~ OCS + sex, family=binomial(link='logit'), data=CBCL_OC)

OCP_OCD_B <- glm(as.factor(bnOCD) ~ OCP + sex, family=binomial(link='logit'), data=CBCL_OC)
OCP_OCD_N <- glm(as.factor(nOCD) ~ OCP + sex, family=binomial(link='logit'), data=CBCL_OC)

COM_OCD_B <- glm(as.factor(bnOCD) ~ COM + sex, family=binomial(link='logit'), data=CBCL_OC)
COM_OCD_N <- glm(as.factor(nOCD) ~ COM + sex, family=binomial(link='logit'), data=CBCL_OC)

OBS_OCD_B <- glm(as.factor(bnOCD) ~ OBS + sex, family=binomial(link='logit'), data=CBCL_OC)
OBS_OCD_N <- glm(as.factor(nOCD) ~ OBS + sex, family=binomial(link='logit'), data=CBCL_OC)

OCD_OR <- as.data.frame(rbind(  
        cbind(OR = exp(coef(OCS_OCD_B)), exp(confint(OCS_OCD_B)), P=summary(OCS_OCD_B)$coefficients[,4], ND=OCS_OCD_B$null.deviance, D=OCS_OCD_B$deviance, OCD="bnOCD", X="OCS")["OCS",],
        cbind(OR = exp(coef(OCS_OCD_N)), exp(confint(OCS_OCD_N)), P=summary(OCS_OCD_N)$coefficients[,4], ND=OCS_OCD_N$null.deviance, D=OCS_OCD_N$deviance, OCD="nOCD", X="OCS")["OCS",],
    
        cbind(OR = exp(coef(OCP_OCD_B)), exp(confint(OCP_OCD_B)), P=summary(OCP_OCD_B)$coefficients[,4], ND=OCP_OCD_B$null.deviance, D=OCP_OCD_B$deviance, OCD="bnOCD", X="OCP")["OCP",],
        cbind(OR = exp(coef(OCP_OCD_N)), exp(confint(OCP_OCD_N)), P=summary(OCP_OCD_N)$coefficients[,4], ND=OCP_OCD_N$null.deviance, D=OCP_OCD_N$deviance, OCD="nOCD", X="OCP")["OCP",],
    
        cbind(OR = exp(coef(COM_OCD_B)), exp(confint(COM_OCD_B)), P=summary(COM_OCD_B)$coefficients[,4], ND=COM_OCD_B$null.deviance, D=COM_OCD_B$deviance, OCD="bnOCD", X="Compulsions")["COM",],
        cbind(OR = exp(coef(COM_OCD_N)), exp(confint(COM_OCD_N)), P=summary(COM_OCD_N)$coefficients[,4], ND=COM_OCD_N$null.deviance, D=COM_OCD_N$deviance, OCD="nOCD", X="Compulsions")["COM",],
    
        cbind(OR = exp(coef(OBS_OCD_B)), exp(confint(OBS_OCD_B)), P=summary(OBS_OCD_B)$coefficients[,4], ND=OBS_OCD_B$null.deviance, D=OBS_OCD_B$deviance, OCD="bnOCD", X="Obsessions")["OBS",],
        cbind(OR = exp(coef(OBS_OCD_N)), exp(confint(OBS_OCD_N)), P=summary(OBS_OCD_N)$coefficients[,4], ND=OBS_OCD_N$null.deviance, D=OBS_OCD_N$deviance, OCD="nOCD", X="Obsessions")["OBS",]
))

OCD_OR <- OCD_OR %>%
    mutate(OR = as.numeric(OR),
           `2.5 %` = as.numeric(`2.5 %`),
           `97.5 %` = as.numeric(`97.5 %`),
            P = as.numeric(P),
            ND=as.numeric(ND),
            D=as.numeric(D)) %>%
    rowwise() %>%
    mutate(PseudoRsq=1-(D/ND))

OCD_OR$X <- factor(OCD_OR$X, levels=c("OCS", "OCP", "Compulsions", "Obsessions"))

CBCL_OC$PRED_OCS_B <- as.numeric(predict(OCS_OCD_B, type="response")>=0.5)
confusionMatrix(data=factor(CBCL_OC$PRED_OCS_B, levels=c(0,1)), reference=factor(CBCL_OC$bnOCD, levels=c(0,1)), positive="1")

CBCL_OC$PRED_OCS_N <- as.numeric(predict(OCS_OCD_N, type="response")>=0.5)
confusionMatrix(data=factor(CBCL_OC$PRED_OCS_N, levels=c(0,1)), reference=factor(CBCL_OC$nOCD, levels=c(0,1)), positive="1")

CBCL_OC$PRED_OCP_B <- as.numeric(predict(OCP_OCD_B, type="response")>=0.5)
confusionMatrix(data=factor(CBCL_OC$PRED_OCP_B, levels=c(0,1)), reference=factor(CBCL_OC$bnOCD, levels=c(0,1)), positive="1")

CBCL_OC$PRED_OCP_N <- as.numeric(predict(OCP_OCD_N, type="response")>=0.5)
confusionMatrix(data=factor(CBCL_OC$PRED_OCP_N, levels=c(0,1)), reference=factor(CBCL_OC$nOCD, levels=c(0,1)), positive="1")

CBCL_OC$PRED_COM_B <- as.numeric(predict(COM_OCD_B, type="response")>=0.5)
confusionMatrix(data=factor(CBCL_OC$PRED_COM_B, levels=c(0,1)), reference=factor(CBCL_OC$bnOCD, levels=c(0,1)), positive="1")

CBCL_OC$PRED_COM_N <- as.numeric(predict(COM_OCD_N, type="response")>=0.5)
confusionMatrix(data=factor(CBCL_OC$PRED_COM_N, levels=c(0,1)), reference=factor(CBCL_OC$nOCD, levels=c(0,1)), positive="1")

CBCL_OC$PRED_OBS_B <- as.numeric(predict(OBS_OCD_B, type="response")>=0.5)
confusionMatrix(data=factor(CBCL_OC$PRED_OBS_B, levels=c(0,1)), reference=factor(CBCL_OC$bnOCD, levels=c(0,1)), positive="1")

CBCL_OC$PRED_OBS_N <- as.numeric(predict(OBS_OCD_N, type="response")>=0.5)
confusionMatrix(data=factor(CBCL_OC$PRED_OBS_N, levels=c(0,1)), reference=factor(CBCL_OC$nOCD, levels=c(0,1)), positive="1")

CBCLOR <- OCD_OR %>% 
    ggplot(aes(x=OR, xmin=`2.5 %`, xmax=`97.5 %`, y=OCD)) +
        geom_point(color="firebrick3", size =2.5) +
        geom_errorbar(width=.3, color="firebrick1") +
        geom_vline(xintercept=1, linetype="dashed", color="darkgrey") +
        labs(y=NULL, x=NULL) +
        scale_x_continuous(breaks=seq(0.7, 3.6, 0.3), limits=c(0.9, 3.5)) +
        theme_bw() +
        theme(axis.text.x = element_text(size=15),
            plot.title = element_text(hjust = 0, size=18, face="plain"),
            axis.title = element_text(size=15),
            axis.text.y = element_text(angle=90, hjust=0.5, size=15),
            strip.background = element_rect(fill="transparent", color="transparent"),
            strip.text = element_text(face="plain", size=15, hjust=0),
            strip.placement = "outside",
            legend.position = "bottom",
            legend.title = element_text(vjust=1, size=18),
            legend.text = element_text(size=15)) +
        facet_wrap(.~X, ncol=1)

plot_grid(CBCLHist, CBCLOR, labels=c("A", "B"), nrow=1, label_size = 18)

ggsave("./CBCLPlot.png", height=12, width=16, 
       device="png", units="in", dpi=300, plot=last_plot(), 
       bg="white")

### FIGURE 2
plot_grid(plot_grid(PlotPrevBN, PlotPrevN, labels=c("A", "B"), ncol=1, label_size = 18),
		  CBCLHist, CBCLOR, labels=c("", "C", "D"), nrow=1, label_size = 18)

ggsave("./Figure2.png", height=12, width=20, 
       device="png", units="in", dpi=300, plot=last_plot(), 
       bg="white")
