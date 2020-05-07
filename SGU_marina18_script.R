library(tidyverse)
library(readxl)
library(MoCiS2) # devtools::install_github("NRM-MOC/MoCiS2")

# Cleans table of biodata to contain columns PROV_KOD_ORIGINAL, PROVTAG_DAT, TOTV,TOTL, ALDR, ANTAL_DAGAR, KON
biodata <- read_excel("data/marin biologdata 2018.xlsx", guess_max = 100000) %>% 
  select(PROV_KOD_ORIGINAL = ACC, PROVTAG_DAT = "Dödsdatum från", PROVTAG_DAT2 = "Dödsdatum till", Katalog, Kön, 
         TOTV = Vikt, TOTL = Totallängdä, ALDR = `Ålder från`) %>% 
  mutate(PROVTAG_DAT2 = as.Date(PROVTAG_DAT2),
         PROVTAG_DAT = as.Date(PROVTAG_DAT),
         ANTAL_DAGAR = as.numeric(PROVTAG_DAT2-PROVTAG_DAT),
         ANTAL_DAGAR = ifelse(is.na(ANTAL_DAGAR), 1, ANTAL_DAGAR), 
         KON = case_when(Katalog == "Ägg" ~ "I",
                         Katalog == "Mussla" ~ "H",
                         Kön == "Hane" ~ "M",
                         Kön == "Hona" ~ "F"),
         TOTV = as.numeric(TOTV),
         TOTL = as.numeric(TOTL),
         ALDR = as.numeric(ALDR)) %>% 
  select(-Katalog, -Kön, -PROVTAG_DAT2) %>% 
  # Date unavailable for the following
  mutate(PROVTAG_DAT = ifelse(PROV_KOD_ORIGINAL %in% c("C2018/04029", "C2018/04030", "C2018/04031", "C2018/04032", 
                                                       "C2018/04033", "C2018/04034", "C2018/04035", "C2018/04036", "C2018/04037", 
                                                       "C2018/04038", "C2018/04039", "C2018/04040"),
                              as.Date("2018-10-01"),
                              PROVTAG_DAT))




metaller <- moc_read_lab("data/Metaller_marina 2019_2018 prover.xlsm")
bromerade <- moc_read_lab("data/BFRs_Marina2019_2018ÅrsProver_200422.xlsm")
klorerade <- moc_read_lab("data/CLCs_Marina2019_2018ÅrsProver_200408.xlsm")
dioxiner <- moc_read_lab("data/Dioxins_marina 2019_2018 prover_NRM_20191018.xlsm")
hg <- moc_read_lab("data/Hg_marina 2019_2018 prover.xlsm")
pah <- moc_read_lab("data/IVL PAHs_marina 2019_2018 prover.xlsm")
tennorganiska <- moc_read_lab("data/IVL Tennorganiska_marina 2019_2018 prover_RA.xlsm")
pfas <- moc_read_lab("data/PFASs_marina 2019_2018 prover.xlsm")
sia <- moc_read_lab("data/SI_marina 2019_2018 prover.xlsm", negative_for_nondetect = FALSE)

analysdata <- bind_rows(metaller, 
                        bromerade, 
                        klorerade, 
                        dioxiner, 
                        hg, 
                        pah, 
                        tennorganiska, 
                        pfas, 
                        sia)

SGU <- moc_join_SGU(biodata, analysdata)

moc_write_SGU(SGU, "PROVMETADATA", "marina18_PROVMETADATA.xlsx", program = "hav")
moc_write_SGU(SGU, "PROVDATA_BIOTA", "marina18_PROVDATA_BIOTA.xlsx", program = "hav")
moc_write_SGU(SGU, "DATA_MATVARDE", "marina18_DATA_MATVARDE.xlsx", program = "hav")
