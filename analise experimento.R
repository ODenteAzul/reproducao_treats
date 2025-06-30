library(tidyverse)
library(naniar)
library(readxl)
library(writexl)
library(ggrepel)
library(kableExtra)
library(rebus)
library(TTR)
library(pracma)
library(tidyquant)
library(forecast)
library(janitor)
library(lmerTest)
library(emmeans)
library(lme4)
library(car)
library(broom)
library(sjPlot)
library(ggeffects)
library(performance)
library(lattice)
library(feather)

options(scipen=999)

cargill_color_blue <- c("#279989","#007681","#005F86")
cargill_color_red <- c("#9E2A2F","#BE531C","#CF7F00")
cargill_color_orange <- c("#DAAA00","#ABAD25","#658D1B")

cargill_color_all <- c("#9E2A2F","#BE531C","#CF7F00","#DAAA00","#ABAD25","#658D1B",
                       "#279989","#007681","#005F86")

#df <- read_excel("Coleta de dados experimento Aiden 2023 - leite.xlsx", "SAS Format 2")
#micotoxinas <- read_excel("Coleta de dados experimento Aiden 2023 - leite.xlsx", "micotoxinas")

#write_feather(df, "dados_exp.feather")
#write_feather(micotoxinas, "dados_micotoxinas.feather")

df <- read_feather("dados_exp.feather")
micotoxinas <- read_feather("dados_micotoxinas.feather")

# kg para Lb: 1 = 2.20462

df <- df %>%
  select(-183, -184)

#df$LACT <- ifelse(df$LACT == 1, "Primípara", "Multípara")





for(i in seq_len(nrow(df))){
  
  semana_inicial <- case_when(df[i,]$WEEKYEAR < 15 ~ 15,
                              .default = df[i,]$WEEKYEAR)
  
  posicao_inicial_semana <- case_when(df[i,]$WEEKYEAR < 15 ~ (15 - df[i,]$WEEKYEAR + 1),
                               .default = 1)
  
  for(j in seq(posicao_inicial_semana, 11)){

    if(semana_inicial <= 30){
      col_inicial <- case_when(j == 1 ~ 15,
                               j == 2 ~ 29,
                               j == 3 ~ 43,
                               j == 4 ~ 57,
                               j == 5 ~ 71,
                               j == 6 ~ 85,
                               j == 7 ~ 99,
                               j == 8 ~ 113,
                               j == 9 ~ 127,
                               j == 10 ~ 141,
                               j == 11 ~ 155)
      
      l <- 3
      
      for(k in seq(col_inicial,(col_inicial+7))){
        df[i, k] <- micotoxinas[micotoxinas$`Semana do ano` == semana_inicial, l]
        
        l <- l + 1
      }
      
      semana_inicial <- semana_inicial + 1
    }
  }
}





df_selected <- df %>%
  select(COW, WEEKYEAR, TREAT, LACT, BRIXCOL, starts_with("PROD"))


names(df_selected) <- c("COW","WEEK","TREAT" ,"LACT","BRIX_COL", "PROD01W",
                        "PROD02W", "PROD03W", "PROD04W", "PROD05W",
                        "PROD06W", "PROD07W", "PROD08W", "PROD09W",
                        "PROD10W", "PROD11W", "PROD12W")



new_pivot <- df_selected %>%
  pivot_longer(cols = c(PROD01W,PROD02W,PROD03W,PROD04W,
                        PROD05W,PROD06W,PROD07W,PROD08W,
                        PROD09W,PROD10W,PROD11W,PROD12W),
               values_to = "valores",
               names_to = "producao")






df_summ <- df %>%
  group_by(TREAT) %>%
  summarize(ECM_avg_w02 = mean(ECM2W, na.rm = TRUE), MF_avg_w02 = mean(MF2W, na.rm = TRUE), MP_avg_w02 = mean(MP2W, na.rm = TRUE), 
            ECM_avg_w03 = mean(ECM3W, na.rm = TRUE), MF_avg_w03 = mean(MF3W, na.rm = TRUE), MP_avg_w03 = mean(MP3W, na.rm = TRUE), 
            ECM_avg_w04 = mean(ECM4W, na.rm = TRUE), MF_avg_w04 = mean(MF4W, na.rm = TRUE), MP_avg_w04 = mean(MP4W, na.rm = TRUE),
            ECM_avg_w05 = mean(ECM5W, na.rm = TRUE), MF_avg_w05 = mean(MF5W, na.rm = TRUE), MP_avg_w05 = mean(MP5W, na.rm = TRUE),
            ECM_avg_w06 = mean(ECM6W, na.rm = TRUE), MF_avg_w06 = mean(MF6W, na.rm = TRUE), MP_avg_w06 = mean(MP6W, na.rm = TRUE),
            ECM_avg_w07 = mean(ECM7W, na.rm = TRUE), MF_avg_w07 = mean(MF7W, na.rm = TRUE), MP_avg_w07 = mean(MP7W, na.rm = TRUE),
            ECM_avg_w08 = mean(ECM8W, na.rm = TRUE), MF_avg_w08 = mean(MF8W, na.rm = TRUE), MP_avg_w08 = mean(MP8W, na.rm = TRUE),
            ECM_avg_w09 = mean(ECM9W, na.rm = TRUE), MF_avg_w09 = mean(MF9W, na.rm = TRUE), MP_avg_w09 = mean(MP9W, na.rm = TRUE),
            ECM_avg_w10 = mean(ECM10W, na.rm = TRUE), MF_avg_w10 = mean(MF10W, na.rm = TRUE), MP_avg_w10 = mean(MP10W, na.rm = TRUE),
            ECM_avg_w11 = mean(ECM11W, na.rm = TRUE), MF_avg_w11 = mean(MF11W, na.rm = TRUE), MP_avg_w11 = mean(MP11W, na.rm = TRUE),
            ECM_avg_w12 = mean(ECM12W, na.rm = TRUE), MF_avg_w12 = mean(MF12W, na.rm = TRUE), MP_avg_w12 = mean(MP12W, na.rm = TRUE),
            MPRod_avg_w02 = mean(PROD2W, na.rm = TRUE),
            MPRod_avg_w03 = mean(PROD3W, na.rm = TRUE),
            MPRod_avg_w04 = mean(PROD4W, na.rm = TRUE),
            MPRod_avg_w05 = mean(PROD5W, na.rm = TRUE),
            MPRod_avg_w06 = mean(PROD6W, na.rm = TRUE),
            MPRod_avg_w07 = mean(PROD7W, na.rm = TRUE),
            MPRod_avg_w08 = mean(PROD8W, na.rm = TRUE),
            MPRod_avg_w09 = mean(PROD9W, na.rm = TRUE),
            MPRod_avg_w10 = mean(PROD10W, na.rm = TRUE),
            MPRod_avg_w11 = mean(PROD11W, na.rm = TRUE),
            MPRod_avg_w12 = mean(PROD12W, na.rm = TRUE))

df_summ_lact <- df %>%
  group_by(TREAT, LACT) %>%
  summarize(ECM_avg_w02 = mean(ECM2W, na.rm = TRUE), MF_avg_w02 = mean(MF2W, na.rm = TRUE), MP_avg_w02 = mean(MP2W, na.rm = TRUE), 
            ECM_avg_w03 = mean(ECM3W, na.rm = TRUE), MF_avg_w03 = mean(MF3W, na.rm = TRUE), MP_avg_w03 = mean(MP3W, na.rm = TRUE), 
            ECM_avg_w04 = mean(ECM4W, na.rm = TRUE), MF_avg_w04 = mean(MF4W, na.rm = TRUE), MP_avg_w04 = mean(MP4W, na.rm = TRUE),
            ECM_avg_w05 = mean(ECM5W, na.rm = TRUE), MF_avg_w05 = mean(MF5W, na.rm = TRUE), MP_avg_w05 = mean(MP5W, na.rm = TRUE),
            ECM_avg_w06 = mean(ECM6W, na.rm = TRUE), MF_avg_w06 = mean(MF6W, na.rm = TRUE), MP_avg_w06 = mean(MP6W, na.rm = TRUE),
            ECM_avg_w07 = mean(ECM7W, na.rm = TRUE), MF_avg_w07 = mean(MF7W, na.rm = TRUE), MP_avg_w07 = mean(MP7W, na.rm = TRUE),
            ECM_avg_w08 = mean(ECM8W, na.rm = TRUE), MF_avg_w08 = mean(MF8W, na.rm = TRUE), MP_avg_w08 = mean(MP8W, na.rm = TRUE),
            ECM_avg_w09 = mean(ECM9W, na.rm = TRUE), MF_avg_w09 = mean(MF9W, na.rm = TRUE), MP_avg_w09 = mean(MP9W, na.rm = TRUE),
            ECM_avg_w10 = mean(ECM10W, na.rm = TRUE), MF_avg_w10 = mean(MF10W, na.rm = TRUE), MP_avg_w10 = mean(MP10W, na.rm = TRUE),
            ECM_avg_w11 = mean(ECM11W, na.rm = TRUE), MF_avg_w11 = mean(MF11W, na.rm = TRUE), MP_avg_w11 = mean(MP11W, na.rm = TRUE),
            ECM_avg_w12 = mean(ECM12W, na.rm = TRUE), MF_avg_w12 = mean(MF12W, na.rm = TRUE), MP_avg_w12 = mean(MP12W, na.rm = TRUE),
            MPRod_avg_w02 = mean(PROD2W, na.rm = TRUE),
            MPRod_avg_w03 = mean(PROD3W, na.rm = TRUE),
            MPRod_avg_w04 = mean(PROD4W, na.rm = TRUE),
            MPRod_avg_w05 = mean(PROD5W, na.rm = TRUE),
            MPRod_avg_w06 = mean(PROD6W, na.rm = TRUE),
            MPRod_avg_w07 = mean(PROD7W, na.rm = TRUE),
            MPRod_avg_w08 = mean(PROD8W, na.rm = TRUE),
            MPRod_avg_w09 = mean(PROD9W, na.rm = TRUE),
            MPRod_avg_w10 = mean(PROD10W, na.rm = TRUE),
            MPRod_avg_w11 = mean(PROD11W, na.rm = TRUE),
            MPRod_avg_w12 = mean(PROD12W, na.rm = TRUE),
            MPRod_avg_w01 = mean(PROD1W, na.rm = TRUE))


df_pivoted <- df_summ %>%
  pivot_longer(cols = c(MPRod_avg_w02, MPRod_avg_w03, MPRod_avg_w04, MPRod_avg_w05, MPRod_avg_w06, MPRod_avg_w07, MPRod_avg_w08,
                    MPRod_avg_w09, MPRod_avg_w10, MPRod_avg_w11, MPRod_avg_w12, MF_avg_w02, MF_avg_w03, MF_avg_w04, MF_avg_w05,
                    MF_avg_w06, MF_avg_w07, MF_avg_w08, MF_avg_w09, MF_avg_w10, MF_avg_w11, MF_avg_w12, MP_avg_w02, MP_avg_w03, MP_avg_w04, MP_avg_w05,
                    MP_avg_w06, MP_avg_w07, MP_avg_w08, MP_avg_w09, MP_avg_w10, MP_avg_w11, MP_avg_w12, ECM_avg_w02, ECM_avg_w03, ECM_avg_w04, ECM_avg_w05,
                    ECM_avg_w06, ECM_avg_w07, ECM_avg_w08, ECM_avg_w09, ECM_avg_w10, ECM_avg_w11, ECM_avg_w12))

df_pivoted_lact <- df_summ_lact %>%
  pivot_longer(cols = c(MPRod_avg_w01, MPRod_avg_w02, MPRod_avg_w03, MPRod_avg_w04, MPRod_avg_w05, MPRod_avg_w06, MPRod_avg_w07, MPRod_avg_w08,
                        MPRod_avg_w09, MPRod_avg_w10, MPRod_avg_w11, MPRod_avg_w12, MF_avg_w02, MF_avg_w03, MF_avg_w04, MF_avg_w05,
                        MF_avg_w06, MF_avg_w07, MF_avg_w08, MF_avg_w09, MF_avg_w10, MF_avg_w11, MF_avg_w12, MP_avg_w02, MP_avg_w03, MP_avg_w04, MP_avg_w05,
                        MP_avg_w06, MP_avg_w07, MP_avg_w08, MP_avg_w09, MP_avg_w10, MP_avg_w11, MP_avg_w12, ECM_avg_w02, ECM_avg_w03, ECM_avg_w04, ECM_avg_w05,
                        ECM_avg_w06, ECM_avg_w07, ECM_avg_w08, ECM_avg_w09, ECM_avg_w10, ECM_avg_w11, ECM_avg_w12))


df_pivoted$week <- case_when(
                            df_pivoted$name %in% c("MPRod_avg_w02", "MF_avg_w02", "MP_avg_w02", "ECM_avg_w02") ~ "Week 02",
                            df_pivoted$name %in% c("MPRod_avg_w03", "MF_avg_w03", "MP_avg_w03", "ECM_avg_w03") ~ "Week 03",
                            df_pivoted$name %in% c("MPRod_avg_w04", "MF_avg_w04", "MP_avg_w04", "ECM_avg_w04") ~ "Week 04",
                            df_pivoted$name %in% c("MPRod_avg_w05", "MF_avg_w05", "MP_avg_w05", "ECM_avg_w05") ~ "Week 05",
                            df_pivoted$name %in% c("MPRod_avg_w06", "MF_avg_w06", "MP_avg_w06", "ECM_avg_w06") ~ "Week 06",
                            df_pivoted$name %in% c("MPRod_avg_w07", "MF_avg_w07", "MP_avg_w07", "ECM_avg_w07") ~ "Week 07",
                            df_pivoted$name %in% c("MPRod_avg_w08", "MF_avg_w08", "MP_avg_w08", "ECM_avg_w08") ~ "Week 08",
                            df_pivoted$name %in% c("MPRod_avg_w09", "MF_avg_w09", "MP_avg_w09", "ECM_avg_w09") ~ "Week 09",
                            df_pivoted$name %in% c("MPRod_avg_w10", "MF_avg_w10", "MP_avg_w10", "ECM_avg_w10") ~ "Week 10",
                            df_pivoted$name %in% c("MPRod_avg_w11", "MF_avg_w11", "MP_avg_w11", "ECM_avg_w11") ~ "Week 11",
                            df_pivoted$name %in% c("MPRod_avg_w12", "MF_avg_w12", "MP_avg_w12", "ECM_avg_w12") ~ "Week 12"
                            )

df_pivoted$name <- case_when(
                            df_pivoted$name %in% c("MPRod_avg_w02","MPRod_avg_w03","MPRod_avg_w04","MPRod_avg_w05",
                                                   "MPRod_avg_w06","MPRod_avg_w07","MPRod_avg_w08","MPRod_avg_w09",
                                                   "MPRod_avg_w10","MPRod_avg_w11","MPRod_avg_w12") ~ "MProd",
                            df_pivoted$name %in% c("MF_avg_w02","MF_avg_w03","MF_avg_w04","MF_avg_w05",
                                                   "MF_avg_w06","MF_avg_w07","MF_avg_w08","MF_avg_w09",
                                                   "MF_avg_w10","MF_avg_w11","MF_avg_w12") ~ "MFat",
                            df_pivoted$name %in% c("MP_avg_w02","MP_avg_w03","MP_avg_w04","MP_avg_w05",
                                                   "MP_avg_w06","MP_avg_w07","MP_avg_w08","MP_avg_w09",
                                                   "MP_avg_w10","MP_avg_w11","MP_avg_w12") ~ "MProtein",
                            df_pivoted$name %in% c("ECM_avg_w02","ECM_avg_w03","ECM_avg_w04","ECM_avg_w05",
                                                   "ECM_avg_w06","ECM_avg_w07","ECM_avg_w08","ECM_avg_w09",
                                                   "ECM_avg_w10","ECM_avg_w11","ECM_avg_w12") ~ "ECM"
                            )

df_pivoted_lact$week <- case_when(
  df_pivoted_lact$name == "MPRod_avg_w01" ~ "Week 01",
  df_pivoted_lact$name %in% c("MPRod_avg_w02", "MF_avg_w02", "MP_avg_w02", "ECM_avg_w02") ~ "Week 02",
  df_pivoted_lact$name %in% c("MPRod_avg_w03", "MF_avg_w03", "MP_avg_w03", "ECM_avg_w03") ~ "Week 03",
  df_pivoted_lact$name %in% c("MPRod_avg_w04", "MF_avg_w04", "MP_avg_w04", "ECM_avg_w04") ~ "Week 04",
  df_pivoted_lact$name %in% c("MPRod_avg_w05", "MF_avg_w05", "MP_avg_w05", "ECM_avg_w05") ~ "Week 05",
  df_pivoted_lact$name %in% c("MPRod_avg_w06", "MF_avg_w06", "MP_avg_w06", "ECM_avg_w06") ~ "Week 06",
  df_pivoted_lact$name %in% c("MPRod_avg_w07", "MF_avg_w07", "MP_avg_w07", "ECM_avg_w07") ~ "Week 07",
  df_pivoted_lact$name %in% c("MPRod_avg_w08", "MF_avg_w08", "MP_avg_w08", "ECM_avg_w08") ~ "Week 08",
  df_pivoted_lact$name %in% c("MPRod_avg_w09", "MF_avg_w09", "MP_avg_w09", "ECM_avg_w09") ~ "Week 09",
  df_pivoted_lact$name %in% c("MPRod_avg_w10", "MF_avg_w10", "MP_avg_w10", "ECM_avg_w10") ~ "Week 10",
  df_pivoted_lact$name %in% c("MPRod_avg_w11", "MF_avg_w11", "MP_avg_w11", "ECM_avg_w11") ~ "Week 11",
  df_pivoted_lact$name %in% c("MPRod_avg_w12", "MF_avg_w12", "MP_avg_w12", "ECM_avg_w12") ~ "Week 12"
)

df_pivoted_lact$name <- case_when(
  df_pivoted_lact$name %in% c("MPRod_avg_w01", "MPRod_avg_w02","MPRod_avg_w03","MPRod_avg_w04","MPRod_avg_w05",
                         "MPRod_avg_w06","MPRod_avg_w07","MPRod_avg_w08","MPRod_avg_w09",
                         "MPRod_avg_w10","MPRod_avg_w11","MPRod_avg_w12") ~ "MProd",
  df_pivoted_lact$name %in% c("MF_avg_w02","MF_avg_w03","MF_avg_w04","MF_avg_w05",
                         "MF_avg_w06","MF_avg_w07","MF_avg_w08","MF_avg_w09",
                         "MF_avg_w10","MF_avg_w11","MF_avg_w12") ~ "MFat",
  df_pivoted_lact$name %in% c("MP_avg_w02","MP_avg_w03","MP_avg_w04","MP_avg_w05",
                         "MP_avg_w06","MP_avg_w07","MP_avg_w08","MP_avg_w09",
                         "MP_avg_w10","MP_avg_w11","MP_avg_w12") ~ "MProtein",
  df_pivoted_lact$name %in% c("ECM_avg_w02","ECM_avg_w03","ECM_avg_w04","ECM_avg_w05",
                         "ECM_avg_w06","ECM_avg_w07","ECM_avg_w08","ECM_avg_w09",
                         "ECM_avg_w10","ECM_avg_w11","ECM_avg_w12") ~ "ECM"
)



# todas juntas
df_prod_mean <- df_pivoted %>%
  filter(name == "MProd") %>%
  group_by(TREAT, week) %>%
  summarize(media_prd = mean(value))


# Visão Evolutiva da Produção
df_total_controle <- df_prod_mean %>%
  filter(TREAT == "CONTROLE")
df_total_maxima <- df_prod_mean %>%
  filter(TREAT == "MAXIMA")
df_total_repro <- df_prod_mean %>%
  filter(TREAT == "REPRO")
df_total_solidos <- df_prod_mean %>%
  filter(TREAT == "SOLIDOS")


total_controle <- sum(df_total_controle$media_prd)
total_maxima <- sum(df_total_maxima$media_prd)
total_repro <- sum(df_total_repro$media_prd)
total_solidos <- sum(df_total_solidos$media_prd)

resultado_maxima <- round(((total_maxima/total_controle)*100)-100,2)
resultado_repro <- round(((total_repro/total_controle)*100)-100,2)
resultado_solidos <- round(((total_solidos/total_controle)*100)-100,2)

ggplot(df_prod_mean, aes(week, media_prd, group = TREAT, color = TREAT)) + 
  geom_line() +
  geom_point()



#dividindo Multíparas e Primíparas
df_prod_mean_lact <- df_pivoted_lact %>%
  filter(name == "MProd") %>%
  group_by(TREAT, LACT, week) %>%
  summarize(media_prd = mean(value))


# Visão Evolutiva da Produção
df_total_controle <- df_prod_mean %>%
  filter(TREAT == "CONTROLE")
df_total_maxima <- df_prod_mean %>%
  filter(TREAT == "MAXIMA")
df_total_repro <- df_prod_mean %>%
  filter(TREAT == "REPRO")
df_total_solidos <- df_prod_mean %>%
  filter(TREAT == "SOLIDOS")


total_controle <- sum(df_total_controle$media_prd)
total_maxima <- sum(df_total_maxima$media_prd)
total_repro <- sum(df_total_repro$media_prd)
total_solidos <- sum(df_total_solidos$media_prd)

resultado_maxima <- round(((total_maxima/total_controle)*100)-100,2)
resultado_repro <- round(((total_repro/total_controle)*100)-100,2)
resultado_solidos <- round(((total_solidos/total_controle)*100)-100,2)

ggplot(df_prod_mean, aes(week, media_prd, group = TREAT, color = TREAT)) + 
  geom_line() +
  geom_point()



# Visão Evolutiva da Produção Corrigida (ECM - Fat Protein Corrected Milk)

df_prodECM_mean <- df_pivoted %>%
  filter(name == "ECM") %>%
  group_by(TREAT, week) %>%
  summarize(media_prd = mean(value))

std_dev_ECM <- std(df_prodECM_mean$media_prd)

total_controle_ECM <- df_prodECM_mean %>%
  filter(TREAT == "CONTROLE")
total_maxima_ECM <- df_prodECM_mean %>%
  filter(TREAT == "MAXIMA")
total_repro_ECM <- df_prodECM_mean %>%
  filter(TREAT == "REPRO")
total_solido_ECM <- df_prodECM_mean %>%
  filter(TREAT == "SOLIDOS")

total_controle_ECM <- sum(total_controle_ECM$media_prd)
total_maxima_ECM <- sum(total_maxima_ECM$media_prd)
total_repro_ECM <- sum(total_repro_ECM$media_prd)
total_solidos_ECM <- sum(total_solido_ECM$media_prd)

resultado_maxima_ECM <- round(((total_maxima_ECM/total_controle_ECM)*100)-100,2)
resultado_repro_ECM <- round(((total_repro_ECM/total_controle_ECM)*100)-100,2)
resultado_solidos_ECM <- round(((total_solidos_ECM/total_controle_ECM)*100)-100,2)

ggplot(df_prodECM_mean, aes(week, media_prd, group = TREAT, color = TREAT)) + 
  geom_line() +
  geom_point()




teste <- df_pivoted_lact %>%
  filter(name == "ECM") %>%
  group_by(TREAT, LACT) %>%
  summarize(Mprod_avg = mean(value, na.rm = TRUE))


# Produção média geral - corrigido e não corrigido

df_pivoted_lact %>%
  filter(name %in% c("ECM","MProd")) %>%
  group_by(TREAT, name) %>%
  summarize(Mprod_avg = median(value, na.rm = TRUE)) %>%
  ggplot(aes(TREAT, Mprod_avg, fill = TREAT)) + 
    geom_col(position = "dodge") + 
    theme_light() +
    scale_fill_manual(values = cargill_color_all) +
    labs(title = "Variação da Produção de Leite",
       subtitle = "Com e sem correção de Energia",
       x = "Alimentação",
       y = "Produção Média") +
  theme(legend.position = "none")


# Produção média geral - visão ampla

df_pivoted_lact %>%
  filter(name %in% c("ECM","MProd")) %>%
  group_by(TREAT, name) %>%
  ggplot(aes(TREAT, value, fill = TREAT)) + 
  geom_boxplot() + 
  theme_light() +
  scale_fill_manual(values = cargill_color_all) +
  labs(title = "Variação da Produção de Leite",
       subtitle = "Com e sem correção de Energia",
       x = "Alimentação",
       y = "Produção Média") +
  theme(legend.position = "none") 


# Produção média geral - corrigido e não corrigido - visão ampla

df_pivoted_lact %>%
  filter(name %in% c("ECM","MProd")) %>%
  group_by(TREAT, name) %>%
  ggplot(aes(TREAT, value, fill = TREAT)) + 
  geom_boxplot() +
  facet_wrap(~name) + 
  theme_light() +
  scale_fill_manual(values = cargill_color_all) +
  labs(title = "Variação da Produção de Leite",
       subtitle = "Com e sem correção de Energia",
       x = "Alimentação",
       y = "Produção Média") +
  theme(strip.background = element_rect(fill = "#005F86", colour = "#005F86"),
        legend.position = "none") 


# Produção média geral - milk fat

df_pivoted_lact %>%
  filter(name %in% c("MFat")) %>%
  group_by(TREAT, LACT) %>%
  ggplot(aes(TREAT, value, fill = TREAT)) + 
  geom_boxplot() +
  facet_wrap(~LACT) + 
  theme_light() +
  scale_fill_manual(values = cargill_color_all) +
  labs(title = "Variação da Produção de Leite",
       subtitle = "By Milk Fat",
       x = "Alimentação",
       y = "Produção Média") +
  theme(strip.background = element_rect(fill = "#005F86", colour = "#005F86"),
        legend.position = "none") #+
  #geom_label(aes(x = TREAT, y = Mprod_avg, label = round(Mprod_avg,2)), size = 3,
  #           position = position_dodge(0.9), vjust = -0.1, alpha = .6)

# Produção média geral - Protein

df_pivoted_lact %>%
  filter(name %in% c("MProtein")) %>%
  group_by(TREAT, LACT) %>%
  ggplot(aes(TREAT, value, fill = TREAT)) + 
  geom_boxplot() +
  facet_wrap(~LACT) + 
  theme_light() +
  scale_fill_manual(values = cargill_color_all) +
  labs(title = "Variação da Produção de Leite",
       subtitle = "By Milk Protein",
       x = "Alimentação",
       y = "Produção Média") +
  theme(strip.background = element_rect(fill = "#005F86", colour = "#005F86"),
        legend.position = "none") #+
  #geom_label(aes(x = TREAT, y = Mprod_avg, label = round(Mprod_avg,2)), size = 3,
  #           position = position_dodge(0.9), vjust = -0.1, alpha = .6)


# Produção média geral - primiparas e multíparas
for_plot <- df_pivoted_lact %>%
  filter(name == "MProd") %>%
  group_by(TREAT, LACT)

for_plot$prop <- case_when(for_plot$TREAT == "CONTROLE" ~ 0,
                           for_plot$TREAT == "MAXIMA" ~ resultado_maxima_ECM,
                           for_plot$TREAT == "REPRO" ~ resultado_repro_ECM,
                           for_plot$TREAT == "SOLIDOS" ~ resultado_solidos_ECM) 

ggplot(for_plot, aes(TREAT, value, fill = TREAT)) + 
  geom_boxplot() +
  facet_wrap(~LACT) +
  theme_light() +
  scale_fill_manual(values = cargill_color_all) +
  labs(title = "Variação da Produção de Leite em relação ao CONTROLE",
       subtitle = "Vacas Primíparas e Multíparas",
       x = "Alimentação",
       y = "Produção Média") +
  theme(strip.background = element_rect(fill = "#005F86", colour = "#005F86"),
        legend.position = "none")



#Wich side should it B? Based on T-dist or Normal?

# minha distribuição é normal (Shapiro_Wilk)

df_pivoted_teste_controle <- df_pivoted_lact %>%
  filter(name == "MProd", TREAT == "CONTROLE", LACT == 1)
df_pivoted_teste_solidos <- df_pivoted_lact %>%
  filter(name == "MProd", TREAT == "SOLIDOS", LACT == 1)
df_pivoted_teste_maxima <- df_pivoted_lact %>%
  filter(name == "MProd", TREAT == "MAXIMA", LACT == 1)
df_pivoted_teste_repro <- df_pivoted_lact %>%
  filter(name == "MProd", TREAT == "REPRO", LACT == 1)


shapiro.test(df_pivoted_teste_controle$value) # Não é normal: p-value = 0.0001198


df_pivoted_teste_controle <- df_pivoted_lact %>%
  filter(name == "MProd", TREAT == "CONTROLE", LACT == 2)
df_pivoted_teste_solidos <- df_pivoted_lact %>%
  filter(name == "MProd", TREAT == "SOLIDOS", LACT == 2)
df_pivoted_teste_maxima <- df_pivoted_lact %>%
  filter(name == "MProd", TREAT == "MAXIMA", LACT == 2)
df_pivoted_teste_repro <- df_pivoted_lact %>%
  filter(name == "MProd", TREAT == "REPRO", LACT == 2)


shapiro.test(df_pivoted_teste_controle$value) # Não é normal: p-value = 0.0007743


df_pivoted_teste_controle <- df_pivoted %>%
  filter(name == "MProd", TREAT == "CONTROLE")
df_pivoted_teste_solidos <- df_pivoted %>%
  filter(name == "MProd", TREAT == "SOLIDOS")
df_pivoted_teste_maxima <- df_pivoted %>%
  filter(name == "MProd", TREAT == "MAXIMA")
df_pivoted_teste_repro <- df_pivoted %>%
  filter(name == "MProd", TREAT == "REPRO")


shapiro.test(df_pivoted_teste_controle$value) # Não é normal: p-value = 0.0003086


# t.test

df_pivoted_controle <- df_pivoted_lact %>%
  filter(TREAT == "CONTROLE", name == "MProd")

df_pivoted_maxima <- df_pivoted_lact %>%
  filter(TREAT == "MAXIMA", name == "MProd")

df_pivoted_repro <- df_pivoted_lact %>%
  filter(TREAT == "REPRO", name == "MProd")

df_pivoted_solidos <- df_pivoted_lact %>%
  filter(TREAT == "SOLIDOS", name == "MProd")




# calculando SD, populacao e MEAN para uso no p-value
std_dev_controle <- std(df_pivoted_controle$value)
std_dev_maxima <- std(df_pivoted_maxima$value)
std_dev_repro <- std(df_pivoted_repro$value)
std_dev_solidos <- std(df_pivoted_solidos$value)

media_controle <- mean(df_pivoted_controle$value)
media_maxima <- mean(df_pivoted_maxima$value)
media_repro <- mean(df_pivoted_repro$value)
media_solidos <- mean(df_pivoted_solidos$value)

pop_controle <- nrow(df_pivoted_controle)
pop_maxima <- nrow(df_pivoted_maxima)
pop_repro <- nrow(df_pivoted_repro)
pop_solidos <- nrow(df_pivoted_solidos)



m1 <- c(media_controle, media_controle, media_controle)
m2 <- c(media_maxima, media_solidos, media_repro)
sd1 <- c(std_dev_controle, std_dev_controle, std_dev_controle)
sd2 <- c(std_dev_maxima, std_dev_solidos, std_dev_repro)
num1 <- c(pop_controle, pop_controle, pop_controle)
num2 <- c(pop_maxima, pop_solidos, pop_repro)

se <- sqrt(sd1*sd1/num1+sd2*sd2/num2)
t <- (m1-m2)/se

pt(t,df=pmin(num1, num2)-1)


new_pivot %>%
  plot_frq(valores, type = "histogram", show.mean = TRUE, normal.curve = TRUE)



shapiro.test(new_pivot$valores)



library(broom)
library(broom.mixed)

tidiado <- broom.mixed::tidy(fm1, conf.int = TRUE)

tidiado

lmer_coef %>%
  filter(effect == "fixed" & term != "(Intercept)") %>%
  ggplot(., aes(x = term, y = estimate,
                ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, color = 'red') + 
  geom_point() +
  geom_linerange() +
  coord_flip() +
  theme_bw() +
  ylab("Coefficient estimate and 95% CI") +
  xlab("Regression coefficient")



modelo_0 <- lmer(valores ~ producao + TREAT + LACT + WEEK + BRIX_COL + (producao | COW), REML = FALSE ,data = new_pivot)
modelo_1 <- lmer(valores ~ producao + (1 | COW), REML = FALSE ,data = new_pivot)

print(tidy(modelo_0), n = 150)

summary(modelo_0)
summary(modelo_1)
anova(modelo_0, modelo_1)

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     plot(vcov(modelo_0))

rampas <- fixef(modelo_0)

treat_slope <- ranef(modelo_0)

resultados <- data.frame(treat_slope)




ggplot(new_pivot, aes(producao, valores, group = COW, color = TREAT)) +
  geom_line() +
  scale_color_manual(values = c("#9E2A2F", "#CF7F00", "#ABAD25", "#279989")) +
  theme_bw()


ggplot(new_pivot, aes(producao, valores, group = COW, color = LACT)) +
  geom_line() +
  theme_bw()




# Instale os pacotes se ainda não tiverem sido instalados 

# install.packages("lme4") 

# Carregue as bibliotecas necessárias 



new_pivot %>% 
  group_by(TREAT) %>% 
  plot_frq(valores, type = "histogram", show.mean = TRUE, normal.curve = TRUE) %>% 
  plot_grid()



# Suponha que você tenha um dataframe chamado 'dados' com as variáveis necessárias 
# Ajuste um modelo linear misto

modelor <- lmer(valores ~ TREAT * producao + (1 | COW) + LACT + WEEK + BRIX_COL, data=new_pivot, control = lmerControl(optimizer ="Nelder_Mead"))

modelo <- lmer(valores ~ TREAT * producao + (TREAT | COW) + LACT + WEEK + BRIX_COL, data=new_pivot, control = lmerControl(optimizer ="Nelder_Mead"))
modelo1 <- lmer(valores ~ TREAT * producao + (TREAT | COW) + WEEK + BRIX_COL, data=new_pivot, control = lmerControl(optimizer ="Nelder_Mead"))
modelo2<- lmer(valores ~ TREAT * producao + (TREAT | COW) +  BRIX_COL, data=new_pivot, control = lmerControl(optimizer ="Nelder_Mead"))
modelo3 <- lmer(valores ~ TREAT * producao + (TREAT | COW), data=new_pivot, control = lmerControl(optimizer ="Nelder_Mead"))
modelo4 <- lmer(valores ~ TREAT + producao + (1 | COW) + LACT + WEEK + BRIX_COL, data=new_pivot, control = lmerControl(optimizer ="Nelder_Mead"))
modelot <- lmer(valores ~ COW * producao + (1 | TREAT) + LACT + WEEK + BRIX_COL, data=new_pivot, control = lmerControl(optimizer ="Nelder_Mead"))
modelodb <- lmer(valores ~ TREAT * producao + (1 | COW) + (1 | TREAT) + LACT + WEEK + BRIX_COL, data=new_pivot, control = lmerControl(optimizer ="Nelder_Mead"))
modelodb1<- lmer(valores ~ TREAT * producao + (1 | COW) + (1 | TREAT) + LACT + WEEK, data=new_pivot, control = lmerControl(optimizer ="Nelder_Mead"))


anova(modelor, modelo, modelo1, modelo2, modelo3, modelo4, modelot, modelodb, modelodb1)
model_performance(modelor)
model_performance(modelo)
model_performance(modelo1)
model_performance(modelo2)
model_performance(modelo3)
model_performance(modelo4)
model_performance(modelot)
model_performance(modelodb)
model_performance(modelodb1)


summary(modelodb1)
vcov(modelodb1)

anova(modelodb1)

plot(modelodb1)

plot_model(modelodb1, show.values = TRUE, width = 0.1)+
  ylab("Increase in salary as compared to no education")

tab_model(modelodb1, 
          show.reflvl = TRUE, 
          show.intercept = FALSE, 
          p.style = "numeric_stars")




plot_model(modelodb1, type = "pred", terms = c("producao","TREAT"),
           title = "Predição de Valores Baseados no Modelo",
           legend.title = "Tratamento")

modelodb1

plot_model(modelodb1, type = "pred", terms = c("WEEK","TREAT"))

plot_model(modelodb1, type = "pred", terms = c("LACT","TREAT"))



modelo_dados_prd <- as.data.frame(ggpredict(modelor, terms = c("producao","TREAT")))
modelo_dados_week <- as.data.frame(ggpredict(modelor, terms = c("WEEK","TREAT")))
modelo_dados_lact <- as.data.frame(ggpredict(modelor, terms = c("LACT","TREAT")))

modelo_dados_prd

df_embrio <- read_excel("Coleta de dados experimento Aiden 2023 - dados finais 10-2023 Tiago.xlsx", "SAS Format 2")

df_embrio <- df_embrio %>%
  filter(TREAT %in% c("CONTROLE", "REPRO"), SUPEROV == 1)

df_embrio$CQEI <- round(df_embrio$CQEI,1)

df_embrio %>% 
  filter(TREAT %in% c("CONTROLE", "REPRO")) %>%
  group_by(TREAT) %>% 
  plot_frq(TOTSTRUCT, type = "histogram", show.mean = TRUE) %>% 
  plot_grid()


model_embrio <- lmer(TOTSTRUCT ~ TREAT + FLUSHWEEK + (1 | COW), data = df_embrio, control = lmerControl(optimizer ="Nelder_Mead"))
m.nb <- lmer(TOTSTRUCT ~ TREAT + ( 1 | COW) + (1 | TREAT) + PROD2W + FLUSHDIM + FLUSHMILK + FLUSHMF + FLUSHSCC + LACTN + BSIRE + FLUSHWEEK, data = df_embrio, control = lmerControl(optimizer ="Nelder_Mead"))
m.nb1 <- lmer(TOTSTRUCT ~ TREAT + (1 | COW) + PROD2W + FLUSHDIM + FLUSHMF + FLUSHSCC + LACTN + FLUSHWEEK, data = df_embrio, control = lmerControl(optimizer ="Nelder_Mead"))
m.nb2 <- lmer(TOTSTRUCT ~ TREAT * (1 | COW) + PROD2W + FLUSHDIM + FLUSHMF + FLUSHSCC + LACTN + FLUSHWEEK + BSIRE, data = df_embrio, control = lmerControl(optimizer ="Nelder_Mead"))
m.nb3 <- lmer(TOTSTRUCT ~ TREAT + (1 | COW) + PROD2W + FLUSHDIM + FLUSHMF + FLUSHMILK + FLUSHSCC + LACTN + FLUSHWEEK + BSIRE, data = df_embrio, control = lmerControl(optimizer ="Nelder_Mead"))
m.nb4 <- lmer(TOTSTRUCT ~ TREAT * (TREAT | COW) + PROD2W + FLUSHDIM + FLUSHMILK + FLUSHMF + FLUSHSCC + LACTN + FLUSHWEEK, data = df_embrio, control = lmerControl(optimizer ="Nelder_Mead"))
m.nb5 <- lmer(TOTSTRUCT ~ TREAT * (1 | COW) + PROD2W + FLUSHDIM + FLUSHMILK + FLUSHMF + FLUSHSCC + FLUSHWEEK, data = df_embrio, control = lmerControl(optimizer ="Nelder_Mead"))
m.nball <- lmer(TOTSTRUCT ~ TREAT + ( 1 | COW) + PROD2W + FLUSHDIM + FLUSHMILK + FLUSHMF + FLUSHSCC + LACTN + FLUSHWEEK + CQEI, data = df_embrio, control = lmerControl(optimizer ="Nelder_Mead"))
m.nball_sem <- lmer(TOTSTRUCT ~ TREAT + ( 1 | COW) + PROD2W + FLUSHDIM + FLUSHMILK + FLUSHMF + FLUSHSCC + LACTN + FLUSHWEEK, data = df_embrio, control = lmerControl(optimizer ="Nelder_Mead"))
m.nball4 <- lmer(TOTSTRUCT ~ TREAT + (1 | COW) + PROD2W + FLUSHDIM + FLUSHWEEK + CQEI, data = df_embrio, control = lmerControl(optimizer ="Nelder_Mead"))
m.refitted <- lmer(TOTSTRUCT ~ TREAT + ( 1 | COW) + PROD2W + FLUSHDIM + FLUSHMILK + FLUSHMF + FLUSHSCC + LACTN + FLUSHWEEK + CQEI, data = df_embrio, control = lmerControl(optimizer ="Nelder_Mead"))

anova(m.nb, m.nb1, m.nb2, m.nb3, m.nb4, m.nb5, m.nball, m.nball_sem, m.nball4, m.refitted)

summary(m.refitted)
summary(m.nball)
summary(m.nball_sem)

model_performance(model_embrio)
model_performance(m.nb)
model_performance(m.nb1)
model_performance(m.nb2)
model_performance(m.nb3)
model_performance(m.nb4)
model_performance(m.nb5)
model_performance(m.nball)
model_performance(m.nball_sem)
model_performance(m.nball4)

tab_model(m.nball_sem)

plot(model_embrio)
plot(m.nb)
plot(m.nb1)
plot(m.nb2)
plot(m.nball_sem)
plot(m.nball)


df_embrio %>%
  filter(TREAT %in% c("CONTROLE", "REPRO")) %>%
  group_by(TREAT, FLUSHMF) %>%
  summarize(medias = n())

theme_set(theme_bw())
plot_grpfrq(df_embrio$TOTSTRUCT, df_embrio$TREAT, geom.colors = cargill_color_blue)


plot(m.nball)

plot_model(m.nball_sem, type = "pred", terms = c("TREAT", "FLUSHWEEK"))
plot_model(m.nball_sem, type = "pred", terms = c("TREAT", "FLUSHDIM"))
plot_model(m.nball_sem, type = "pred", terms = c("TREAT", "FLUSHMILK"))
plot_model(m.nball_sem, type = "pred", terms = c("TREAT", "FLUSHMF"))
plot_model(m.nball_sem, type = "pred", terms = c("TREAT", "FLUSHSCC"))
plot_model(m.nball_sem, type = "pred", terms = c("TREAT", "LACTN"))
plot_model(m.nball_sem, type = "pred", terms = c("TREAT", "CQEI"))


dotplot(ranef(m.nball_sem))

