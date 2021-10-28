#carregando pacotes
library(tidyverse)
library(mFilter)
library(readxl)
library(timeSeries)
library(magrittr)
library(DataEditR)
library(tseries)
library(lmtest)
library(stargazer)
library(broom)
### abrindo dados
df <- readxl::read_excel("tcc_data.xlsx", sheet = "deflacionar") %>%  as_tibble()

### pegando colunas que vamos utlizar
df <- df[,5:10]

#renomeando colunas
colnames(df) <- c("date", "desv_rec", "desv_des", "saldo_rcorr", "aporte_rec", "rcorr")

### normalizando pela receita corrente
df %>% select(
  saldo_rcorr,
  desv_rec,
  desv_des,
  aporte_rec,
  rcorr
) %>% mutate(
  saldoprev = saldo_rcorr / rcorr,
  recprev = desv_rec / rcorr,
  desprev = desv_des / rcorr,
  aporte = aporte_rec / rcorr
) -> dados_prev

## selecionando as variaveis normalizadas
prev_filtrado <- c(df["date"], dados_prev[,6:9]) %>%  as_tibble() %>% drop_na()

## visualizando as variaveis
ggplot(prev_filtrado, aes(x= date)) +
  geom_line(aes(y = saldoprev), color = "darkred") +
  geom_line(aes(y = recprev), color = "steelblue") +
  geom_line(aes(y = desprev), color = "darkgreen") +
  ggtitle("Evolução da receita, despesa e resultado previdenciário")

###aplicando hpfilter na receita e na despesa
rec_hp <- hpfilter(prev_filtrado$recprev, freq = 14400) 
des_hp <- hpfilter(prev_filtrado$desprev, freq = 14400)
prev_filtrado["recprev_hp"] <- rec_hp$trend
prev_filtrado["desprev_hp"] <- des_hp$trend

### coletando os desvios da receita e despesa previdenciaria
prev_filtrado %>% select(
  recprev,
  desprev,
  recprev_hp,
  desprev_hp
) %>% mutate(
  desv_rec = recprev - recprev_hp,
  desv_desp = desprev - desprev_hp
) -> hp_filtrado

#### criando novo tibble com as colunas que serao utilizadas na regressao
prev_filtrado_2 <- bind_cols(prev_filtrado$date, prev_filtrado$saldoprev, 
                             prev_filtrado$aporte, hp_filtrado$desv_rec, 
                             hp_filtrado$desv_desp)
colnames(prev_filtrado_2) <- c("date", "saldoprev", "aporte", "desv_rec", "desv_desp")


### colocando o aporte em t-1 
aporte <- prev_filtrado_2$aporte
aporte <- append(aporte, NA,0)
prev_filtrado_2["aporte"] <- aporte[1:64]
prev_filtrado_2 <- prev_filtrado_2[-1,]

### regressao
reg <- lm(saldoprev ~ aporte + desv_rec + desv_desp, data = prev_filtrado_2)

summary(reg)

estacionariedade <- adf.test(prev_filtrado_2$saldoprev)

stargazer(as.matrix(tidy(estacionariedade)), type = "latex")

stargazer(as.matrix(tidy(reg$coefficients)), type = "latex")

stargazer(as.matrix(tidy(reg)), type = "latex")

