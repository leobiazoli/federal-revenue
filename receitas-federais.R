# Predicting federal revenue collection for the Brazilian government

# Loading packages -----------------------------------------------------
library(ggplot2) 
library(tidyr) 
library(forecast) 
library(chron)
library(lubridate) 
library(ggthemes) 
library(tidyverse)
library(readxl)
library(tseries)
library(dplyr)
#install.packages("stringi", dependencies = TRUE)  # Reinstala o pacote
library(stringr)
library(stringi)

# Reading Data ----------------------------------------------

dados = read.csv('ipeadata[23-10-2025-04-05].csv', sep = ';', dec = ',')
dados2 = dados[13:nrow(dados),1:2]

colnames(dados2) <- c("mes", "receitas")

ipca = read.csv('ipeadata[23-10-2025-04-10].csv', dec = ',')

ipca2 = ipca[169:(nrow(ipca)-1),1:2]

colnames(ipca2) <- c("mes", "ipca")


meses_pt <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho", 
              "agosto", "setembro", "outubro", "novembro", "dezembro")

# Merge data

data_final = merge(dados2, ipca2, by = "mes")

colnames(data_final) = c('data', 'receita', 'ipca')

data_final$ipca <- as.numeric(data_final$ipca)

#data_final$receita_real = abs(data_final$receita/((1 + data_final$ipca)/1)) * data_final$ipca[371]

data_final$data = ym(data_final$data)

save(data_final, file = "receitas.csv")

# Rodar a partir daqui
load(file = "receitas.csv")

# 1. Converter o IPCA de percentual para taxa decimal
data_final$ipca_taxa <- data_final$ipca / 100

# 2. Calcular o índice acumulado (base = 1)
data_final$indice_acum <- cumprod(1 + data_final$ipca_taxa)

# 3. Definir o período-base (último mês da série)
base <- nrow(data_final)

# 4. Calcular a receita deflacionada (a preços constantes)
data_final$receita_real <- data_final$receita * 
  (data_final$indice_acum[base] / data_final$indice_acum)


ggplot(data_final, aes(data, receita)) +
  geom_line(aes(color = "Receitas nominais")) +
  geom_line(data = data_final, aes(data, receita_real, color = "Receitas reais"), size = 0.7) +
  # ADICIONE ESTA LINHA para definir as cores
  scale_color_manual(values = c("Receitas nominais" = "grey", 
                                "Receitas reais" = "darkblue")) +
  labs(color = "") +
  theme_classic(base_family = 'serif') +
  xlab('date') +
  ylab('Federal revenue (BRL million)') +
  scale_y_continuous(limits = c(0, 300000), n.breaks = 10) +
  scale_x_date(date_breaks = "3 year",
               limits = as.Date(c('1/1/1995', '1/8/2022'), format = "%d/%m/%Y"), 
               date_labels = "%m-%Y") +
  theme(legend.position = c(0.2, 0.98))

ggplot(data_final, aes(data, receita)) +
  # Alterado para "Nominal revenue"
  geom_line(aes(color = "Nominal revenue")) +
  # Alterado para "Real revenue"
  geom_line(data = data_final, aes(data, receita_real, color = "Real revenue"), size = 0.7) +
  
  # Valores do scale_color_manual atualizados para o inglês
  scale_color_manual(values = c("Nominal revenue" = "grey", 
                                "Real revenue" = "darkblue")) +
  
  labs(color = "") +
  theme_classic(base_family = 'serif') +
  xlab('Date') + # "período" ou "data" traduzido
  ylab('Federal revenue (BRL million)') +
  scale_y_continuous(limits = c(0, 300000), n.breaks = 10) +
  scale_x_date(date_breaks = "3 year",
               limits = as.Date(c('1/1/1995', '1/8/2022'), format = "%d/%m/%Y"), 
               date_labels = "%m-%Y") +
  theme(legend.position = c(0.2, 0.98))
data_final2 <- data_final |>
  filter(data >= as.Date("1995-01-01"))

y_ts <- ts(data = data_final2$receita_real,
           start = c(1995, 1), frequency = 12)

adf.test(y_ts) # serie estacionaria (valor-p = 0,01)
# adf.test(diff(y_ts)) 

# Separando dados ---------------------------------------------------------

n = round(.90*length(y_ts)) #90%
treino = y_ts[1:n] #tamanho 331
teste = y_ts[(n+1):nrow(data_final2)] #tamanho 37

# Modelo SARIMA -----------------------------------------------------------
library(tseries)
library(seastests)
plot(y_ts)

acf(diff(y_ts))

pacf(diff(y_ts))

isSeasonal(y_ts, freq = 12)

plot(decompose(y_ts))

model = auto.arima(treino, seasonal = T)

summary(model)

prev_xtreino<-forecast(model,h=37)

aj1 = prev_xtreino$mean

steps = 37

teste = teste[1:steps]

dfx = data.frame(index = 1:steps, teste, aj1)

names(dfx) = c('data', 'teste', 'ajuste')

rmse(aj1[1:37], teste[1:37])

Box.test(xtreino$residuals, type = c("Ljung-Box"))

# RMSE = 44.298

library(lmtest)

accuracy(model)
coeftest(model)
check_residuals(model)


tsdiag(model)

Box.test(model$residuals, lag = 1)

forecast2<-forecast(model,h=37)

autoplot(forecast2, fcol = 'red', xlab='per?odo', ylab='Receitas federais', main='') + geom_rangeframe() + theme_classic(base_family = 'serif')

# Modelo ARIMA(32,1,3) DE REDE NEURAL -----------------------------------
set.seed(2025)
M1 = nnetar(treino, lambda = 0.5, p = 32, d=1, q = 3, P = 1, size = 20)

# Previsao ----------------------------------------------------------------
steps = 37
prev1 = forecast(M1, h = steps, PI = T)
autoplot(prev1, fcol = 'red', xlab='per?odo', ylab='Receitas federais', main='') + geom_rangeframe() + theme_classic(base_family = 'serif')

# Comparando resultados ---------------------------------------------------
ajuste1 = prev1$mean
teste = teste[1:steps]
df = data.frame(index = 1:steps, teste, ajuste1,aj1)
names(df) = c('data', 'Serie real', 'RNA', 'SARIMA')

df$dat = seq(as.Date("2022/8/1"), by = "month", length.out = 37)

df %>% 
  pivot_longer(-c('dat','data'), names_to = 'var', values_to='receitas.federais') %>%
  ggplot(aes(x = dat, y=receitas.federais, color=var)) + geom_line(size=1.2) +
  xlab('período') + ylab('Receitas federias (R$ em milhões)') + theme_classic(base_family = 'serif')+
  scale_x_date(date_breaks = "4 month",
               limits = as.Date(c('1/8/2022', '1/8/2025'), format="%d/%m/%Y"), date_labels = "%m-%Y") + 
  theme(legend.position = c(0.2, 0.9), legend.title = element_blank())

df %>% 
  pivot_longer(-c('dat','data'), names_to = 'var', values_to='receitas.federais') %>%
  ggplot(aes(x = dat, y=receitas.federais, color=var)) + 
  geom_line(size=1.2) +
  
  # --- LINHA ADICIONADA ---
  # Aqui definimos as cores. Os nomes DEVEM CORRESPONDER 
  # exatamente aos nomes das colunas no seu 'df' antes do pivot_longer.
  scale_color_manual(values = c("Serie real" = "#000000", 
                                "RNA" = "#E69F00", 
                                "SARIMA" = "#56B4E9")) +
  # -------------------------

xlab('date') + 
  ylab('Federal revenue (BRL million)') + 
  theme_classic(base_family = 'serif') +
  scale_x_date(date_breaks = "4 month",
               limits = as.Date(c('1/8/2022', '1/8/2025'), format="%d/%m/%Y"), 
               date_labels = "%m-%Y") + 
  theme(legend.position = c(0.2, 0.9), legend.title = element_blank())
# RMSE --------------------------------------------------------------------
rmse = function(x, y){
  xx = (sum((x-y)**2)/length(x))**.5
  return(xx)
}
rmse(ajuste1[1:37], teste[1:37])
# RMSE=16681,36
# Modelo 3: write.csv(df, 'df_16681.csv', sep = ';')

write.csv(df, 'df_23153.csv', sep = ';')

# Previsao fora da amostra ------------------------------------------------
rede = nnetar(y_ts) #, p = 32, d=1, q = 3, P = 1, size = 20)
#previsao = prev$mean
step = 24
prev_fora = forecast(rede, h = step, PI = T)
autoplot(prev_fora, fcol = 'navy', xlab='year', ylab='Federal revenue', main = "") + geom_rangeframe() + theme_classic(base_family = 'serif')

# resultados 
ajuste1 = prev1$mean
ajuste1_v = prev1$lower[,2]
ajuste1_u = prev1$upper[,2]

# aj1 (prev_xtreino$mean)
aj1_l = prev_xtreino$lower[,2]
aj1_u = prev_xtreino$upper[,2]

teste = teste[1:steps]
df_res = data.frame(index = 1:steps, teste, ajuste1, ajuste1_v, ajuste1_u,aj1, aj1_l, aj1_u)

names(df_res) = c('data', 'Serie real', 'RNA', 'RNA_lower', 'RNA_upper', 
                  'SARIMA', 'SARIMA_lower', 'SARIMA_upper')

df_res |>
  filter(data > 25) |> 
  select(SARIMA_upper) |>
  view()
