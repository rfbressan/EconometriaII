library(ggplot2)
plot <- ggplot(subset(dados, Estado == "SC"), 
               aes(x = Municipio, y = PIB)) +
  geom_bar(stat = "identity", color = "blue", fill = "blue") +
  facet_wrap(facets = ~Ano)
plot

ggplot(data=dados, aes(POPULAÇÃO,fill = Estado)) + 
  geom_histogram(bins = 50) + xlim(0, 2e+06)

ggplot(data=subset(dados,Estado==c("SC","PR","RS")), aes(x=Estado,y=POPULAÇÃO)) + 
  geom_boxplot()

ggplot(data=dados, aes(x=POPULAÇÃO, y=PIB)) + 
  geom_point(shape=1) + 
  xlim(0, 2e+06) + 
  ylim(0,1e+11) + 
  geom_smooth()

# EStatistica descritiva --------------------------------------------------------------
library('psych')
# apresenta as estatisticas ano a ano
describeBy(dados[,4:8],dados$Ano)

library('corrplot')
corrplot(cor(dados[,4:8],method = c("spearman")),method = "circle")
pairs.panels(dados[,4:8],method = "spearman")

# Exemplo 2: Análise dos dados do IDEB dos municípios Brasileiros ----------------------

library(readxl)
tmp <- tempfile(fileext=".xlsx")
download.file("https://www.dropbox.com/s/jv6lxhytzvurq53/planilha_IDEB_workshop_R_2018.xlsx?dl=1",destfile=tmp, mode="wb")
dados2 <- read_excel(tmp, sheet=1, col_names = TRUE,col_types=NULL,na="",skip=0)
dados2$ideb_8 <- as.numeric(dados2$ideb_8)

library(gplots)
plotmeans(ideb_8 ~ regiao, main="Heterogeneidade entre as regiões", data=dados2)
plotmeans(ideb_8 ~ ano, main="Heterogeneidade entre os anos", data=dados2)
plotmeans(ideb_8 ~ uf, main="Heterogeneidade entre as UFs", data=dados2)

library('stargazer')
modelo <- lm(ideb_8~atu_8+ha_8+I(ha_8^2)+dist_8+dsu_8+reprov_8+aband_8+log(PIBpc),data=subset(dados2,ano=="2015"))
stargazer(modelo,type = "text")

# Exemplo 3: Variáveis macroeconômicas importadas diretamente da internet -----------------

library(BETS)
BETS.search(description = "IBC-BR", lang = "pt", view = F)

data <- BETS.get("24364")
plot(data)

library(forecast)
fit <- auto.arima(data, max.order=12, max.d=1)
future = forecast(fit, h = 12)
future
plot(future)

# Exemplo 4: Variáveis financeiras importadas diretamente da inter --------

library(BatchGetSymbols)
ticker <- 'PETR4.SA'
inicio <- Sys.Date()-100
fim <- Sys.Date()
l.out <- BatchGetSymbols(tickers = ticker,
                         first.date = inicio,
                         last.date = fim)

p <- ggplot(data = l.out$df.tickers, aes(x = ref.date, y = price.close))
p <- p + geom_line()
p <- p + labs(x = 'Dates', y = 'Adjusted closing prices')
print(p)

#Biblioteca ustyc: baixa dados da curva de juros dos EUA
library(ustyc)
library(tidyr)
library(ggplot2)
library(stringr)
# Baixa curva em 2018
my.df.yc <- getYieldCurve(year = 2018)$df
# formata
my.df.yc$ref.date <- as.Date(rownames(my.df.yc))
my.df.yc <- gather(data=my.df.yc, key =ref.date)
names(my.df.yc) <- c('ref.date', 'maturity', 'rate')
my.df.yc$maturity <- as.factor(my.df.yc$maturity)
idx <- str_detect(my.df.yc$maturity, 'YEAR')
my.df.yc <- my.df.yc[idx, ]
out <- str_extract_all(string = my.df.yc$maturity,
                       pattern = '([0-9]+)')
my.df.yc$maturity <- as.numeric(out)
last.date <- max(my.df.yc$ref.date)
my.df.yc.last.date <- my.df.yc[my.df.yc$ref.date == last.date, ]
# faz o gráfico
p <- ggplot(my.df.yc.last.date, aes(x=maturity, y=rate))
p <- p + geom_point(size=2)
p <- p + geom_line(size=1)
p <- p + labs(x = 'Maturity (years)', 
              y='Yield Rate',
              title = paste0('US Yield Curve (',last.date,')' ))
print(p)


# Pacote getTDData: baixa a curva de juros do Brasil ----------------------

library(GetTDData)

df.yield <- get.yield.curve()  
p <- ggplot(df.yield, aes(x=ref.date, y = value) ) +
  geom_line(size=1) + geom_point() + facet_grid(~type, scales = 'free') + 
  labs(title = paste0('The current Brazilian Yield Curve '),
       subtitle = paste0('Date: ', df.yield$current.date[1]))     
print(p)  
