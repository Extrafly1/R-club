library(ggplot2)
library(corrplot)
library(reshape2)

data <- read.csv("C:/Users/Extrafly/Desktop/API_GEO_DS2_en_csv_v2_100667.csv", 
                 stringsAsFactors = FALSE, check.names = FALSE)
country <- "Georgia"
country_data <- data[data[["Country Name"]] == country, ]


get_series <- function(code) {
  row <- country_data[country_data[["Indicator Code"]] == code, ]
  if(nrow(row)==0) return(rep(NA, length(5:ncol(country_data))))
  as.numeric(as.character(row[1, 5:ncol(row)]))
}
years <- as.numeric(colnames(data)[5:ncol(data)])

gdp         <- get_series("NY.GDP.MKTP.CD")
pop_growth  <- get_series("SP.POP.GROW")
unemp       <- get_series("SL.UEM.TOTL.ZS")
med_exp     <- get_series("SH.XPD.CHEX.GD.ZS")
life_exp    <- get_series("SP.DYN.LE00.IN")
death_rate  <- get_series("SP.DYN.CDRT.IN")
edu_higher  <- get_series("SE.TER.CUAT.BA.ZS")
export_val  <- get_series("TX.VAL.MRCH.CD.WT")
hi_tech     <- get_series("TX.VAL.TECH.CD")
edu_exp     <- get_series("SE.XPD.TOTL.GB.ZS")
female_ba   <- get_series("SE.TER.CUAT.BA.FE.ZS")
publications<- get_series("IP.JRN.ARTC.SC")

# Кривая ВВП
df_gdp <- data.frame(year = years, gdp = gdp)
ggplot(df_gdp, aes(x = year, y = gdp)) +
  geom_line(color = "royalblue", size = 1.2) +
  theme_minimal() +
  labs(title = "Рост ВВП", x = "Год", y = "ВВП (US$)")

# Функция для корреляций и графика
cor_plot <- function(x, y, xlab, ylab, main){
  plot(x, y, main = main, xlab = xlab, ylab = ylab, pch = 19, col = "steelblue")
  abline(lm(y ~ x), col = "red", lwd = 2)
  legend("topleft", legend = paste("Корреляция:", round(cor(x, y, use="complete.obs"),2)), 
         bty = "n")
}

# a) ВВП и прирост населения
cat("a) ВВП и прирост населения:\n")
cat("Корреляция:", cor(gdp, pop_growth, use="complete.obs"), "\n")
cor_plot(pop_growth, gdp, "Прирост населения (%)", "ВВП", "ВВП и прирост населения")

# b) Прирост населения и безработица
cat("b) Прирост населения и безработица:\n")
cat("Корреляция:", cor(pop_growth, unemp, use="complete.obs"), "\n")
cor_plot(pop_growth, unemp, "Прирост населения (%)", "Безработица (%)", "Прирост населения и безработица")

# c) Медицинские расходы, продолжительность жизни, смертность
cat("c) Медицинские расходы, продолжительность жизни, смертность:\n")
cat("Корреляция расходов на медицину и продолжительности жизни:", cor(med_exp, life_exp, use="complete.obs"), "\n")
cat("Корреляция расходов на медицину и смертности:", cor(med_exp, death_rate, use="complete.obs"), "\n")

# d) Высшее образование, экспорт, высокотехнологичное производство
cat("d) Высшее образование, экспорт, высокотехнологичное производство:\n")
cat("Корреляция высшего образования и экспорта:", cor(edu_higher, export_val, use="complete.obs"), "\n")
cat("Корреляция высшего образования и высокотехнологичного производства:", cor(edu_higher, hi_tech, use="complete.obs"), "\n")

# e) Расходы на образование и бакалавры-женщины
cat("e) Расходы на образование и бакалавры-женщины:\n")
cat("Корреляция:", cor(edu_exp, female_ba, use="complete.obs"), "\n")

# f) Высшее образование и публикации
cat("f) Высшее образование и публикации:\n")
cat("Корреляция:", cor(edu_higher, publications, use="complete.obs"), "\n")

# g) Найти и отобразить на графике наиболее коррелирующие параметры
df <- data.frame(years, gdp, pop_growth, unemp, med_exp, life_exp, death_rate, 
                 edu_higher, export_val, hi_tech, edu_exp, female_ba, publications)
cor_matrix <- cor(df, use="complete.obs")
corrplot(cor_matrix, method = "color", tl.cex=0.8)

# h) Регрессионный анализ
model <- lm(gdp ~ pop_growth + unemp + med_exp + life_exp + edu_higher + export_val + hi_tech, data = df)
cat("==== Итоги регрессии ====\n")
print(summary(model)$coefficients)
cat("R^2 =", summary(model)$adj.r.squared, "\n")

# i) Прогноз по любому атрибуту через predict()
future <- data.frame(
  pop_growth = mean(df$pop_growth, na.rm=TRUE),
  unemp = mean(df$unemp, na.rm=TRUE),
  med_exp = mean(df$med_exp, na.rm=TRUE),
  life_exp = mean(df$life_exp, na.rm=TRUE),
  edu_higher = mean(df$edu_higher, na.rm=TRUE),
  export_val = mean(df$export_val, na.rm=TRUE),
  hi_tech = mean(df$hi_tech, na.rm=TRUE)
)
cat("==== Прогноз ВВП по средним значениям факторов:\n")
print(predict(model, newdata=future))

# Дополнительно: прогноз на последние 5 лет
future_years <- data.frame(
  pop_growth = tail(df$pop_growth, 5),
  unemp = tail(df$unemp, 5),
  med_exp = tail(df$med_exp, 5),
  life_exp = tail(df$life_exp, 5),
  edu_higher = tail(df$edu_higher, 5),
  export_val = tail(df$export_val, 5),
  hi_tech = tail(df$hi_tech, 5)
)
predicted_gdp <- predict(model, newdata = future_years)
pred_years <- tail(df$years, 5)
cat("==== Прогноз ВВП по годам:\n")
print(data.frame(Year=pred_years, GDP_Prediction=predicted_gdp))