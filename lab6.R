library(ggplot2)
library(dplyr)
library(car)

# === 0. ЗАГРУЗКА И ПОДГОТОВКА ДАННЫХ ===
data <- read.csv("C:/Users/Extrafly/Desktop/athlete_events.csv")
data <- subset(data, !is.na(Weight))  # Убираем строки с NA в весе

# === 1. ДЕСКРИПТИВНЫЙ АНАЛИЗ: ВЕЛОСПОРТ ===
cycling <- subset(data, Sport == "Cycling")

summary(cycling$Weight)

hist(cycling$Weight, 
     main = "Распределение веса (велоспорт)", 
     xlab = "Вес, кг", col = "lightblue", border = "white")

boxplot(cycling$Weight, 
        main = "Boxplot веса (велоспорт)", 
        horizontal = TRUE, col = "orange")

# === 2. ПРОВЕРКА НА НОРМАЛЬНОСТЬ И ДИСПЕРСИЮ ===
shapiro.test(sample(cycling$Weight, min(5000, nrow(cycling))))

countries <- c("ITA", "TUR", "ESP", "ROU", "SLO")
cycling_countries <- subset(cycling, NOC %in% countries)
bartlett.test(Weight ~ NOC, data = cycling_countries)

# === 3. ГИПОТЕЗА О СРЕДНЕМ ВЕСЕ (в выбранных странах) ===
t.test(cycling_countries$Weight, mu = 70)

# === 4. СРАВНЕНИЕ ВЕСА ЖЕНЩИН В ДВУХ ВИДАХ СПОРТА ===
sport1 <- subset(data, Sport == "Cycling" & Sex == "F")
sport2 <- subset(data, Sport == "Rowing" & Sex == "F")

shapiro.test(sample(sport1$Weight, min(5000, nrow(sport1))))
shapiro.test(sample(sport2$Weight, min(5000, nrow(sport2))))

compare_df <- rbind(
  transform(sport1, SportType = "Cycling"),
  transform(sport2, SportType = "Rowing")
)
bartlett.test(Weight ~ SportType, data = compare_df)
t.test(sport1$Weight, sport2$Weight)

# === ДОПОЛНИТЕЛЬНО: ПРОВЕРКА НОРМАЛЬНОСТИ НА СГЕНЕРИРОВАННЫХ ДАННЫХ ===

set.seed(0)
numb <- rnorm(100, mean = 2, sd = 5)
shapiro.test(numb)

set.seed(0)
uniform_vec <- runif(100, min = -10, max = 10)
shapiro.test(uniform_vec)

qqnorm(numb, main = "QQ-график: rnorm(100)")
qqline(numb, col = 2)

qqPlot(numb, main = "car::qqPlot с доверительным интервалом")
