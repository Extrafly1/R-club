# ---------------------------
# Лабораторная работа 5.2: Байесовская классификация и деревья решений
# ---------------------------

# Загрузка необходимых библиотек
library(e1071)        # Наивный Байес
library(party)        # Деревья решений
library(randomForest) # Случайный лес
library(caret)        # Матрица ошибок и точность
library(ggplot2)      # Для дополнительной визуализации (если нужно)

# ---------------------------
# 1. Загрузка данных и подготовка
# ---------------------------

# Загрузка данных
data <- read.csv("clustered_tourism_data.csv")

# Преобразование кластеров в фактор
data$kmeans <- as.factor(data$kmeans)

# Удаление ненужных столбцов
data <- data[, !names(data) %in% c("hclust", "Region")]

# Разделение на обучающую и тестовую выборки
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
trainData <- data[ind == 1, ]
testData <- data[ind == 2, ]

# ---------------------------
# 2. Наивный Байесовский классификатор
# ---------------------------

nb_model <- naiveBayes(kmeans ~ ., data = trainData)
nb_pred <- predict(nb_model, testData)
nb_confusion <- confusionMatrix(nb_pred, testData$kmeans)
cat("Точность (Наивный Байес):", round(nb_confusion$overall['Accuracy'] * 100, 2), "%\n")

# ---------------------------
# 3. Дерево решений
# ---------------------------

dt_model <- ctree(kmeans ~ ., data = trainData)
plot(dt_model, main = "Дерево решений")
dt_pred <- predict(dt_model, testData)
dt_confusion <- confusionMatrix(dt_pred, testData$kmeans)
cat("Точность (Дерево решений):", round(dt_confusion$overall['Accuracy'] * 100, 2), "%\n")

# ---------------------------
# 4. Случайный лес
# ---------------------------

rf_model <- randomForest(kmeans ~ ., data = trainData, ntree = 100)
rf_pred <- predict(rf_model, testData)
rf_confusion <- confusionMatrix(rf_pred, testData$kmeans)
cat("Точность (Случайный лес):", round(rf_confusion$overall['Accuracy'] * 100, 2), "%\n")

# ---------------------------
# 5. Сравнение точности
# ---------------------------

results <- data.frame(
  Метод = c("Наивный Байес", "Дерево решений", "Случайный лес"),
  Точность = c(
    round(nb_confusion$overall['Accuracy'] * 100, 2),
    round(dt_confusion$overall['Accuracy'] * 100, 2),
    round(rf_confusion$overall['Accuracy'] * 100, 2)
  )
)
print(results)

# ---------------------------
# 6. Анализ кластеров
# ---------------------------

# Вычисление средних и стандартных отклонений по числовым переменным
numeric_vars <- sapply(data, is.numeric)
numeric_data <- data[, numeric_vars]

means <- aggregate(numeric_data, by = list(Кластер = data$kmeans), FUN = mean)
sds <- aggregate(numeric_data, by = list(Кластер = data$kmeans), FUN = sd)

cat("\nСредние значения признаков по кластерам:\n")
print(means)

cat("\nСтандартные отклонения признаков по кластерам:\n")
print(sds)

# ---------------------------
# 7. Визуализация ядерной плотности
# ---------------------------

# Выбираем 3 любые числовые переменные, например:
var1 <- names(numeric_data)[1]
var2 <- names(numeric_data)[2]
var3 <- names(numeric_data)[3]

# Графики плотности
opar <- par(no.readonly = TRUE)
layout(matrix(1:4, 2, 2, byrow = TRUE))

plot(density(data[[var1]][data$kmeans == 1]), col = "red", lwd = 2, main = var1, xlab = var1)
lines(density(data[[var1]][data$kmeans == 2]), col = "blue", lwd = 2)
lines(density(data[[var1]][data$kmeans == 3]), col = "green", lwd = 2)
legend("topright", legend = levels(data$kmeans), col = c("red", "blue", "green"), lty = 1)

plot(density(data[[var2]][data$kmeans == 1]), col = "red", lwd = 2, main = var2, xlab = var2)
lines(density(data[[var2]][data$kmeans == 2]), col = "blue", lwd = 2)
lines(density(data[[var2]][data$kmeans == 3]), col = "green", lwd = 2)
legend("topright", legend = levels(data$kmeans), col = c("red", "blue", "green"), lty = 1)

plot(density(data[[var3]][data$kmeans == 1]), col = "red", lwd = 2, main = var3, xlab = var3)
lines(density(data[[var3]][data$kmeans == 2]), col = "blue", lwd = 2)
lines(density(data[[var3]][data$kmeans == 3]), col = "green", lwd = 2)
legend("topright", legend = levels(data$kmeans), col = c("red", "blue", "green"), lty = 1)

par(opar)
