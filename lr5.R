# ---------------------------
# 1. Импорт данных и предобработка
# ---------------------------

library(ggplot2)
library(cluster)
library(factoextra)
library(scatterplot3d)

data <- read.csv2("C:/Users/Extrafly/Desktop/туристские поездки_0.csv", sep = ";", dec = ",")

str(data)
summary(data)

data <- data[-1]

scaled_data <- scale(data)

# ---------------------------
# 2. Определение оптимального числа кластеров
# ---------------------------

# Метод локтя
set.seed(123)
wss <- sapply(1:10, function(k) {
  kmeans(scaled_data, k, nstart = 50)$tot.withinss
})

fviz_nbclust(scaled_data, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(title = "Метод локтя")

# Метод силуэта
fviz_nbclust(scaled_data, kmeans, method = "silhouette") +
  labs(title = "Метод силуэта")

# Статистика разрыва
gap_stat <- clusGap(scaled_data, FUN = kmeans, nstart = 50, K.max = 10, B = 50)
fviz_gap_stat(gap_stat) + 
  labs(title = "Статистика разрыва")

# ---------------------------
# 3. Иерархическая кластеризация
# ---------------------------

# Построение дендрограммы
dist_matrix <- dist(scaled_data)
hc <- hclust(dist_matrix, method = "ward.D2")
plot(hc, main = "Дендрограмма")
rect.hclust(hc, k = 3, border = 2:4)

# Назначение кластеров
data$hclust <- cutree(hc, k = 3)

# ---------------------------
# 4. K-means кластеризация
# ---------------------------

set.seed(123)
km <- kmeans(scaled_data, centers = 3, nstart = 50)
data$kmeans <- km$cluster

# ---------------------------
# 5. Визуализация результатов
# ---------------------------

# Кластерный график с эллипсами
fviz_cluster(km, 
             data = data[, -which(names(data) %in% c("hclust", "kmeans"))],  # Исходные данные без кластерных меток
             ellipse.type = "norm", 
             palette = "Set2",
             ggtheme = theme_minimal(),
             main = "Визуализация кластеров (PCA)") +
  theme(legend.position = "top")

# Столбчатые диаграммы (Рис. 5.8)
ggplot(data, aes(x = factor(kmeans), y = USD, fill = factor(kmeans))) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Средние расходы по кластерам", x = "Кластер")

# Боксплоты (Рис. 5.12)
ggplot(data, aes(x = factor(kmeans), y = Days, fill = factor(kmeans))) +
  geom_boxplot() +
  labs(title = "Распределение дней по кластерам", x = "Кластер")

# Scatterplot матрица (Рис. 5.13)
pairs(data[,1:5], 
      col = data$kmeans,
      main = "Матрица scatterplot",
      pch = 19)

# 3D визуализация (Рис. 5.15)
scatterplot3d(data[, c("Days", "USD", "ExtrCharges")],
              color = data$kmeans,
              pch = 19,
              main = "3D кластеризация",
              xlab = "Дни",
              ylab = "USD",
              zlab = "Доп. расходы")
legend("topright", 
       legend = paste("Кластер", 1:3),
       col = 1:3, 
       pch = 19)

# write.csv(data, "clustered_tourism_data.csv", row.names = FALSE)