library(tidyverse)

data <- read.csv("italy.csv", header = TRUE)

colnames(data) <- c("Year", "City", "Category", "Gold", "Silver", "Bronze", "Total", 
                    "Place4", "Place5", "Place6", "Place7", "Place8")

# Фильтрация данных (категория 'All') - График 1
all_data <- filter(data, Category == "All")

# Преобразование в длинный формат
selected_columns <- select(all_data, Year, Gold, Silver, Bronze, Place4:Place8)
places_long <- pivot_longer(selected_columns, cols = -Year, names_to = "Place", values_to = "Count")
places_long <- mutate(
  places_long,
  Place = factor(
    Place,
    levels = c("Gold", "Silver", "Bronze", "Place4", "Place5", "Place6", "Place7", "Place8"),
    labels = 1:8
  )
)

# График распределения мест
ggplot(places_long, aes(x = factor(Year), y = Count, fill = Place)) +
  geom_col(position = "stack") +
  scale_fill_viridis_d() +
  labs(
    title = "Распределение мест 1-8 на Олимпиадах",
    x = "Год Олимпиады",
    y = "Количество спортсменов",
    fill = "Место"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Круговая диаграмма золотых медалей - График 2
grouped_gold <- group_by(all_data, Year)
gold_sum <- summarise(grouped_gold, Gold = sum(Gold))

ggplot(gold_sum, aes(x = "", y = Gold, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Доля золотых медалей по Олимпиадам", fill = "Год") +
  theme_void()

# Фильтрация по полу - График 3
gender_data <- filter(data, Category == "Men" | Category == "Women")
grouped_gender <- group_by(gender_data, Year, Category)
prize_trend <- summarise(grouped_gender, Prize = sum(Gold + Silver + Bronze), .groups = "drop")

ggplot(prize_trend, aes(x = Year, y = Prize, color = Category)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Динамика призовых мест (мужчины vs женщины)",
    x = "Год",
    y = "Количество призовых мест",
    color = "Категория"
  ) +
  scale_color_manual(values = c("Men" = "blue", "Women" = "red")) +
  theme_minimal()

# Фильтрация последних 6 Олимпиад - График 4
last_6_olympics <- filter(all_data, Year >= 2004)
selected_medals <- select(last_6_olympics, Year, Gold, Silver, Bronze)
mutated_medals <- mutate(selected_medals, Prize = Gold + Silver + Bronze)
medals_data <- pivot_longer(mutated_medals, cols = c(Gold, Prize), names_to = "Type", values_to = "Count")

ggplot(medals_data, aes(x = Year, y = Count, color = Type)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Динамика золотых и призовых мест (последние 6 Олимпиад)",
    x = "Год",
    y = "Количество",
    color = "Тип"
  ) +
  scale_color_manual(values = c("Gold" = "gold", "Prize" = "darkgreen")) +
  theme_minimal()

# Анализ по полу - График 5
gender_medals <- filter(data, (Category == "Men" | Category == "Women") & Year >= 2004)
grouped_gender2 <- group_by(gender_medals, Year, Category)
gender_summary <- summarise(
  grouped_gender2,
  Gold = sum(Gold),
  Prize = sum(Gold + Silver + Bronze),
  .groups = "drop"
)

ggplot(gender_summary, aes(x = Year, y = Prize, color = Category)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Динамика призовых мест по полу",
    x = "Год",
    y = "Количество",
    color = "Пол"
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()



install.packages("tidyverse")

install.packages(c("dplyr", "ggplot2", "tidyr"))
