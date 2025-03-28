# Лабораторная работа 3(полная версия): Визуализация олимпийских достижений

## Цель работы
Анализ и визуализация данных олимпийских достижений заданной страны (Италия) с использованием различных типов графиков:
- Столбчатых диаграмм,
- Круговых диаграмм,
- Линейных графиков динамики.

## Требования
- R версии 4.0 и выше,
- Установленные пакеты: `tidyverse`, `ggplot2`, `viridis`,
- Данные в формате CSV, соответствующие структуре из источника [olympteka.ru](http://olympteka.ru).

## Установка и запуск
1. Установите необходимые пакеты:
   ```R
   install.packages(c("tidyverse", "viridis"))
   ```

2. Загрузите данные по Италии в файл `italy.csv` со следующими столбцами:
   ```
   Year, City, Category, Gold, Silver, Bronze, Total, Place4, Place5, Place6, Place7, Place8
   ```

3. Запустите R-скрипт целиком или по частям для генерации графиков.

## Описание кода

### Основные этапы обработки данных
1. **Загрузка данных**:
   ```R
   data <- read.csv("italy.csv", header = TRUE)
   ```

2. **Фильтрация данных**:
   - Только категория "All" для общего анализа,
   - Отдельно "Men" и "Women" для гендерного сравнения.

3. **Преобразования**:
   - Переименование столбцов,
   - Преобразование в длинный формат для визуализации мест 1-8,
   - Расчет суммарных показателей для круговых диаграмм.

### Графики

#### 1. Распределение мест 1-8 (столбчатая диаграмма)
- **Описание**: Показывает общее количество спортсменов Италии, занявших места с 1 по 8 на каждой Олимпиаде.
- **Код**: Используется `geom_col()` с накоплением.
- **Особенности**: 
  - Цветовая палитра `viridis`,
  - Подписи осей под углом 45°.

#### 2. Доля золотых медалей (круговая диаграмма)
- **Описание**: Отображает вклад каждой Олимпиады в общее количество золотых медалей.
- **Код**: `coord_polar()` для преобразования столбцов в сектора.

#### 3. Динамика призовых мест (линейные графики)
- **Описание**: Сравнение количества призовых мест (Gold+Silver+Bronze) между мужчинами и женщинами.
- **Особенности**: 
  - Разные цвета для категорий ("Men" — синий, "Women" — красный),
  - Толщина линии 1.2.

#### 4. Динамика золотых и призовых мест за последние 6 Олимпиад
- **Описание**: Сравнение трендов по золотым медалям и общим призовым местам.
- **Цвета**: 
  - "Gold" — золотой,
  - "Prize" — темно-зеленый.

#### 5. Гендерный анализ призовых мест (последние 6 Олимпиад)
- **Описание**: Аккумулированные данные по мужчинам и женщинам на одном графике.
- **Палитра**: `Set1` из `scale_color_brewer()`.

## Соответствие заданию
| Требование                    | Реализация                         |
|-------------------------------|------------------------------------|
| Столбчатая диаграмма мест 1-8 | График 1                           |
| Круговая диаграмма золотых    | График 2                           |
| Тренды по полу (30 лет)       | График 3 (адаптировано под данные) |
| Анализ последних 6 Олимпиад   | Графики 4 и 5                      |

## Ограничения
1. Данные только по Италии (вместо 7 стран),
2. Анализ проводится для общего вида спорта (без разделения на летние/зимние, по причине того что вид спорта летний и зимой не проходит),
3. Период анализа ограничен доступными данными в `italy.csv`.

## Примеры графиков
(Вставить скриншоты графиков при наличии)

## Заключение
Код полностью реализует визуализацию олимпийских достижений Италии с использованием:
- Столбчатых и круговых диаграмм,
- Линейных графиков для анализа тенденций,
- Гендерного сравнения.
