# ------------------------------
# 0. Загрузка необходимых библиотек
# ------------------------------
library(readxl)
library(dplyr)
library(network)
library(ergm)
library(ggplot2)
library(sna)     # Для расчёта метрик сети

# ------------------------------
# 1. Импорт данных
# ------------------------------
# Укажите путь к вашей папке с данными
ruta <- "/Users/kirillrabdel/Downloads/untitledfolder2"
setwd(ruta)

# Импорт данных о торговых связях (граф)
data_net_2021 <- read_excel("data_net_2021.xlsx") %>%
  select(ReporterISO3, PartnerISO3, `2021`) %>%
  rename(weight = `2021`) %>%
  mutate(weight = as.numeric(weight)) %>%
  na.omit()

# Импорт данных об атрибутах стран
atributos <- read_excel("P_Data_Extract_From_World_Development_Indicators-3.xlsx") %>%
  select(`Country Code`, `GDP (current US$)`, `Population, total`, `landlocked`, `Eurasian_agreement`) %>%
  filter(!is.na(`GDP (current US$)`), !is.na(`Population, total`)) %>%
  # Добавляем логарифмированные переменные для улучшения сходимости модели
  mutate(logGDP = log(`GDP (current US$)`),
         logPopulation = log(`Population, total`))

# ------------------------------
# 2. Создание графа с помощью igraph
# ------------------------------
# Создаем направленный граф с взвешенными ребрами
g_igraph_2021 <- graph_from_data_frame(data_net_2021, directed = TRUE)

# Фильтрация сети для уменьшения плотности (удаляем связи с весом ниже порога)
threshold <- quantile(E(g_igraph_2021)$weight, 0.5)  # Удаляем связи ниже медианы
 
# Рассчитываем общую degree centrality (входящую и исходящую)
degree_centrality <- igraph::degree(g_igraph_2021, mode = "all")
V(g_igraph_2021)$degree <- degree_centrality

# Выделяем узлы с наивысшей степенью (верхний 10% по degree)
top_countries <- V(g_igraph_2021)[degree_centrality >= quantile(degree_centrality, 0.9)]
V(g_igraph_2021)$color <- ifelse(V(g_igraph_2021) %in% top_countries, "red", "pink")

# ------------------------------
# 3. Улучшенная визуализация сети
# ------------------------------
# Улучшенная визуализация графа с ggraph
# Используем igraph для визуализации графа
plot(g_igraph_2021, 
     vertex.size = degree_centrality / max(degree_centrality) * 15,  # Размер узлов по центральности
     vertex.label.cex = 0.7,  # Размер подписей узлов
     vertex.color = ifelse(V(g_igraph_2021) %in% top_countries, "lightblue", "lightblue"),
     edge.arrow.size = 0.3,  # Размер стрелок
     edge.width = E(g_igraph_2021)$weight / max(E(g_igraph_2021)$weight) * 5,  # Ширина ребер по весу
     main = "Взвешенная и направленная торговая сеть после фильтрации")


# ------------------------------
# 4. Анализ метрик сети
# ------------------------------
# Диаметр сети и глобальный коэффициент кластеризации
diameter_value <- igraph::diameter(g_igraph_2021, directed = TRUE)
clustering_coef <- transitivity(g_igraph_2021, type = "global")

cat("Диаметр сети после фильтрации:", diameter_value, "\n")
cat("Глобальный коэффициент кластеризации после фильтрации:", clustering_coef, "\n")

# Расчет центральностей
closeness_centrality <- igraph::closeness(g_igraph_2021, normalized = TRUE)
betweenness_centrality <- igraph::betweenness(g_igraph_2021, normalized = TRUE)
V(g_igraph_2021)$closeness <- closeness_centrality
V(g_igraph_2021)$betweenness <- betweenness_centrality

# ------------------------------
# 5. Построение тепловой карты (матрица смежности)
# ------------------------------
adjacency_matrix <- as.matrix(as_adjacency_matrix(g_igraph_2021, attr = "weight"))
adjacency_df <- as.data.frame(as.table(adjacency_matrix))

ggplot(adjacency_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Тепловая карта торговых потоков", x = "Экспортеры", y = "Импортеры") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------------------------------
# 6. Моделирование ERGM
# ------------------------------
# Преобразование графа в network для ERGM
adjacency_matrix_2021 <- as_adjacency_matrix(g_igraph_2021, attr = "weight", sparse = FALSE)
nw_2021 <- network(adjacency_matrix_2021, directed = TRUE, matrix.type = "adjacency")

# Удаляем узлы, отсутствующие в атрибутах
valid_countries <- intersect(colnames(adjacency_matrix_2021), atributos$`Country Code`)
nw_2021 <- delete.vertices(nw_2021, which(!(network.vertex.names(nw_2021) %in% valid_countries)))
atributos <- atributos %>% filter(`Country Code` %in% valid_countries)

# Добавляем атрибуты к узлам
set.vertex.attribute(nw_2021, 'logGDP', atributos$logGDP)
set.vertex.attribute(nw_2021, 'logPopulation', atributos$logPopulation)
set.vertex.attribute(nw_2021, 'Landlocked', atributos$`landlocked`)
set.vertex.attribute(nw_2021, 'TradeAgreement', atributos$`Eurasian_agreement`)

# Модель ERGM
set.seed(42)
ergm_fit_2021 <- ergm(nw_2021 ~ edges +
                        nodecov("logGDP") +
                        nodecov("logPopulation") +
                        nodematch("TradeAgreement") +
                        nodefactor("Landlocked"))

summary(ergm_fit_2021)
