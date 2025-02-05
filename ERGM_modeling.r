# Устанавливаем нужные библиотеки
library(readxl)
library(dplyr)
library(igraph)
library(network)
library(ergm)



# 🔹 1. Импорт данных #####################################################################

# Пути к файлам
ruta <- "/Users/kirillrabdel/Downloads/untitledfolder2"
setwd(ruta)

# Торговые связи (граф)
data_net_2021 <- read_excel("data_net_2021.xlsx")

data_net_2021 <- data_net_2021 %>%
  select("ReporterISO3", "PartnerISO3", "2021")

# Атрибуты стран
atributos <- read_excel("P_Data_Extract_From_World_Development_Indicators-3.xlsx")

# Удаление ненужных столбцов
atributos <- atributos[ , !(names(atributos) %in% c("Time", "Time Code", "Country Name"))]

# Преобразование всех числовых столбцов
atributos[ , -c(1,2)] <- lapply(atributos[ , -c(1,2)], as.numeric)

# Проверяем структуру данных
str(atributos)


# 🔹 2. Создание графа #####################################################################

# Преобразуем данные для igraph
to_network_2021 <- data_net_2021[, c("ReporterISO3", "PartnerISO3")]
to_network_2021$ReporterISO3 <- as.character(data_net_2021$ReporterISO3)
to_network_2021$PartnerISO3 <- as.character(data_net_2021$PartnerISO3)

# Создаем граф
g_igraph_2021 <- graph_from_data_frame(data_net_2021, directed = TRUE)

# Получаем матрицу смежности
adjacency_matrix_2021 <- get.adjacency(g_igraph_2021)

# Преобразуем в объект сети network
nw_2021 <- network(adjacency_matrix_2021, directed = TRUE)
class(nw_2021); nw_2021

# Визуализация сети
plot(nw_2021, label = network.vertex.names(nw_2021))

# 🔹 3. Добавление атрибутов стран #######################################################

atributos$"Eurasian_agreement" <- as.numeric(trimws(atributos$"Eurasian_agreement"))
atributos$"landlocked" <- as.numeric(trimws(atributos$"landlocked"))
atributos$"EU_prefferential_agreements_and_membership" <- as.numeric(trimws(atributos$"EU_prefferential_agreements_and_membership"))
atributos$"WTO" <- as.numeric(trimws(atributos$"WTO"))

set.vertex.attribute(nw_2021, 'Agriculture (% of GDP)', as.numeric(atributos$`Agriculture, forestry, and fishing, value added (% of GDP)`))
set.vertex.attribute(nw_2021, 'Industry (% of GDP)', as.numeric(atributos$`Industry (including construction), value added (% of GDP)`))
set.vertex.attribute(nw_2021, 'Foreign direct investment (% of GDP)', as.numeric(atributos$`Foreign direct investment, net inflows (% of GDP)`))
set.vertex.attribute(nw_2021, 'Landlocked', as.numeric(atributos$landlocked))
set.vertex.attribute(nw_2021, 'Eurasian Agreements', as.numeric(atributos$`Eurasian_agreement`))
set.vertex.attribute(nw_2021, 'Total Population', as.numeric(atributos$`Population, total`))
set.vertex.attribute(nw_2021, 'Services (% of GDP)', as.numeric(atributos$`Services, value added (% of GDP)`))
set.vertex.attribute(nw_2021, 'Total Unemployment', as.numeric(atributos$`Unemployment, total (% of total labor force) (modeled ILO estimate)`))
set.vertex.attribute(nw_2021, 'WTO', as.numeric(atributos$`WTO`))

# 🔹 4. ERGM-модель ######################################################################

set.seed(0)

ergm_model_2021 <- formula(nw_2021 ~ edges + 
                             nodemain('Agriculture (% of GDP)') +
                             nodemain('Industry (% of GDP)') +
                             nodemain('Foreign direct investment (% of GDP)') +
                             nodemain('Total Population') +
                             nodemain('Services (% of GDP)') +
                             nodematch('Agriculture (% of GDP)') +
                             nodematch('Industry (% of GDP)') +
                             nodefactor('Landlocked') +
                             nodematch('Total Population') +
                             nodematch('Services (% of GDP)'))
                  

# Запуск модели
summary(ergm_model_2021)

set.seed(42)
ergm_fit_2021 <- ergm(formula = ergm_model_2021)
summary(ergm_fit_2021)

# Вывод коэффициентов
ergm_fit_2021$coefficients

# ANOVA-анализ
anova(ergm_fit_2021)