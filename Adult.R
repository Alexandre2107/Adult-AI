# Carregando os Dados do arquivo
database <- read.csv(file = "Adult.CSV")

colnames(database) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")

# 1. Identificação do atributo alvo (saída)
target_attribute <- "income"

# 2. Identificação dos tipos de dados dos atributos de entrada (quantitativo, qualitativo)
str(database)

data_types <- c(
  age = "quantitativo",
  workclass = "qualitativo",
  fnlwgt = "quantitativo",
  education = "qualitativo",
  education_num = "quantitativo",
  marital_status = "qualitativo",
  occupation = "qualitativo",
  relationship = "qualitativo",
  race = "qualitativo",
  sex = "qualitativo",
  capital_gain = "quantitativo",
  capital_loss = "quantitativo",
  hours_per_week = "quantitativo",
  native_country = "qualitativo",
  income = "qualitativo"
)
data_types

# 3. Identificação da escala de dados dos atributos de entrada (nominal, ordinal, intervalar, racional)
data_scales <- c(
  age = "Racional",
  workclass = "Nominal",
  fnlwgt = "Racional",
  education = "Ordinal",
  education_num = "Ordinal",
  marital_status = "Nominal",
  occupation = "Nominal",
  relationship = "Nominal",
  race = "Nominal",
  sex = "Nominal",
  capital_gain = "Racional",
  capital_loss = "Racional",
  hours_per_week = "Racional",
  native_country = "Nominal",
  income = "Nominal"
)
data_scales

# 4. Exploração dos dados através de medidas de localidade
numeric_columns1 <- sapply(database, is.numeric)
summary(database[, numeric_columns1])

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

for (i in 1:ncol(database)) {
  mod_val <- Modes(database[,i])
  print(mod_val)
}

# 5. Exploração dos dados através de medidas de espalhamento
spread_measures <- function(x) {
  c(variance = var(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), range = range(x, na.rm = TRUE))
}

# Identifica quais colunas são numéricas.
numeric_columns2 <- sapply(database, is.numeric)
# Aplica a função spread_measures a cada coluna numérica para calcular a variância, desvio padrão e amplitude para cada uma
sapply(database[, numeric_columns2], spread_measures)

# 6. Exploração dos dados através de medidas de distribuição;
# Funções para calcular assimetria e curtose
calc_skewness <- function(x) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  skewness <- sum(((x - mean_x) / sd_x) ^ 3, na.rm = TRUE) / n
  return(skewness)
}

calc_kurtosis <- function(x) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  kurtosis <- sum(((x - mean_x) / sd_x) ^ 4, na.rm = TRUE) / n - 3
  return(kurtosis)
}

# Supondo que 'database' é o seu conjunto de dados
numeric_columns3 <- sapply(database, is.numeric)

# Plotando boxplots para cada variável numérica
boxplot(database$age, main = "Age Boxplot")
boxplot(database$fnlwgt, main = "Fnlwgt Boxplot")
boxplot(database$education_num, main = "Education_num Boxplot")
boxplot(database$capital_gain, main = "capital_gain Boxplot")
boxplot(database$capital_loss, main = "capital_loss Boxplot")
boxplot(database$hours_per_week, main = "hours_per_week Boxplot")

# Calculando assimetria e curtose
skewness_values <- sapply(database[, numeric_columns3], calc_skewness)
kurtosis_values <- sapply(database[, numeric_columns3], calc_kurtosis)

# Criando um dataframe para plotagem
metrics_df <- data.frame(
  Variable = names(skewness_values),
  Skewness = skewness_values,
  Kurtosis = kurtosis_values
)

# Gráfico de assimetria
barplot(metrics_df$Skewness, names.arg = metrics_df$Variable, 
        main = "Assimetria das Variáveis Numéricas", 
        col = "lightblue", ylim = c(min(metrics_df$Skewness) - 1, max(metrics_df$Skewness) + 1),
        cex.names = 0.7, las = 2)

# Gráfico de curtose
barplot(metrics_df$Kurtosis, names.arg = metrics_df$Variable, 
        main = "Curtose das Variáveis Numéricas", 
        col = "lightgreen", ylim = c(min(metrics_df$Kurtosis) - 1, max(metrics_df$Kurtosis) + 1),
        cex.names = 0.7, las = 2)

# 7. Identificação e separação do conjunto de teste
train_data <- read.csv(file = "Adult.CSV")
test_data <- read.csv(file = "adultTest.csv")

colnames(train_data) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")

colnames(test_data) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")
test_data$income <- gsub(" >50K.", " >50K", test_data$income)
test_data$income <- gsub(" <=50K.", " <=50K", test_data$income)

# Identificando colunas numéricas
numeric_columns <- sapply(train_data, is.numeric)

par(mfrow=c(1, 1))
for (col in names(train_data)[numeric_columns]) {
  boxplot(train_data[[col]], test_data[[col]],
          names = c("Train", "Test"),
          main = paste("Boxplot Comparativo para", col),
          col = c("lightblue", "lightgreen"))
}

jitter_horizontal <- function(x) {
  jitter(x, factor = 0.1)
}

# Criando um loop para gerar gráficos de dispersão comparativos
for (col in names(train_data)[numeric_columns]) {
  plot(jitter_horizontal(rep(1, length(train_data[[col]]))), train_data[[col]], 
       xlim = c(0.5, 2.5), xaxt = "n", 
       main = paste("Gráfico de Dispersão Comparativo para", col), 
       xlab = "Conjunto de Dados", ylab = col, col = "blue")
  
  points(jitter_horizontal(rep(2, length(test_data[[col]]))), test_data[[col]], col = "green")
  
  axis(1, at = 1:2, labels = c("Train", "Test"))
  legend("topright", legend = c("Train", "Test"), col = c("blue", "green"), pch = 1)
}

# 8. Identificação e eliminação de atributos não necessários
#é a copia de education_num 
train_data$education <- remove()
#Não é um dado significante para a base de dados
train_data$fnlwgt <- remove()
#Relationship é um subgrupo de marital_status
train_data$relationship <- remove()

test_data$education <- remove()
test_data$fnlwgt <- remove()
test_data$relationship <- remove()

# 9. Identificação e eliminação de exemplos não necessários

#Achar o único exemplo onde não há nada nele
empty_rows <- !complete.cases(train_data)
#Printar a linha no qual ele se encontra
print(train_data[empty_rows, ])
#Remover a linha N/A
train_data <- na.omit(train_data)

#Checando se há algum exemplo que está como NA na base de teste
empty_rows <- !complete.cases(test_data)
print(test_data[empty_rows, ])


# 10 e 11. Análise e aplicação de técnicas de amostragem de dados
# Checar a distribuição da base de treinamento do atributo alvo
table(train_data$income)
# Calcular proporções
prop.table(table(train_data$income))

#Checar a distribuição da base de teste do atributo alvo
table(test_data$income)
prop.table(table(test_data$income))

# Função para realizar amostragem estratificada
amostra_estratificada <- function(data, target_col) {
  # Separar as classes
  class_min <- subset(data, data[[target_col]] == " >50K")
  class_maj <- subset(data, data[[target_col]] == " <=50K")
  
  # Número de exemplos na classe minoritária
  num_min <- nrow(class_min)
  
  # Amostragem aleatória da classe majoritária
  set.seed(123) # para reprodutibilidade
  class_maj_sampled <- class_maj[sample(nrow(class_maj), num_min, replace = FALSE), ]
  
  # Combinar as classes amostradas
  balanced_data <- rbind(class_min, class_maj_sampled)
  
  # Embaralhar os dados para evitar ordenação por classe
  balanced_data <- balanced_data[sample(nrow(balanced_data)), ]
  
  return(balanced_data)
}

# Aplicar a função nas bases de dados
train_data_balanced <- amostra_estratificada(train_data, "income")
test_data_balanced <- amostra_estratificada(test_data, "income")

# Nova distribuição da base de treinamento
table(train_data_balanced$income)
prop.table(table(train_data_balanced$income))

# Nova distribuição da base de teste
table(test_data_balanced$income)
prop.table(table(test_data_balanced$income))



# 12. Limpeza de dados
# Substituir " ?" por NA
train_data[train_data == " ?"] <- NA
test_data[test_data == " ?"] <- NA

# Imprimir a porcentagem de valores NA em cada coluna para verificar
print(sapply(train_data, function(col) {
  sum(is.na(col)) * 100 / length(col)
}))

print(sapply(test_data, function(col) {
  sum(is.na(col)) * 100 / length(col)
}))

# Função para calcular a moda
Modes <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Substituir NA pela moda em cada coluna do train_data
train_data <- data.frame(lapply(train_data, function(col) {
  if (any(is.na(col))) {
    col[is.na(col)] <- Modes(col[!is.na(col)])
  }
  return(col)
}))

# Substituir NA pela moda em cada coluna do test_data
test_data <- data.frame(lapply(test_data, function(col) {
  if (any(is.na(col))) {
    col[is.na(col)] <- Modes(col[!is.na(col)])
  }
  return(col)
}))

# Verificar duplicatas no train_data
duplicated_train <- duplicated(train_data)
any_duplicated_train <- any(duplicated_train)
print(paste("Existem linhas duplicadas em train_data:", any_duplicated_train))

# Verificar duplicatas no test_data
duplicated_test <- duplicated(test_data)
any_duplicated_test <- any(duplicated_test)
print(paste("Existem linhas duplicadas em test_data:", any_duplicated_test))


# Remover linhas duplicadas do train_data
train_data <- train_data[!duplicated(train_data), ]

# Remover linhas duplicadas do test_data
test_data <- test_data[!duplicated(test_data), ]

# Verificar novamente duplicatas no train_data
duplicated_train <- duplicated(train_data)
any_duplicated_train <- any(duplicated_train)
print(paste("Existem linhas duplicadas em train_data:", any_duplicated_train))

# Verificar novamente duplicatas no test_data
duplicated_test <- duplicated(test_data)
any_duplicated_test <- any(duplicated_test)
print(paste("Existem linhas duplicadas em test_data:", any_duplicated_test))


# Imprimir a porcentagem de valores NA em cada coluna para verificar
print(sapply(train_data, function(col) {
  sum(is.na(col)) * 100 / length(col)
}))

print(sapply(test_data, function(col) {
  sum(is.na(col)) * 100 / length(col)
}))

# Verificar a moda para colunas específicas
print(Modes(train_data$workclass))
print(Modes(train_data$education_num))
print(Modes(train_data$occupation))
print(Modes(train_data$hours_per_week))
print(Modes(train_data$native_country))

print(Modes(test_data$workclass))
print(Modes(test_data$occupation))
print(Modes(test_data$native_country))

table(train_data$workclass)
table(test_data$workclass)

#Criação de grupos para alocar grupos parecidos (WorkClass)
train_data$workclass <- as.character(train_data$workclass)

train_data$workclass[train_data$workclass == " Without-pay" | 
                   train_data$workclass == " Never-worked"] <- " Unemployed"

train_data$workclass[train_data$workclass == " State-gov" |
                  train_data$workclass == " Local-gov"] <- " SL-gov"

train_data$workclass[train_data$workclass == " Self-emp-inc" |
                  train_data$workclass == " Self-emp-not-inc"] <- " Self-employed"

table(train_data$workclass)

test_data$workclass <- as.character(test_data$workclass)

test_data$workclass[test_data$workclass == " Without-pay" | 
                   test_data$workclass == " Never-worked"] <- " Unemployed"

test_data$workclass[test_data$workclass == " State-gov" |
                  test_data$workclass == " Local-gov"] <- " SL-gov"

test_data$workclass[test_data$workclass == " Self-emp-inc" |
                  test_data$workclass == " Self-emp-not-inc"] <- " Self-employed"

table(test_data$workclass)

#Criação de grupos para alocar grupos parecidos (Marital Status)
table(train_data$marital_status)

train_data$marital_status <- as.character(train_data$marital_status)

train_data$marital_status[train_data$marital_status == " Married-AF-spouse" |
                       train_data$marital_status == " Married-civ-spouse" |
                       train_data$marital_status == " Married-spouse-absent"] <- " Married"

train_data$marital_status[train_data$marital_status == " Divorced" |
                       train_data$marital_status == " Separated" |
                       train_data$marital_status == " Widowed"] <- " Not-Married"
table(train_data$marital_status)

table(test_data$marital_status)

test_data$marital_status <- as.character(test_data$marital_status)

test_data$marital_status[test_data$marital_status == " Married-AF-spouse" |
                       test_data$marital_status == " Married-civ-spouse" |
                       test_data$marital_status == " Married-spouse-absent"] <- " Married"

test_data$marital_status[test_data$marital_status == " Divorced" |
                       test_data$marital_status == " Separated" |
                       test_data$marital_status == " Widowed"] <- " Not-Married"
table(test_data$marital_status)

#Criação de grupos para alocar grupos parecidos (Native Country)
train_data$native_country <- as.character(train_data$native_country)

north_america <- c(" Canada", " Cuba", " Dominican-Republic", " El-Salvador", " Guatemala",
                   " Haiti", " Honduras", " Jamaica", " Mexico", " Nicaragua",
                   " Outlying-US(Guam-USVI-etc)", " Puerto-Rico", " Trinadad&Tobago",
                   " United-States")
asia <- c(" Cambodia", " China", " Hong", " India", " Iran", " Japan", " Laos",
          " Philippines", " Taiwan", " Thailand", " Vietnam")
south_america <- c(" Columbia", " Ecuador", " Peru")
europe <- c(" England", " France", " Germany", " Greece", " Holand-Netherlands",
            " Hungary", " Ireland", " Italy", " Poland", " Portugal", " Scotland",
            " Yugoslavia")
other <- c(" South")
train_data$native_country[train_data$native_country %in% north_america] <- " North America"
train_data$native_country[train_data$native_country %in% asia] <- " Asia"
train_data$native_country[train_data$native_country %in% south_america] <- " South America"
train_data$native_country[train_data$native_country %in% europe] <- " Europe"
train_data$native_country[train_data$native_country %in% other] <- " Other"

table(train_data$native_country)

test_data$native_country <- as.character(test_data$native_country)

north_america <- c(" Canada", " Cuba", " Dominican-Republic", " El-Salvador", " Guatemala",
                   " Haiti", " Honduras", " Jamaica", " Mexico", " Nicaragua",
                   " Outlying-US(Guam-USVI-etc)", " Puerto-Rico", " Trinadad&Tobago",
                   " United-States")
asia <- c(" Cambodia", " China", " Hong", " India", " Iran", " Japan", " Laos",
          " Philippines", " Taiwan", " Thailand", " Vietnam")
south_america <- c(" Columbia", " Ecuador", " Peru")
europe <- c(" England", " France", " Germany", " Greece", " Holand-Netherlands",
            " Hungary", " Ireland", " Italy", " Poland", " Portugal", " Scotland",
            " Yugoslavia")
other <- c(" South")
test_data$native_country[test_data$native_country %in% north_america] <- " North America"
test_data$native_country[test_data$native_country %in% asia] <- " Asia"
test_data$native_country[test_data$native_country %in% south_america] <- " South America"
test_data$native_country[test_data$native_country %in% europe] <- " Europe"
test_data$native_country[test_data$native_country %in% other] <- " Other"

table(test_data$native_country)

# 13. Identificação e conversão dos tipos de dados
# Conversão dos valores de 'workclass' e 'marital_status'
table(train_data$workclass)

train_data$workclass <- ifelse(train_data$workclass == " Federal-gov", 0, train_data$workclass)
train_data$workclass <- ifelse(train_data$workclass == " Private", 1, train_data$workclass)
train_data$workclass <- ifelse(train_data$workclass == " Self-employed", 2, train_data$workclass)
train_data$workclass <- ifelse(train_data$workclass == " SL-gov", 3, train_data$workclass)
train_data$workclass <- ifelse(train_data$workclass == " Unemployed", 4, train_data$workclass)

table(test_data$workclass)

test_data$workclass <- ifelse(test_data$workclass == " Federal-gov", 0, test_data$workclass)
test_data$workclass <- ifelse(test_data$workclass == " Private", 1, test_data$workclass)
test_data$workclass <- ifelse(test_data$workclass == " Self-employed", 2, test_data$workclass)
test_data$workclass <- ifelse(test_data$workclass == " SL-gov", 3, test_data$workclass)
test_data$workclass <- ifelse(test_data$workclass == " Unemployed", 4, test_data$workclass)

table(train_data$marital_status)

train_data$marital_status <- ifelse(train_data$marital_status == " Married", 0, train_data$marital_status)
train_data$marital_status <- ifelse(train_data$marital_status == " Not-Married", 1, train_data$marital_status)
train_data$marital_status <- ifelse(train_data$marital_status == " Never-married", 2, train_data$marital_status)

table(test_data$marital_status)

test_data$marital_status <- ifelse(test_data$marital_status == " Married", 0, test_data$marital_status)
test_data$marital_status <- ifelse(test_data$marital_status == " Not-Married", 1, test_data$marital_status)
test_data$marital_status <- ifelse(test_data$marital_status == " Never-married", 2, test_data$marital_status)

# Conversão dos valores 'occupation'
table(train_data$occupation)

train_data$occupation <- ifelse(train_data$occupation == " Adm-clerical", 0, train_data$occupation)
train_data$occupation <- ifelse(train_data$occupation == " Armed-Forces", 1, train_data$occupation)
train_data$occupation <- ifelse(train_data$occupation == " Craft-repair", 2, train_data$occupation)
train_data$occupation <- ifelse(train_data$occupation == " Exec-managerial", 3, train_data$occupation)
train_data$occupation <- ifelse(train_data$occupation == " Farming-fishing", 4, train_data$occupation)
train_data$occupation <- ifelse(train_data$occupation == " Handlers-cleaners", 5, train_data$occupation)
train_data$occupation <- ifelse(train_data$occupation == " Machine-op-inspct", 6, train_data$occupation)
train_data$occupation <- ifelse(train_data$occupation == " Other-service", 7, train_data$occupation)
train_data$occupation <- ifelse(train_data$occupation == " Priv-house-serv", 8, train_data$occupation)
train_data$occupation <- ifelse(train_data$occupation == " Prof-specialty", 9, train_data$occupation)
train_data$occupation <- ifelse(train_data$occupation == " Protective-serv", 10, train_data$occupation)
train_data$occupation <- ifelse(train_data$occupation == " Sales", 11, train_data$occupation)
train_data$occupation <- ifelse(train_data$occupation == " Tech-support", 12, train_data$occupation)
train_data$occupation <- ifelse(train_data$occupation == " Transport-moving", 13, train_data$occupation)

table(test_data$occupation)

test_data$occupation <- ifelse(test_data$occupation == " Adm-clerical", 0, test_data$occupation)
test_data$occupation <- ifelse(test_data$occupation == " Armed-Forces", 1, test_data$occupation)
test_data$occupation <- ifelse(test_data$occupation == " Craft-repair", 2, test_data$occupation)
test_data$occupation <- ifelse(test_data$occupation == " Exec-managerial", 3, test_data$occupation)
test_data$occupation <- ifelse(test_data$occupation == " Farming-fishing", 4, test_data$occupation)
test_data$occupation <- ifelse(test_data$occupation == " Handlers-cleaners", 5, test_data$occupation)
test_data$occupation <- ifelse(test_data$occupation == " Machine-op-inspct", 6, test_data$occupation)
test_data$occupation <- ifelse(test_data$occupation == " Other-service", 7, test_data$occupation)
test_data$occupation <- ifelse(test_data$occupation == " Priv-house-serv", 8, test_data$occupation)
test_data$occupation <- ifelse(test_data$occupation == " Prof-specialty", 9, test_data$occupation)
test_data$occupation <- ifelse(test_data$occupation == " Protective-serv", 10, test_data$occupation)
test_data$occupation <- ifelse(test_data$occupation == " Sales", 11, test_data$occupation)
test_data$occupation <- ifelse(test_data$occupation == " Tech-support", 12, test_data$occupation)
test_data$occupation <- ifelse(test_data$occupation == " Transport-moving", 13, test_data$occupation)

# Conversão dos valores 'race'
table(train_data$race)

train_data$race <- ifelse(train_data$race == " Amer-Indian-Eskimo", 0, train_data$race)
train_data$race <- ifelse(train_data$race == " Asian-Pac-Islander", 1, train_data$race)
train_data$race <- ifelse(train_data$race == " Black", 2, train_data$race)
train_data$race <- ifelse(train_data$race == " Other", 3, train_data$race)
train_data$race <- ifelse(train_data$race == " White", 4, train_data$race)

table(test_data$race)

test_data$race <- ifelse(test_data$race == " Amer-Indian-Eskimo", 0, test_data$race)
test_data$race <- ifelse(test_data$race == " Asian-Pac-Islander", 1, test_data$race)
test_data$race <- ifelse(test_data$race == " Black", 2, test_data$race)
test_data$race <- ifelse(test_data$race == " Other", 3, test_data$race)
test_data$race <- ifelse(test_data$race == " White", 4, test_data$race)

# Conversão dos valores 'sex'
table(train_data$sex)

train_data$sex <- ifelse(train_data$sex == " Male", 0, train_data$sex)
train_data$sex <- ifelse(train_data$sex == " Female", 1, train_data$sex)

table(test_data$sex)

test_data$sex <- ifelse(test_data$sex == " Male", 0, train_data$sex)
test_data$sex <- ifelse(test_data$sex == " Female", 1, train_data$sex)

# Conversão dos valores 'native_country'

table(train_data$native_country)

train_data$native_country <- ifelse(train_data$native_country == " North America", 0, train_data$native_country)
train_data$native_country <- ifelse(train_data$native_country == " Asia", 1, train_data$native_country)
train_data$native_country <- ifelse(train_data$native_country == " South America", 2, train_data$native_country)
train_data$native_country <- ifelse(train_data$native_country == " Europe", 3, train_data$native_country)
train_data$native_country <- ifelse(train_data$native_country == " Other", 4, train_data$native_country)

table(test_data$native_country)

test_data$native_country <- ifelse(test_data$native_country == " North America", 0, test_data$native_country)
test_data$native_country <- ifelse(test_data$native_country == " Asia", 1, test_data$native_country)
test_data$native_country <- ifelse(test_data$native_country == " South America", 2, test_data$native_country)
test_data$native_country <- ifelse(test_data$native_country == " Europe", 3, test_data$native_country)
test_data$native_country <- ifelse(test_data$native_country == " Other", 4, test_data$native_country)

# Conversão dos valores 'income'
table(train_data$income)

train_data$income <- ifelse(train_data$income == " <=50K", 0, train_data$income)
train_data$income <- ifelse(train_data$income == " >50K", 1, train_data$income)

table(test_data$income)

test_data$income <- ifelse(test_data$income == " <=50K", 0, test_data$income)
test_data$income <- ifelse(test_data$income == " >50K", 1, test_data$income)

# 14. Análise e aplicação de alguma técnica para redução de dimensionalidade;

head(train_data_balanced)

# Normalização das variáveis numéricas
numeric_cols <- sapply(train_data_balanced, is.numeric)
train_data_num <- train_data_balanced[, numeric_cols]

# Transformação de variáveis categóricas em variáveis dummy
train_data_cat <- train_data_balanced[, !numeric_cols]

# Usar a função model.matrix para criar variáveis dummy
dummies <- model.matrix(~ . - 1, data = train_data_cat) # -1 para remover o intercepto

# Combinar dados normalizados e variáveis dummy
train_data_processed <- cbind(scale(train_data_num), dummies)

# Aplicar PCA
pca_result <- prcomp(train_data_processed, center = TRUE, scale. = TRUE)

# Resumo dos resultados da PCA
summary(pca_result)


# Gráfico da variância explicada
variance_explained <- summary(pca_result)$importance[2, ]
barplot(variance_explained, main = "Proporção da Variância Explicada", 
        xlab = "Componentes Principais", ylab = "Proporção da Variância")

# Cálculo do número de componentes que explicam 95% da variância
cumulative_variance <- cumsum(variance_explained)
num_components <- which(cumulative_variance >= 0.95)[1]

cat("Numero de componentes principais que explicam 95% da variancia:", num_components)


# Obter as componentes principais
pca_data <- as.data.frame(pca_result$x[, 1:num_components])

# Visualizar os dados transformados
head(pca_data)

# Definindo a função para calcular métricas de avaliação
cross_validation_metrics <- function(train_data, k) {
  n <- nrow(train_data)  # Número total de observações
  folds <- sample(rep(1:k, length.out = n))  # Dividindo os dados em k grupos aleatórios
  
  accuracy_list <- c()
  precision_list <- c()
  recall_list <- c()
  confusion_matrices <- list()
  
  for (i in 1:k) {
    # Separando o fold de validação
    validation_indexes <- which(folds == i)
    validation_set <- train_data[validation_indexes, ]
    
    # Dados de treinamento são todos os outros folds
    training_set <- train_data[-validation_indexes, ]
    
    # Garantindo que o target (última coluna) seja numérico
    target_col <- ncol(training_set)
    training_set[, target_col] <- as.numeric(training_set[, target_col])
    validation_set[, target_col] <- as.numeric(validation_set[, target_col])
    
    # Modelo simples: prever com base na média do target (threshold = 0.5 para classificação)
    mean_prediction <- mean(training_set[, target_col])
    threshold <- 0.5
    predictions <- ifelse(rep(mean_prediction, nrow(validation_set)) >= threshold, 1, 0)
    
    # Valores reais do target
    actual <- validation_set[, target_col]
    
    # Matriz de Confusão
    true_positive <- sum(predictions == 1 & actual == 1)
    true_negative <- sum(predictions == 0 & actual == 0)
    false_positive <- sum(predictions == 1 & actual == 0)
    false_negative <- sum(predictions == 0 & actual == 1)
    
    # Calculando métricas
    accuracy <- (true_positive + true_negative) / length(actual)
    precision <- true_positive / (true_positive + false_positive + 1e-10)  # Adicionando um pequeno valor para evitar divisão por zero
    recall <- true_positive / (true_positive + false_negative + 1e-10)
    
    # Armazenando resultados
    accuracy_list <- c(accuracy_list, accuracy)
    precision_list <- c(precision_list, precision)
    recall_list <- c(recall_list, recall)
    
    # Armazenando a matriz de confusão
    confusion_matrices[[i]] <- matrix(c(true_negative, false_positive, false_negative, true_positive), 
                                      nrow = 2, byrow = TRUE,
                                      dimnames = list('Actual' = c('0', '1'), 'Predicted' = c('0', '1')))
  }
  
  # Retornando as métricas médias e as matrizes de confusão
  results <- list(
    accuracy = mean(accuracy_list),
    precision = mean(precision_list),
    recall = mean(recall_list),
    confusion_matrices = confusion_matrices
  )
  
  return(results)
}

# Exemplo de uso da função com k = 5 folds
resultado_cross_validation <- cross_validation_metrics(train_data, k = 5)

# Exibindo os resultados
print(paste("Acurácia Média:", resultado_cross_validation$accuracy))
print(paste("Precisão Média:", resultado_cross_validation$precision))
print(paste("Recall Médio:", resultado_cross_validation$recall))

# Exibindo as matrizes de confusão de cada fold
for (i in 1:5) {
  print(paste("Matriz de Confusão do Fold", i))
  print(resultado_cross_validation$confusion_matrices[[i]])
}


