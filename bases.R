# Import libraries
if(!require(pacman)) install.packages("pacman")
pacman::p_load(readxl, dplyr, ggplot2, GGally) # "corrplot", "hrbrthemes", "ggthemes"

# Configuração do projeto 
## Carregando dados
ecommerce_dataset.path <- "C:/Users/3CON-RJ/Desktop/Yan/Faculdades/TCC/Bases/ECommerceDataset.xlsx"
ecommerce_dataset <- read_xlsx(ecommerce_dataset.path, sheet = 2)
ecommerce_dict <- read_xlsx(ecommerce_dataset.path, sheet = 1)

## Paleta de cores
cor_unica <- "#3D348B"
paleta <- c("#1B1B1E", "#FA9500", "#76877D", "#3D348B", "#B8DBD9")

# Estudo da base
## Analisando as variáveis
unique(ecommerce_dataset$PreferredLoginDevice)
unique(ecommerce_dataset$PreferredPaymentMode) # CC é cripto currency ou credit card?
unique(ecommerce_dataset$PreferedOrderCat)
unique(ecommerce_dataset$MaritalStatus)

## Ajustando a base
ecommerce_dataset <- ecommerce_dataset[-1]
ecommerce_dataset <- ecommerce_dataset %>% 
  mutate(PreferredPaymentMode = (if_else(PreferredPaymentMode == "COD", "Cash on Delivery", PreferredPaymentMode))) %>%
  mutate(PreferredPaymentMode = (if_else(PreferredPaymentMode == "CC", "Credit Card", PreferredPaymentMode))) %>%
  mutate(PreferredPaymentMode = (if_else(PreferredPaymentMode == "UPI", "Unified Payments Interface", PreferredPaymentMode)))
unique(ecommerce_dataset$PreferredPaymentMode)

## Estatísticas básicas
summary(ecommerce_dataset)

## Checando correlação das variáveis
correlacao_graph <- ggpairs(ecommerce_dataset)
correlacao_graph

M <- cor(ecommerce_dataset[-1])
corrplot(M, method = "circle")

## Proporção de churn
N <- nrow(ecommerce_dataset)
total_churn <- sum(ecommerce_dataset$Churn)
total_retido <- N - total_churn
total_retido/N
total_churn/N

## Contagem de NAs no dataset
na_count <- colSums(is.na(ecommerce_dataset))
na_count_ge_zero <- subset(na_count, na_count > 0)
na_count_ge_zero <- filter(na_count, na_count[names(na_count)] > 0)
ggplot(data = data.frame(coluna = names(na_count_ge_zero), count = na_count_ge_zero), 
       aes(x = coluna, y = count)) +
  theme_minimal() + 
  geom_bar(stat = "identity", fill = cor_unica) +
  labs(title = "Contagem de NAs por Coluna", x = "Variáveis que possuem NAs", y = "Contagem de NAs") + 
  ylim(0, 500)
max(na_count_ge_zero)/N # A variável que possui o maior valor de NAs possui apenas 5.4% da base em NAs

# Frequência
## Churn
freq.churn <- ecommerce_dataset %>% 
  group_by(Churn) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n) * 100) %>%
  ungroup()
plot.churn <- ggplot(freq.churn, aes(x = as.factor(Churn), y = freq, fill = as.factor(Churn))) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = paleta) + 
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Percentual") + 
  xlab("Churn")
plot.churn

freq.paymentmode <- ecommerce_dataset %>% 
  group_by(PreferredPaymentMode) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n) * 100) %>%
  ungroup()
