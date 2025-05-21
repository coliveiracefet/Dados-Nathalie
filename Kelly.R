install.packages("lme4")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readxl")
install.packages("purrr")

# Carregar pacotes necessários
library(lme4)
library(ggplot2)
library(dplyr)
library(readxl)
library(purrr)

# Carregar os dados
data <- Dados_Kelly

# Renomear colunas para manter consistência com o script original
colnames(data) <- c("Participante", "VLT", "Perfil", "Frase", "Tipo_De_Frase", "Escolha", "RT_Alvo", "Ordem")

# Converter variáveis para os tipos corretos
data$Escolha <- as.factor(data$Escolha)
data$VLT <- as.factor(data$VLT)
data$Participante <- as.factor(data$Participante)
data$Frase <- as.factor(data$Frase)

# Filtrar respostas que não sejam 1 ou 2
data <- data %>% filter(Escolha %in% c(1, 2))
data$VLT <- relevel(data$VLT, ref = "6")


# Modelo GLMM considerando todos os grupos juntos (efeitos aleatórios para participantes e frases)

model_all <- glmer(Escolha ~ 1 + (1 | Participante) + (1 | Frase), 
                   family = binomial(link = "logit"), 
                   data = data)
summary(model_all)

#Modelo GLMM considerando os grupos de frases
#significativo, manter VLT
model_frases <- glmer(Escolha ~ Tipo_De_Frase + (1 | Participante) + (1 | Frase), 
                      family = binomial(link = "logit"), 
                      data = data)
summary(model_frases)
anova (model_frases,model_all)


# Modelo GLMM considerando os grupos de VLT 
#não é significativo, excluir os tipos de frases
model_vlt <- glmer(Escolha ~ VLT + (1 | Participante) + (1 | Frase), 
                   family = binomial(link = "logit"), 
                   data = data)
summary(model_vlt)     

anova (model_vlt,model_all)


model_full <- glmer(Escolha ~ VLT + Tipo_De_Frase + (1 | Participante) + (1 | Frase), 
                    family = binomial(link = "logit"), 
                    data = data)
summary(model_full)     

# Comparação de modelos aninhados para testar a influência de VLT
#significativo
nested_comparison <- anova(model_vlt, model_all, test = "Chisq")
cat("\nComparação de modelos aninhados:\n")
print(nested_comparison)

# Comparação de modelos aninhados para testar a influência de tipo de frases
#naosignificativo
nested_comparison2 <- anova(model_frases, model_all, test = "Chisq")
cat("\nComparação de modelos aninhados:\n")
print(nested_comparison2)


# Análise separada para cada grupo de VLT
#excluí o efeito aleatório de frase
results_vlt_groups <- data %>%
  group_split(VLT) %>%
  map(~ glmer(Escolha ~ 1 + (1 | Participante), 
              family = binomial(link = "logit"), 
              data = .))

# Exibir os sumários dos modelos por grupo de VLT
vlt_levels <- unique(data$VLT)
for (i in seq_along(results_vlt_groups)) {
  cat("\nSumário para o grupo VLT", vlt_levels[i], "\n")
  print(summary(results_vlt_groups[[i]]))
}

# Criar gráfico de desempenho por grupo de VLT
performance <- data %>%
  group_by(VLT, Escolha) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Proportion = Count / sum(Count))

# Gerar gráfico
plot <- ggplot(performance, aes(x = VLT, y = Proportion, fill = Escolha)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Desempenho por Grupo VLT", 
       x = "Grupo VLT", 
       y = "Proporção", 
       fill = "Escolha") +
  theme_minimal()

# Salvar e exibir o gráfico
ggsave("performance_plot.png", plot)
print(plot)

library(dplyr)

# Calcular a proporção de escolhas corretas (Escolha == 1) por grupo VLT
proporcao_acertos <- data %>%
  group_by(VLT) %>%
  summarise(
    Total_Respostas = n(), # Número total de respostas por grupo VLT
    Acertos = sum(Escolha == 1), # Número de escolhas corretas (Escolha == 1)
    Proporcao_Acertos = Acertos / Total_Respostas # Proporção de acertos
  )

# Exibir a tabela
print(proporcao_acertos)

library(dplyr)

# Calcular a proporção de escolhas corretas (Escolha == 1) por Tipo_De_Frase e VLT
proporcao_acertos_grupo <- data %>%
  group_by(Tipo_De_Frase, VLT) %>%
  summarise(
    Total_Respostas = n(), # Número total de respostas por grupo
    Acertos = sum(Escolha == 1), # Número de escolhas corretas (Escolha == 1)
    Proporcao_Acertos = Acertos / Total_Respostas, # Proporção de acertos
    .groups = "drop" # Evita agrupamento adicional
  )

# Exibir a tabela
print(proporcao_acertos_grupo)


#EXTRA

## Proporção de Acertos por Tipo de Frase e Grupo VLT

### Tabela
```{r}
proporcao_acertos_grupo %>%
  kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

ggplot(proporcao_acertos_grupo, aes(x = VLT, y = Proporcao_Acertos, fill = Tipo_De_Frase)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Proporção de Acertos por Tipo de Frase e Grupo VLT",
    x = "Grupo VLT",
    y = "Proporção de Acertos",
    fill = "Tipo de Frase"
  ) +
  theme_minimal()

# Gráfico de proporção de respostas corretas e incorretas por Tipo de Frase e Grupo VLT
ggplot(data, aes(x = Tipo_De_Frase, fill = factor(Escolha))) +
  geom_bar(position = "fill") +
  facet_wrap(~ VLT) +
  scale_fill_manual(values = c("1" = "darkgreen", "2" = "darkred"), 
                    labels = c("1" = "Correto", "2" = "Incorreto")) +
  labs(
    title = "Proporção de Respostas Corretas e Incorretas por Tipo de Frase e Grupo VLT",
    x = "Tipo de Frase",
    y = "Proporção",
    fill = "Escolha"
  ) +
  theme_minimal(base_size = 16) +  # Aumenta o tamanho da fonte
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18, face = "bold"),
    strip.text = element_text(size = 16, face = "bold"), # Títulos das facetas
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  )

# Criar o gráfico com cores ajustadas e ordem invertida
ggplot(data, aes(x = Tipo_De_Frase, fill = factor(Escolha))) +
  geom_bar(position = position_fill(reverse = TRUE)) +  # Reversed for correct stacking
  facet_wrap(
    ~ VLT,
    labeller = labeller(VLT = c("6" = "NS", "5" = "VLT-5", "34" = "VLT-3")),
    nrow = 1  # Put all facets in one row if space allows
  ) +
  scale_x_discrete(labels = c(
    "Condicao_1" = "Non.Temp.Red.IV",
    "Condicao_2" = "Temp.Red.IV",
    "Controle" = "Temp.Red.PV"
  )) +
  scale_fill_manual(
    values = c("1" = "gray20", "2" = "gray80"),
    labels = c("1" = "Correct", "2" = "Incorrect")
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    x = "Items",
    y = "Proportion",
    fill = "Choice"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # Tilted x-axis labels
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 20, face = "bold"),
    strip.text = element_text(size = 18, face = "bold"),
    strip.placement = "outside",  # Optional: move strips outside if needed
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16),
    plot.margin = margin(10, 10, 20, 10)
  )


# Ensure the correct order: "Correto" (green) on the bottom, "Incorreto" (red) on the top
data$Escolha <- factor(data$Escolha, levels = c("2", "1"))  

ggplot(data, aes(x = factor(Frase), fill = Escolha)) +  
  geom_bar(position = position_fill(reverse = TRUE)) +
  facet_wrap(
    ~ VLT,
    labeller = labeller(VLT = c("6" = "NS", "5" = "VLT-5", "34" = "VLT-3")),
    nrow = 1  # Spread facets horizontally for subtitle fit
  ) +
  scale_fill_manual(
    values = c("1" = "gray20", "2" = "gray80"), 
    labels = c("1" = "Correct", "2" = "Incorrect")
  ) +
  labs(
    x = "Items",
    y = "Proportion",
    fill = "Choice"
  ) +
  theme_minimal(base_size = 16) +  
  theme(
    axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 18, face = "bold"),
    strip.text = element_text(size = 16, face = "bold"),
    strip.placement = "outside",  # Optional: place facet titles outside plot area
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    plot.margin = margin(10, 10, 20, 10)  # Adjust margins for breathing space
  )


#Análise de Tempo de Reação

# Garantir que as variáveis estão no formato correto
data$RT_Alvo <- as.numeric(data$RT_Alvo)
data$Escolha <- as.factor(data$Escolha)
data$VLT <- as.factor(data$VLT)
data$Participante <- as.factor(data$Participante)

# Remover valores faltantes em RT_Alvo
data <- data %>% filter(!is.na(RT_Alvo))

# Modelo geral (todos os dados juntos)

# Analisar RT
data$RT_Alvo <- as.numeric(data$RT_Alvo)
data$VLT <- relevel(data$VLT, ref = "6")

# Visualização
ggplot(data, aes(x = factor(VLT, levels = c("6", "3", "5"), 
                            labels = c("NS", "VLT-3", "VLT-5")), 
                 y = RT_Alvo, fill = Escolha)) + 
  geom_boxplot(position = position_dodge(width = 0.75)) + 
  scale_fill_manual(values = c("1" = "gray35", "2" = "gray80"), 
                    labels = c("1" = "Correct", "2" = "Incorrect")) + 
  theme_minimal(base_size = 16) + 
  labs(
    x = "Speakers' Profile",
    y = "Response Time (ms)", 
    fill = "Choice") + 
  theme(
    axis.title = element_text(size = 18),  
    axis.text = element_text(size = 16),   
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 14)  
  )

ggplot(data, aes(x = "Overall", y = RT_Alvo, fill = Escolha)) + 
  geom_boxplot(position = position_dodge(width = 0.75)) + 
  scale_fill_manual(values = c("1" = "gray35", "2" = "gray80"), 
                    labels = c("1" = "Correct", "2" = "Incorrect")) + 
  theme_minimal(base_size = 16) + 
  labs(
    x = NULL,  # Remove o título do eixo X
    y = "Response Time (ms)", 
    fill = "Choice"
  ) + 
  theme(
    axis.text.x = element_blank(),  # Remove os rótulos do eixo X
    axis.ticks.x = element_blank(), # Remove os "risquinhos" do eixo X
    axis.title = element_text(size = 18),  
    axis.text.y = element_text(size = 16),   
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 14)  
  )

#modelo com VLT
modelo_interacao <- lmer(RT_Alvo ~ Escolha * VLT + (1 | Participante), data = data)
summary(modelo_interacao)

modelo_sem_escolha <- lmer(RT_Alvo ~ 1 + (1 | Participante), data = data)
# Modelo nulo
anova(modelo_interacao, modelo_sem_escolha)  # Compara os modelos

#Modelo sem VLT
modelo_interacao2<- lmer(RT_Alvo ~ Escolha + (1 | Participante), data = data)
summary(modelo_interacao2)

modelo_sem_escolha2<- lmer(RT_Alvo ~ 1 + (1 | Participante), data = data)
# Modelo nulo
anova(modelo_interacao2, modelo_sem_escolha2)  # Compara os modelos
