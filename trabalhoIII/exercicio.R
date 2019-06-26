# Exercicios relacionados a Regressão linear (trabalho III)

# Os dados a seguir são de medidas relacionado a voz de pacientes que
# se encontram em um estado inicial de Parkinson. Este estudo foi desenvolvido
# para verificar a eficácia desse método em monitorar o progresso da doença nesses
# pacientes de forma não invasiva.

# Carregar arquivo
data <- read.csv("parkinsons_updrs.data")

# A tabela contém 22 colunas. Abaixo está a descrição de cada coluna:
# subject# - Identificador do paciente 
# age - idade do paciente
# sex - gênero do paciente '0' - masculino, '1' - feminino 
# test_time - Tempo desde o recrutamento no experimento.
# motor_UPDRS - pontuação do UPDRS motora dado pelo clínico.
# total_UPDRS - pontuação do UPDRS total dado pelo clínico.
# Jitter(%),Jitter(Abs),Jitter:RAP,Jitter:PPQ5,Jitter:DDP - Várias medidas de variação da frequência fundamental
# Shimmer,Shimmer(dB),Shimmer:APQ3,Shimmer:APQ5,Shimmer:APQ11,Shimmer:DDA - Várias medidas de variação da amplitude
# NHR,HNR - Duas medidas da proporção do rúido com o componente tonal na voz
# RPDE - Uma medida de complexidade dinâmica não linear
# DFA - Expoente de escala fractal de sinal
# PPE - Uma medida não linear de variação de frequência fundamental

# Nos exercícios a seguir, considere a colunta "total_UPDRS" como a variável
# resposta e as colunas posteriores ("Jitter(%)" a "PPE") como sendo as 
# variáveis preditoras. 

# 1) (0,5 ponto) Existe alguma variável que esteja fortemente correlacionado com
# a variável resposta "total_UPDRS"?

# 2) (0,5 pontos) Determine os coeficientes do modelo de regressão e escreva 
# a fórmula da regressão linear múltipla utilizando todas as variáveis
# preditoras disponíveis. 


# 3) (1 ponto) Tente encontrar um modelo de regressão linear múltipla que não utilize 
# todas as variáveis preditoras, mas que seja considerado um modelo mais
# ajustado aos dados segundo o AIC.


# 4) (1 ponto) Escolha um dos modelos de regressão gerado no exercício anterior que você 
# julgue ser interessante para este problema e avalie quanto ao seu poder preditivo. 
# Utilize as métricas como o RMSE e MAE, e o histograma dos resíduos. O que você diria
# sobre este modelo de regressão?
