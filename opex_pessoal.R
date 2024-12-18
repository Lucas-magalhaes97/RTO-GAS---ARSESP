require(tidyverse)
require(magrittr)
require(data.table)

# Lendo os drivers
drivers <- fread("Drivers.csv", header = TRUE)

# renomeando variável V1 para "período"
drivers <- rename(drivers, Período = V1)
drivers <- as.data.frame(drivers)
str(drivers)
drivers <- t(drivers)
drivers <- as.data.frame(drivers)  # quando faço a transposta ele vira type matriz, porém para manipular os preciso com as funções que eu estou utilizando preciso transformar novamente em data frame

# Depois que eu fiz a transposta, ele não identificou o cabeçalho. Preciso  que entenda o cabeçalho
colnames(drivers) <- drivers[1,]
ncol(drivers)
drivers <- drivers[-1,]

# Deu quase certo, porém a variável "Período" foi excluído. Uso a função rownames_to_column para transformar row names em uma coluna.
drivers <- rownames_to_column(drivers, "Período") # agora deu certo

str(drivers)

# Porém ele está considerando os valores como character, e não numeric. Transformando em numérico

drivers <- drivers %>%
  mutate(across(c(
    `Extensão de rede`,
    `Extensão adicional de rede`,
    `Novos domicílios`,
    `Usuários residenciais`,
    `Novos usuários residenciais`,
    `Novos usuários comerciais`,
    Usuários,
    `Novos usuários`),
    ~as.numeric(gsub("[,.]", "", .))
  ))


#################################################################

# Consolidando os drivers por ano de acordo com cada variável
# 1) preciso transformar o período em data
drivers$Período <- dmy(paste0("01/", drivers$Período))

str(drivers)
any(is.na(drivers))

drivers_ano <- drivers %>% 
  mutate(Ano = year(Período)) %>% 
  group_by(Ano) %>% 
  summarise(`Extensão de rede` = sum(`Extensão de rede`, na.rm = T),
            `Extensão adicional de rede` = sum(`Extensão adicional de rede`, na.rm = T),
            `Novos domicílios` = sum(`Novos domicílios`, na.rm = T),
            `Usuários residenciais` = sum(`Usuários residenciais`, na.rm = T),
            `Novos usuários residenciais` = sum(`Novos usuários residenciais`, na.rm = T),
            `Novos usuários comerciais` = sum(`Novos usuários comerciais`, na.rm = T),
            `Usurários` = sum(Usuários, na.rm = T),
            `Novos usuários` = sum(`Novos usuários`, na.rm = T))
 
 

drivers_ano <- mutate(drivers_ano, Fixo = 1) %>% 
  select(1, last_col(), everything())

view(drivers_ano) 

#####################################################################
# Encontrando a taxa de crescimento do drivers

