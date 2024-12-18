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
            `Usuários` = sum(Usuários, na.rm = T),
            `Novos usuários` = sum(`Novos usuários`, na.rm = T))
 
 

drivers_ano <- mutate(drivers_ano, Fixo = 1) %>% 
  select(1, last_col(), everything())

view(drivers_ano) 




#####################################################################
# Encontrando a taxa de crescimento do drivers de forma manual

# Extensão de rede
extensao_2018 <- drivers_ano$`Extensão de rede`[drivers_ano$Ano == 2018]
extensao_2019 <- drivers_ano$`Extensão de rede`[drivers_ano$Ano == 2019]
extensao_2020 <- drivers_ano$`Extensão de rede`[drivers_ano$Ano == 2020]
extensao_2021 <- drivers_ano$`Extensão de rede`[drivers_ano$Ano == 2021]
extensao_2022 <- drivers_ano$`Extensão de rede`[drivers_ano$Ano == 2022]
extensao_2023 <- drivers_ano$`Extensão de rede`[drivers_ano$Ano == 2023]
extensao_2024 <- drivers_ano$`Extensão de rede`[drivers_ano$Ano == 2024]

taxa_extensao_2018 <- extensao_2018/extensao_2018
taxa_extensao_2019 <- extensao_2019/extensao_2018
taxa_extensao_2020 <- extensao_2020/extensao_2018
taxa_extensao_2021 <- extensao_2021/extensao_2018
taxa_extensao_2022 <- extensao_2022/extensao_2018
taxa_extensao_2023 <- extensao_2023/extensao_2018
taxa_extensao_2024 <- extensao_2024/extensao_2018


# Extensão adicional de rede
extensao_adicional_2018 <- drivers_ano$`Extensão adicional de rede`[drivers_ano$Ano == 2018]
extensao_adicional_2019 <- drivers_ano$`Extensão adicional de rede`[drivers_ano$Ano == 2019]
extensao_adicional_2020 <- drivers_ano$`Extensão adicional de rede`[drivers_ano$Ano == 2020]
extensao_adicional_2021 <- drivers_ano$`Extensão adicional de rede`[drivers_ano$Ano == 2021]
extensao_adicional_2022 <- drivers_ano$`Extensão adicional de rede`[drivers_ano$Ano == 2022]
extensao_adicional_2023 <- drivers_ano$`Extensão adicional de rede`[drivers_ano$Ano == 2023]
extensao_adicional_2024 <- drivers_ano$`Extensão adicional de rede`[drivers_ano$Ano == 2024]

taxa_extensao_adicional_2018 <- extensao_adicional_2018/extensao_adicional_2018
taxa_extensao_adicional_2019 <- extensao_adicional_2019/extensao_adicional_2018
taxa_extensao_adicional_2020 <- extensao_adicional_2020/extensao_adicional_2018
taxa_extensao_adicional_2021 <- extensao_adicional_2021/extensao_adicional_2018
taxa_extensao_adicional_2022 <- extensao_adicional_2022/extensao_adicional_2018
taxa_extensao_adicional_2023 <- extensao_adicional_2023/extensao_adicional_2018
taxa_extensao_adicional_2024 <- extensao_adicional_2024/extensao_adicional_2018


# Novos domicílios
novos_domicilios_2018 <- drivers_ano$`Novos domicílios`[drivers_ano$Ano == 2018]
novos_domicilios_2019 <- drivers_ano$`Novos domicílios`[drivers_ano$Ano == 2019]
novos_domicilios_2020 <- drivers_ano$`Novos domicílios`[drivers_ano$Ano == 2020]
novos_domicilios_2021 <- drivers_ano$`Novos domicílios`[drivers_ano$Ano == 2021]
novos_domicilios_2022 <- drivers_ano$`Novos domicílios`[drivers_ano$Ano == 2022]
novos_domicilios_2023 <- drivers_ano$`Novos domicílios`[drivers_ano$Ano == 2023]
novos_domicilios_2024 <- drivers_ano$`Novos domicílios`[drivers_ano$Ano == 2024]

taxa_novos_domicilios_2018 <- novos_domicilios_2018/novos_domicilios_2018
taxa_novos_domicilios_2019 <- novos_domicilios_2019/novos_domicilios_2018
taxa_novos_domicilios_2020 <- novos_domicilios_2020/novos_domicilios_2018
taxa_novos_domicilios_2021 <- novos_domicilios_2021/novos_domicilios_2018
taxa_novos_domicilios_2022 <- novos_domicilios_2022/novos_domicilios_2018
taxa_novos_domicilios_2023 <- novos_domicilios_2023/novos_domicilios_2018
taxa_novos_domicilios_2024 <- novos_domicilios_2024/novos_domicilios_2018

# Novos usuários comerciais
novos_usuarios_comerciais_2018 <- drivers_ano$`Novos usuários comerciais`[drivers_ano$Ano == 2018]
novos_usuarios_comerciais_2019 <- drivers_ano$`Novos usuários comerciais`[drivers_ano$Ano == 2019]
novos_usuarios_comerciais_2020 <- drivers_ano$`Novos usuários comerciais`[drivers_ano$Ano == 2020]
novos_usuarios_comerciais_2021 <- drivers_ano$`Novos usuários comerciais`[drivers_ano$Ano == 2021]
novos_usuarios_comerciais_2022 <- drivers_ano$`Novos usuários comerciais`[drivers_ano$Ano == 2022]
novos_usuarios_comerciais_2023 <- drivers_ano$`Novos usuários comerciais`[drivers_ano$Ano == 2023]
novos_usuarios_comerciais_2024 <- drivers_ano$`Novos usuários comerciais`[drivers_ano$Ano == 2024]

taxa_novos_comerciais_2018 <- novos_usuarios_comerciais_2018/novos_usuarios_comerciais_2018
taxa_novos_comerciais_2019 <- novos_usuarios_comerciais_2019/novos_usuarios_comerciais_2018
taxa_novos_comerciais_2020 <- novos_usuarios_comerciais_2020/novos_usuarios_comerciais_2018
taxa_novos_comerciais_2021 <- novos_usuarios_comerciais_2021/novos_usuarios_comerciais_2018
taxa_novos_comerciais_2022 <- novos_usuarios_comerciais_2022/novos_usuarios_comerciais_2018
taxa_novos_comerciais_2023 <- novos_usuarios_comerciais_2023/novos_usuarios_comerciais_2018
taxa_novos_comerciais_2024 <- novos_usuarios_comerciais_2024/novos_usuarios_comerciais_2018

# Novos usuários residenciais
novos_usuarios_residenciais_2018 <- drivers_ano$`Novos usuários residenciais`[drivers_ano$Ano == 2018]
novos_usuarios_residenciais_2019 <- drivers_ano$`Novos usuários residenciais`[drivers_ano$Ano == 2019]
novos_usuarios_residenciais_2020 <- drivers_ano$`Novos usuários residenciais`[drivers_ano$Ano == 2020]
novos_usuarios_residenciais_2021 <- drivers_ano$`Novos usuários residenciais`[drivers_ano$Ano == 2021]
novos_usuarios_residenciais_2022 <- drivers_ano$`Novos usuários residenciais`[drivers_ano$Ano == 2022]
novos_usuarios_residenciais_2023 <- drivers_ano$`Novos usuários residenciais`[drivers_ano$Ano == 2023]
novos_usuarios_residenciais_2024 <- drivers_ano$`Novos usuários residenciais`[drivers_ano$Ano == 2024]

taxa_novos_residenciais_2018 <- novos_usuarios_residenciais_2018/novos_usuarios_residenciais_2018
taxa_novos_residenciais_2019 <- novos_usuarios_residenciais_2019/novos_usuarios_residenciais_2018
taxa_novos_residenciais_2020 <- novos_usuarios_residenciais_2020/novos_usuarios_residenciais_2018
taxa_novos_residenciais_2021 <- novos_usuarios_residenciais_2021/novos_usuarios_residenciais_2018
taxa_novos_residenciais_2022 <- novos_usuarios_residenciais_2022/novos_usuarios_residenciais_2018
taxa_novos_residenciais_2023 <- novos_usuarios_residenciais_2023/novos_usuarios_residenciais_2018
taxa_novos_residenciais_2024 <- novos_usuarios_residenciais_2024/novos_usuarios_residenciais_2018

# Usuários
usuarios_2018 <- drivers_ano$`Usuários`[drivers_ano$Ano == 2018]
usuarios_2019 <- drivers_ano$`Usuários`[drivers_ano$Ano == 2019]
usuarios_2020 <- drivers_ano$`Usuários`[drivers_ano$Ano == 2020]
usuarios_2021 <- drivers_ano$`Usuários`[drivers_ano$Ano == 2021]
usuarios_2022 <- drivers_ano$`Usuários`[drivers_ano$Ano == 2022]
usuarios_2023 <- drivers_ano$`Usuários`[drivers_ano$Ano == 2023]
usuarios_2024 <- drivers_ano$`Usuários`[drivers_ano$Ano == 2024]

taxa_usuarios_2018 <- usuarios_2018/usuarios_2018
taxa_usuarios_2019 <- usuarios_2019/usuarios_2018
taxa_usuarios_2020 <- usuarios_2020/usuarios_2018
taxa_usuarios_2021 <- usuarios_2021/usuarios_2018
taxa_usuarios_2022 <- usuarios_2022/usuarios_2018
taxa_usuarios_2023 <- usuarios_2023/usuarios_2018
taxa_usuarios_2024 <- usuarios_2024/usuarios_2018

# Usuários residenciais
usuarios_residenciais_2018 <- drivers_ano$`Usuários residenciais`[drivers_ano$Ano == 2018]
usuarios_residenciais_2019 <- drivers_ano$`Usuários residenciais`[drivers_ano$Ano == 2019]
usuarios_residenciais_2020 <- drivers_ano$`Usuários residenciais`[drivers_ano$Ano == 2020]
usuarios_residenciais_2021 <- drivers_ano$`Usuários residenciais`[drivers_ano$Ano == 2021]
usuarios_residenciais_2022 <- drivers_ano$`Usuários residenciais`[drivers_ano$Ano == 2022]
usuarios_residenciais_2023 <- drivers_ano$`Usuários residenciais`[drivers_ano$Ano == 2023]
usuarios_residenciais_2024 <- drivers_ano$`Usuários residenciais`[drivers_ano$Ano == 2024]

taxa_usuarios_residenciais_2018 <- usuarios_residenciais_2018/usuarios_residenciais_2018
taxa_usuarios_residenciais_2019 <- usuarios_residenciais_2019/usuarios_residenciais_2018
taxa_usuarios_residenciais_2020 <- usuarios_residenciais_2020/usuarios_residenciais_2018
taxa_usuarios_residenciais_2021 <- usuarios_residenciais_2021/usuarios_residenciais_2018
taxa_usuarios_residenciais_2022 <- usuarios_residenciais_2022/usuarios_residenciais_2018
taxa_usuarios_residenciais_2023 <- usuarios_residenciais_2023/usuarios_residenciais_2018
taxa_usuarios_residenciais_2024 <- usuarios_residenciais_2024/usuarios_residenciais_2018

# Novos usuários
novos_usuarios_2018 <- drivers_ano$`Usuários residenciais`[drivers_ano$Ano == 2018]
novos_usuarios_2019 <- drivers_ano$`Usuários residenciais`[drivers_ano$Ano == 2019]
novos_usuarios_2020 <- drivers_ano$`Usuários residenciais`[drivers_ano$Ano == 2020]
novos_usuarios_2021 <- drivers_ano$`Usuários residenciais`[drivers_ano$Ano == 2021]
novos_usuarios_2022 <- drivers_ano$`Usuários residenciais`[drivers_ano$Ano == 2022]
novos_usuarios_2023 <- drivers_ano$`Usuários residenciais`[drivers_ano$Ano == 2023]
novos_usuarios_2024 <- drivers_ano$`Usuários residenciais`[drivers_ano$Ano == 2024]

taxa_novos_usuarios_2018 <- novos_usuarios_2018/novos_usuarios_2018
taxa_novos_usuarios_2019 <- novos_usuarios_2019/novos_usuarios_2018
taxa_novos_usuarios_2020 <- novos_usuarios_2020/novos_usuarios_2018
taxa_novos_usuarios_2021 <- novos_usuarios_2021/novos_usuarios_2018
taxa_novos_usuarios_2022 <- novos_usuarios_2022/novos_usuarios_2018
taxa_novos_usuarios_2023 <- novos_usuarios_2023/novos_usuarios_2018
taxa_novos_usuarios_2024 <- novos_usuarios_2024/novos_usuarios_2018

##### criando uma tabela de forma manual

# criando um data frame
drivers_taxa_crescimento <- data.frame(
  Período = seq(as.Date("2018-01-01"), as.Date("2024-01-01"), by = "year"),
  Fixo = 1,
  `Extensao_de_rede`= c(taxa_extensao_2018, taxa_extensao_2019, taxa_extensao_2020, taxa_extensao_2021, taxa_extensao_2022, taxa_extensao_2023, taxa_extensao_2024),
  `Extensao_adicional_de_rede` = c(taxa_extensao_adicional_2018, taxa_extensao_adicional_2019, taxa_extensao_adicional_2020, taxa_extensao_adicional_2021, taxa_extensao_adicional_2022, taxa_extensao_adicional_2023, taxa_extensao_adicional_2024),
  `Novos_domicilios`= c(taxa_novos_domicilios_2018, taxa_novos_domicilios_2019, taxa_novos_domicilios_2020, taxa_novos_domicilios_2021, taxa_novos_domicilios_2022, taxa_novos_domicilios_2023,taxa_novos_domicilios_2024),
  `Usuarios_residenciais`= c(taxa_usuarios_residenciais_2018, taxa_usuarios_residenciais_2019, taxa_usuarios_residenciais_2020, taxa_usuarios_residenciais_2021, taxa_usuarios_residenciais_2022, taxa_usuarios_residenciais_2023, taxa_usuarios_residenciais_2024),
  `Novos_usuarios_residenciais` = c(taxa_novos_residenciais_2018, taxa_novos_residenciais_2019, taxa_novos_residenciais_2020, taxa_novos_residenciais_2021, taxa_novos_residenciais_2022, taxa_novos_residenciais_2023, taxa_novos_residenciais_2024),
  `Novos_usuarios_comerciais`= c(taxa_novos_comerciais_2018, taxa_novos_comerciais_2019, taxa_novos_comerciais_2020, taxa_novos_comerciais_2021, taxa_novos_comerciais_2022, taxa_novos_comerciais_2023, taxa_novos_comerciais_2024),
  `Usuarios` = c(taxa_Usuarios_2018, taxa_usuarios_2019, taxa_usuarios_2020, taxa_usuarios_2021, taxa_usuarios_2022, taxa_usuarios_2023, taxa_usuarios_2024),
  `Novos_usuarios` = c(taxa_novos_usuarios_2018, taxa_novos_usuarios_2019, taxa_novos_usuarios_2020, taxa_novos_usuarios_2021, taxa_novos_usuarios_2022, taxa_novos_usuarios_2023, taxa_novos_usuarios_2024)
  
) # feito a taxa de crescimento de cada drivers

view(drivers_taxa_crescimento)


# Calcular médias das colunas (ignorando a coluna Período)
medias_taxa_crescimento <- colMeans(drivers_taxa_crescimento[,-1], na.rm = TRUE)

# Transformar em um data frame e renomear a coluna adequadamente
medias_taxa_crescimento <- data.frame(medias_taxa_crescimento)

View(medias_taxa_crescimento)

# media para drivers_ano
medias_drivers_ano <- colMeans(drivers_ano[,-1], na.rm = T)
medias_drivers_ano <- round(medias_drivers_ano, 0)
medias_drivers_ano <- data.frame(medias_drivers_ano)


