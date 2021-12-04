#  Find Census microdata here
#  https://www.ibge.gov.br/estatisticas/sociais/populacao/9662-censo-demografico-2010.html?edicao=9752&t=microdados
#  download for RJ, extract "Amostra_Pessoas_33.txt"

#  Definition area de ponderacao
# http://200.144.244.241/foruns/index.php/pt/wiki/16-conceitos/32-area-de-ponderacao
# Áreas de ponderação (AP) são unidades geográficas -- definidas apenas para os Censos de 2000 e 2010 -- constituídas 
# do agrupamento mutuamente exclusivo de setores censitários contíguos. São construídas pelo IBGE para que seja possível 
# aplicar dos procedimentos de calibração dos pesos amostrais.
# As áreas de ponderação são a menor unidade geográfica com possibilidade de obter representatividade estatística a 
# partir das amostras dos Censos Demográficos. 
# O número de domicílios e de indivíduos habitando numa área de ponderação, consequentemente, não pode ser muito reduzido,
# sob pena de perda de precisão de suas estimativas. Por esta razão, principalmente nas regiões menos povoadas, as áreas 
# de ponderação acabam ocupando uma larga extensão territorial. Para o Censo de 2010, o IBGE estabeleceu que uma AP 
# deveria ter, no mínimo, 400 domicílios ocupados na amostra. Em geral, Áreas de Ponderação são regiões dentro de 
# municípios -- e, por esta razão, permitem fazer análises intramunicipais. No entanto, quando os próprios municípios não 
# possuem aquele mínimo de domicílios estabelecido, o próprio município é considerado, por inteiro, como uma única área 
# de ponderação.

# Shapefiles area de ponderacao
# https://www.ibge.gov.br/geociencias/downloads-geociencias.html


library(tidyverse)
library(sf) # for reading and processing shapefiles
library(here) # for file references

# https://cran.r-project.org/web/packages/geobr/vignettes/intro_to_geobr.html
library(geobr)


#  ---------------  Try out how shape files work  ------------------------------------------------------
#  Check out how to read in shape files and look at their characteristics
map_rj = st_read("census_rj/33_RJ_Rio de Janeiro/RIO DE JANEIRO_area de ponderacao.shp")
st_geometry_type(map_rj)
st_crs(map_rj) # info about units
map_rj # what does this object look like?
area_pon_names = map_rj$CD_APONDE
area_pon_size = st_area(map_rj) # get area in m2
area_pon_km2 = area_pon_size / 1e6
geo = tibble(area.pon=as.numeric(area_pon_names), km2=as.numeric(area_pon_km2))
#  If you want to make nice maps with these shape files, use the package "ggplot"
#  ---------------  End of "try out" code  ------------------------------------------------------


#  Generate dataset that contains land area in km2 for each area de ponderacao in RJ
geo <- geobr::read_weighting_area(year = "2010", code_weighting = "RJ") %>% 
  tidyr::drop_na() %>% 
  dplyr::mutate(
    area.pon.km2 = sf::st_area(.) / 1e6, 
    km2 = as.numeric(area.pon.km2),
    area.pon = as.numeric(code_weighting)) %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(area.pon, km2)
sum(geo$km2)  # Bernardo says this should be 43969, not exact but close enough


#  ---------------  Read in census data  ------------------------------------------------------
# cens = readRDS(here::here("census_rj", "census_rj_pessoas.Rds")) # full census microdata, all variables

# keep.var = c("V0002", "V0011", "V0010", "V0641", "V0648", "V0601", "V6036", "V6525")
# name.var = c("municip", "area.pon", "peso.amostra", "work.active", "work.type", "gender", "age", "total.monthly")
# cens = cens[,keep.var]
# names(cens) = name.var

# cens = cens %>% dplyr::mutate(peso.amostra=peso.amostra/1e13)

# saveRDS(cens, "census_rj/census_rj_pessoas_ps2.Rds")
cens = readRDS("census_rj/census_rj_pessoas_ps2.Rds")

sprintf("%0.0f", head(cens$area.pon))
sprintf("%0.0f", tail(cens$area.pon))


#  Calculate the population per area de ponderacao using the person weights
#  Notice that we need the full population to find the full population, so dont drop "nonworking" etc. yet
pop = cens %>% 
  group_by(area.pon) %>%
  summarise(pop=sum(peso.amostra)) %>%
  ungroup 
sum(pop$pop) # how many people in the state of RJ in 2010? plausible?


#  Keep only people who are actively working in the week of Jul 31, 2010; younger than 75, earning more than zero R$
cens = cens %>% 
  dplyr::filter(work.active==1, age<=75, total.monthly>0) 
summary(cens)
dim(cens)


#  Merge data
data = left_join(cens, pop, by="area.pon") # merge in population per area de ponderacao
data = left_join(data, geo, by="area.pon") # merge in km2 per area de ponderacao
data = data %>% dplyr::mutate(density = pop/km2)
summary(data)
#  538 areas de ponderacao in the census data but only 330 in the shapefiles
#  Thanks Rafael

data = data %>% dplyr::filter(!is.na(km2)) # keep only observations with non-NA information on km2 and density


#  Use person weights in peso amostra to correct oversampling/undersampling; take 10 percent random sample
n = dim(data)[1]
set.seed(536547) # fix seed value for random sampling, to make sure everyone gets the same results
xs = sample(1:n, floor(n/10), prob=data$peso.amostra, replace=TRUE)
data = data[xs,] # dataset ready to estimate


