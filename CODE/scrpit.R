#   datan käsittely ja priorin hämmennsytä

library(tidyverse)
library(runjags)
library(readxl)


data_r <- read_xlsx("DATA/Finnish_M74_data-2022_paivitetty_TPa_21_01_2023.xlsx",
                  sheet = "Data")

data <- data_r %>% 
  select(YSFM = YSFM, TIAM = `TIAM_nmol/g`) %>% 
  filter(!is.na(YSFM) & !is.na(TIAM)) %>% 
  mutate(YSFM = YSFM/100,
         logTIAM = log(TIAM))



data %>%
  ggplot() +
  geom_point(aes(x = logTIAM, y = YSFM), col = "blue")

#   katsotaan logtiamiinin akselia, jolla muutos 0-100 tulee tapahtumaan
data$logTIAM %>% summary
#   -2.9-2.6
#   mitä muuta tiedetään?
#   kuolleisuus pienenee , kun logtiamiinin arvo kasvaa

# luodaan arvojoukku x-akseli logtiamiinin min ja max perusteella
sim_x <- seq(-2.9, 2.6, by = 0.01)

#   luodaan rakenne, jolla "s-käyrän" sovitusta voidaan tarkastella
#   tätä varten tarvitaan expit-funktio
expit <- function(x) 1/(1+exp(-x))
a = 0; b =1 

y = expit(a + b * sim_x )



mod <- "model{
for(i in 1:n){

logit(y[i]) = a + b * x[i] + e

}

a ~ dnorm(0,10^-2)
#   beta tulee olla negatiivinen 
b = -b_r
b_r ~ dlnorm(0,1)

e ~ dnorm(0,5^-2)
}"

x <- seq(-2.9, 2.6, by = 0.1)
res <- run.jags(mod, monitor = c("y", "a", "b", "e"), 
                data = list(x = x,
                n = length(x)))

resdf <- res$mcmc %>% as.matrix %>% as.data.frame()


resdf %>% select(starts_with("y[")) %>% 
  boxplot()





