# Porównanie  "CSIV_60b", "IIPC.1_13b", "ISC_31b", "ISC_63b"                
# wybrane jako lepsze itemy: CSIV_60b IPC_13a ISC_31b ISC_63b


## 1

dane %>% janitor::tabyl(CSIV_60)
dane %>% janitor::tabyl(CSIV_60b)

rstatix::levene_test(val ~ group, data = tibble(val = c(as.integer(dane$CSIV_60)[202:609], dane$CSIV_60b[202:609]),
                                                group = factor(rep(1:2, each = 408))))
t.test(as.integer(dane$CSIV_60)[202:609], dane$CSIV_60b[202:609])
wilcox.test(as.integer(dane$CSIV_60)[202:609], dane$CSIV_60b[202:609])
t.test(as.integer(dane$CSIV_60)[1:201], as.integer(dane$CSIV_60)[202:609])

hist(as.integer(dane$CSIV_60) - dane$CSIV_60b)
plot(as.integer(dane$CSIV_60) - dane$CSIV_60b, type = 'l')

normwhn.test::whitenoise.test((as.integer(dane$CSIV_60) - dane$CSIV_60b)[202:609]) # biały szum
cpgram(as.integer(dane$CSIV_60) - dane$CSIV_60b)                                   # homogeniczność wariancji
shapiro.test(as.integer(dane$CSIV_60) - dane$CSIV_60b)                             # normalność
nortest::sf.test(as.integer(dane$CSIV_60) - dane$CSIV_60b)                         # normalność

ggplot(dane %>%
         mutate(CSIV_60 = as.integer(CSIV_60)) %>%
         select(CSIV_60, CSIV_60b) %>%
         pivot_longer(cols = everything(), names_to = "variable", values_to = "value"),
       aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5, position = "identity", bins = 30) +
  scale_fill_manual(values = c("CSIV_60" = "royalblue", "CSIV_60b" = "darkorange")) -> p1

psych::fa(cor(dane %>%
                mutate(CSIV_60 = as.integer(CSIV_60)) %>%
                select(CSIV_60, CSIV_60b),
              use = "complete.obs"), nfactors = 1, rotate = "varimax", fm = "ml")$loadings

shapiro.test(as.integer(dane$CSIV_60))
shapiro.test(dane$CSIV_60b)  

sd(as.integer(dane$CSIV_60),  na.rm = TRUE) / mean(as.integer(dane$CSIV_60),  na.rm = TRUE)
sd(as.integer(dane$CSIV_60b), na.rm = TRUE) / mean(as.integer(dane$CSIV_60b), na.rm = TRUE)

# chyba lepsza b


## 2

dane %>% janitor::tabyl(IIPC.1_13)
dane %>% janitor::tabyl(IIPC.1_13b)

rstatix::levene_test(val ~ group, data = tibble(val = c(as.integer(dane$IIPC.1_13)[202:609], dane$IIPC.1_13b[202:609]),
                                                group = factor(rep(1:2, each = 408))))
t.test(as.integer(dane$IIPC.1_13)[202:609], dane$IIPC.1_13b[202:609])
wilcox.test(as.integer(dane$IIPC.1_13)[202:609], dane$IIPC.1_13b[202:609])
t.test(as.integer(dane$IIPC.1_13)[1:201], as.integer(dane$IIPC.1_13)[202:609])

hist(as.integer(dane$IIPC.1_13) - dane$IIPC.1_13b)
plot(as.integer(dane$IIPC.1_13) - dane$IIPC.1_13b, type = 'l')

normwhn.test::whitenoise.test((as.integer(dane$IIPC.1_13) - dane$IIPC.1_13b)[202:609])  # biały szum
cpgram(as.integer(dane$IIPC.1_13) - dane$IIPC.1_13b)                                    # homogeniczność wariancji
shapiro.test(as.integer(dane$IIPC.1_13) - dane$IIPC.1_13b)                              # normalność
nortest::sf.test(as.integer(dane$IIPC.1_13) - dane$IIPC.1_13b)                          # normalność

ggplot(dane %>%
         mutate(IIPC.1_13 = as.integer(IIPC.1_13)) %>%
         select(IIPC.1_13, IIPC.1_13b) %>%
         pivot_longer(cols = everything(), names_to = "variable", values_to = "value"),
       aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5, position = "identity", bins = 30) +
  scale_fill_manual(values = c("IIPC.1_13" = "royalblue", "IIPC.1_13b" = "darkorange")) -> p2

psych::fa(cor(dane %>%
                mutate(IIPC.1_13 = as.integer(IIPC.1_13)) %>%
                select(IIPC.1_13, IIPC.1_13b),
              use = "complete.obs"), nfactors = 1, rotate = "varimax", fm = "ml")$loadings

shapiro.test(as.integer(dane$IIPC.1_13))
shapiro.test(dane$IIPC.1_13b)  

sd(as.integer(dane$IIPC.1_13),  na.rm = TRUE) / mean(as.integer(dane$IIPC.1_13),  na.rm = TRUE)
sd(as.integer(dane$IIPC.1_13b), na.rm = TRUE) / mean(as.integer(dane$IIPC.1_13b), na.rm = TRUE)

# chyba lepsza a (?)


## 3

dane %>% janitor::tabyl(ISC_31)
dane %>% janitor::tabyl(ISC_31b)

rstatix::levene_test(val ~ group, data = tibble(val = c(as.integer(dane$ISC_31)[202:609], dane$ISC_31b[202:609]),
                                                group = factor(rep(1:2, each = 408))))
t.test(as.integer(dane$ISC_31)[202:609], dane$ISC_31b[202:609])
wilcox.test(as.integer(dane$ISC_31)[202:609], dane$ISC_31b[202:609])
t.test(as.integer(dane$ISC_31)[1:201], as.integer(dane$ISC_31)[202:609])

hist(as.integer(dane$ISC_31) - dane$ISC_31b)
plot(as.integer(dane$ISC_31) - dane$ISC_31b, type = 'l')

normwhn.test::whitenoise.test((as.integer(dane$ISC_31) - dane$ISC_31b)[202:609])  # biały szum
cpgram(as.integer(dane$ISC_31) - dane$ISC_31b)                                    # homogeniczność wariancji
shapiro.test(as.integer(dane$ISC_31) - dane$ISC_31b)                              # normalność
nortest::sf.test(as.integer(dane$ISC_31) - dane$ISC_31b)                          # normalność

ggplot(dane %>%
         mutate(ISC_31 = as.integer(ISC_31)) %>%
         select(ISC_31, ISC_31b) %>%
         pivot_longer(cols = everything(), names_to = "variable", values_to = "value"),
       aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5, position = "identity", bins = 30) +
  scale_fill_manual(values = c("ISC_31" = "royalblue", "ISC_31b" = "darkorange")) -> p3

psych::fa(cor(dane %>%
                mutate(ISC_31 = as.integer(ISC_31)) %>%
                select(ISC_31, ISC_31b),
              use = "complete.obs"), nfactors = 1, rotate = "varimax", fm = "ml")$loadings

shapiro.test(as.integer(dane$ISC_31))
shapiro.test(dane$ISC_31b)  

sd(as.integer(dane$ISC_31),  na.rm = TRUE) / mean(as.integer(dane$ISC_31),  na.rm = TRUE)
sd(as.integer(dane$ISC_31b), na.rm = TRUE) / mean(as.integer(dane$ISC_31b), na.rm = TRUE)

# lepsza b


## 4

dane %>% janitor::tabyl(ISC_63)
dane %>% janitor::tabyl(ISC_63b)

rstatix::levene_test(val ~ group, data = tibble(val = c(as.integer(dane$ISC_63)[202:609], dane$ISC_63b[202:609]),
                                                group = factor(rep(1:2, each = 408))))
t.test(as.integer(dane$ISC_63)[202:609], dane$ISC_63b[202:609])
wilcox.test(as.integer(dane$ISC_63)[202:609], dane$ISC_63b[202:609])
t.test(as.integer(dane$ISC_63)[1:201], as.integer(dane$ISC_63)[202:609])

hist(as.integer(dane$ISC_63) - dane$ISC_63b)
plot(as.integer(dane$ISC_63) - dane$ISC_63b, type = 'l')

normwhn.test::whitenoise.test((as.integer(dane$ISC_63) - dane$ISC_63b)[202:609])  # biały szum
cpgram(as.integer(dane$ISC_63) - dane$ISC_63b)                                    # homogeniczność wariancji
shapiro.test(as.integer(dane$ISC_63) - dane$ISC_63b)                              # normalność
nortest::sf.test(as.integer(dane$ISC_63) - dane$ISC_63b)                          # normalność

ggplot(dane %>%
         mutate(ISC_63 = as.integer(ISC_63)) %>%
         select(ISC_63, ISC_63b) %>%
         pivot_longer(cols = everything(), names_to = "variable", values_to = "value"),
       aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5, position = "identity", bins = 30) +
  scale_fill_manual(values = c("ISC_63" = "royalblue", "ISC_63b" = "darkorange")) -> p4

psych::fa(cor(dane %>%
                mutate(ISC_63 = as.integer(ISC_63)) %>%
                select(ISC_63, ISC_63b),
              use = "complete.obs"), nfactors = 1, rotate = "varimax", fm = "ml")$loadings

shapiro.test(as.integer(dane$ISC_63))
shapiro.test(dane$ISC_63b)  

sd(as.integer(dane$ISC_63),  na.rm = TRUE) / mean(as.integer(dane$ISC_63),  na.rm = TRUE)
sd(as.integer(dane$ISC_63b), na.rm = TRUE) / mean(as.integer(dane$ISC_63b), na.rm = TRUE)

# chyba lepsza b


##


ggpubr::ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)



# zmiany w dane2
dane2$survey_finish_time <- NULL     # technical question
dane2$sex3 <- NULL                   # technical question
dane2$age <- NULL                    # redundant question

colnames(dane2)[c(1:7,206:227)] <- c("id", "gender", "age", "residence", 
                                     "tenants", "tenants_input", "education", "martial_state", "relationship",
                                     "job_activity_study", "job_activity_work", "job_activity_workless", "job_activity_other",
                                     "if_housing_partner", "if_children", "if_pharmacotherapy", "if_psychotherapy_now",
                                     "if_somatic_disease", "if_life_event", "if_psychotherapy", "psychotherapy_time", "if_hospital_treatment",
                                     "if_medication", "change_self-esteem", "change_work", "change_relationships", "id_kod")

dane2 |> dplyr::select(dplyr::contains("aktywnosc_zawodowa_")) |> rowSums()


str(dane2)
