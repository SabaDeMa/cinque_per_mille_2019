##### LIBRARIES ######
library(tabulizer)
library(tidyverse)
library(ggplot2)
library(fst)
library(xlsx)
library(scales)


##### DOWNLOAD FILE ######
download.file(     url = "https://www.agenziaentrate.gov.it/portale/documents/20143/2616507/5x1000+-+AF+2019+-+Elenco+destinatari+ammessi+al+contributo+-+P01.pdf/300b809e-e22b-0db4-f884-a71fd7380622",
              destfile = file.path("raw_file", "doc_1.pdf"),
                 quiet = FALSE,
                method = "wininet",
                  mode = "wb")


download.file(     url = "https://www.agenziaentrate.gov.it/portale/documents/20143/2616507/5x1000+-+AF+2019+-+Elenco+destinatari+ammessi+al+contributo+-+P02.pdf/c242d7b5-6fa0-f3eb-1cee-667477dc9567",
                   destfile = file.path("raw_file", "doc_2.pdf"),
                   quiet = FALSE,
                   method = "wininet",
                   mode = "wb")


download.file(     url = "https://www.agenziaentrate.gov.it/portale/documents/20143/2616507/5x1000+-+AF+2019+-+Elenco+destinatari+ammessi+al+contributo+-+P03.pdf/e4b473bb-d483-8f17-9af9-b90956960675",
                   destfile = file.path("raw_file", "doc_3.pdf"),
                   quiet = FALSE,
                   method = "wininet",
                   mode = "wb")


download.file(     url = "https://www.agenziaentrate.gov.it/portale/documents/20143/2616507/5x1000+-+AF+2019+-+Elenco+destinatari+ammessi+al+contributo+-+P04.pdf/a061cd0b-fa72-31f7-136d-49cb535aba25",
                   destfile = file.path("raw_file", "doc_4.pdf"),
                   quiet = FALSE,
                   method = "wininet",
                   mode = "wb")

download.file(     url = "https://www.agenziaentrate.gov.it/portale/documents/20143/2616507/5x1000+-+AF+2019+-+Elenco+destinatari+ammessi+al+contributo+-+P05.pdf/8e7593df-3c91-4e4c-398b-d938f76cfde8",
                   destfile = file.path("raw_file", "doc_5.pdf"),
                   quiet = FALSE,
                   method = "wininet",
                   mode = "wb")

##### PARSING ######
doc_1_parsed <- extract_tables( file = file.path("raw_file", "doc_1.pdf"),
                                encoding = "UTF8")
doc_2_parsed <- extract_tables( file = file.path("raw_file", "doc_2.pdf"),
                                encoding = "UTF8")
doc_3_parsed <- extract_tables( file = file.path("raw_file", "doc_3.pdf"),
                                encoding = "UTF8")
doc_4_parsed <- extract_tables( file = file.path("raw_file", "doc_4.pdf"),
                                encoding = "UTF8")
doc_5_parsed <- extract_tables( file = file.path("raw_file", "doc_5.pdf"),
                                encoding = "UTF8")

end <- Sys.time()

doc_1_table <- as.data.frame(do.call(what = rbind, args = doc_1_parsed))
doc_2_table <- as.data.frame(do.call(what = rbind, args = doc_2_parsed))
doc_3_table <- as.data.frame(do.call(what = rbind, args = doc_3_parsed))
doc_4_table <- as.data.frame(do.call(what = rbind, args = doc_4_parsed))
doc_5_table <- as.data.frame(do.call(what = rbind, args = doc_5_parsed))

raw_df <- do.call(what = rbind, args = list(
doc_1_table,
doc_2_table,
doc_3_table,
doc_4_table,
doc_5_table)
)


##### DATA QUALITY ######
col_names <- c("prog", "cf", "desc", "reg", "prov", "comune", "vol", "asd", "rs", "rsan", "comuni", "mibac", "egap", "nu_scelte", "imp", "imp_prop", "imp_tot")
names(raw_df) <- col_names


clean_index <- grepl(pattern = "Prog", x = raw_df$prog)

raw_df1 <- raw_df[!clean_index, ]
row.names(raw_df1) <- NULL

raw_df1$prog <- as.integer(as.character(raw_df1$prog))

raw_df1$nu_scelte <- gsub(pattern = "\\.",
                      replacement = "",
                      x = raw_df1$nu_scelte)
raw_df1$imp <- gsub(pattern = "\\.",
                          replacement = "",
                          x = raw_df1$imp)
raw_df1$imp_prop <- gsub(pattern = "\\.",
                          replacement = "",
                          x = raw_df1$imp_prop)
raw_df1$imp_tot <- gsub(pattern = "\\.",
                          replacement = "",
                          x = raw_df1$imp_tot)

##### NUMBER CONVERSION ######
raw_df1$nu_scelte <- gsub(pattern = ",",
                          replacement = ".",
                          x = raw_df1$nu_scelte)
raw_df1$imp <- gsub(pattern = ",",
                    replacement = ".",
                    x = raw_df1$imp)
raw_df1$imp_prop <- gsub(pattern = ",",
                         replacement = ".",
                         x = raw_df1$imp_prop)
raw_df1$imp_tot <- gsub(pattern = ",",
                        replacement = ".",
                        x = raw_df1$imp_tot)


raw_df1$prog <- as.numeric(as.character(raw_df1$prog))
raw_df1$nu_scelte <- as.numeric(as.character(raw_df1$nu_scelte))
raw_df1$imp <- as.numeric(as.character(raw_df1$imp))
raw_df1$imp_prop <- as.numeric(as.character(raw_df1$imp_prop))
raw_df1$imp_tot <- as.numeric(as.character(raw_df1$imp_tot))



##### EXPORT ######
# Fast Storage
fst::write_fst(x = raw_df1, compress = 0,
               path = file.path("output_file", "ag_entra_5_mill_data.fst"))
# CSV
write.csv2(x = raw_df1,
           file = file.path("output_file", "ag_entra_5_mill_data.csv"),
           quote = TRUE,
           na = "")
# Excel
# xlsx::write.xlsx(x = raw_df1, file = file.path("output_file", "ag_entra_5_mill_data.xlsx"),
#                  col.names = TRUE,
#                  showNA = FALSE)


##### PLOTS ######
raw_df1 %>%
  select(prog, imp_tot) %>%
  mutate(tot_sum = sum(imp_tot),
         perc = (imp_tot / tot_sum),
         cum_sum = cumsum(perc)) %>%
  ggplot(aes(prog, cum_sum)) +
  geom_line(size = 1) +
  scale_x_continuous(name = "Numero di Enti",
                     breaks = scales::breaks_width(10000),
                     minor_breaks = scales::breaks_width(5000)) +
  scale_y_continuous(labels = label_percent(),
                     breaks = scales::breaks_width(0.1),
                     name = "Percentuale Cumulata") +
  ggtitle("Distribuzione degli Importi erogati", subtitle = "in termini percentuali cumulati")



w_df1 %>%
  mutate(tot_sum = sum(imp_tot),
         perc = (imp_tot / tot_sum),
         cum_sum = cumsum(perc)) %>% filter(cum_sum >= 0.8) -> small_entities

small_entities %>% 
  ggplot(aes(imp_tot, reg)) +
  geom_boxplot(outlier.alpha = 0.1,
               outlier.size = 2) +
  scale_x_continuous(name = "Importo Erogato",
                     breaks = scales::breaks_width(500)) +
  scale_y_discrete(name = "Regione") +
  ggtitle("Distribuzione degli Importi per Regione",
          subtitle = "Per il 90% Enti che Percepiscono il 20% dei contributi")

##### UNIVERSIT ######
raw_df1 %>% 
  filter(grepl(pattern = ".*universit.*", ignore.case = TRUE, x = desc)) %>% 
  select(desc, reg, prov, nu_scelte, imp_tot) %>% 
  summarise(sum(imp_tot))

##### UNIVERSIT ######
raw_df1 %>% 
  filter(grepl(pattern = ".*universit.*", ignore.case = TRUE, x = desc)) %>% 
  select(desc, reg, prov, nu_scelte, imp_tot) %>% 
  summarise(sum(imp_tot))


