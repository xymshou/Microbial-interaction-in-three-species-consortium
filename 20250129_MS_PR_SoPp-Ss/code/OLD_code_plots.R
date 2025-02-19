
## ----packages_we_need--------------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)

setwd("C:/Users/miran/OneDrive - University College London/data/20250129_MS_PR_SoPp-Ss/processed")

# data upload
data_csv <- read.csv("20250129_MS_So-Pp-Ss_parsed.csv")

# example plot the whole plate as separate wells
ggplot(data_csv %>%
         filter(strain_1 != ""),
       aes(x=time/(60*60),
           y=OD600)) +
  geom_line() +
  facet_grid(row~col) +
  theme_bw()

# strain ~ media
ggplot(data_csv%>%
         filter(strain_1 != "blank"),
       aes(x=time/(60*60),
           y=OD600,
           colour=replicate,
           group=well)) +
  geom_line() +
  facet_grid(media~name) +
  scale_x_continuous(name = 'time (h)') +
  scale_y_continuous(name = 'OD600') +  # can also use this to do axes transforms such as log
  scale_colour_continuous(name = 'rep') +  # label colour legend 
  theme_bw()

# strain ~ media
ggplot(data_csv%>%
         filter(strain_1 != "blank")%>%
         filter(media == "BG11"),
       aes(x=time/(60*60),
           y=OD600,
           colour=name,
           group=well)) +
  geom_line() +
  facet_grid(~media) +
  scale_x_continuous(name = 'time (h)') +
  scale_y_continuous(name = 'OD600') +  # can also use this to do axes transforms such as log
  #scale_colour_continuous(name = 'rep') +  # label colour legend 
  theme_bw()

# example extracting data

lb_so <- data_csv %>%
  filter(media == "LB") %>%
  filter(name == "So")
lb_so

#--------------------
# louie's old code
  
# Yeast and LAB growth in CDM35
ggplot(data_csv%>%
         filter(strain_1 != "blank") %>%
         filter(media == "CDM35"),
       aes(x=time/(60*60),
           y=OD600,
           colour=name,
           group=well)) +
  geom_line() +
  scale_x_continuous(name = 'time (h)') +
  scale_y_continuous(name = 'OD600') +  # can also use this to do axes transforms such as log
  scale_colour_discrete(name = 'strain') +  # label colour legend 
  ggtitle("CDM35")+
  theme_bw()


# Yeast and LAB growth in spent CDM35
ggplot(data_csv%>%
         filter(strain_1 != "blank") %>%
         filter(media == "spCDM35"),
       aes(x=time/(60*60),
           y=OD600,
           colour=name,
           group=well)) +
  geom_line() +
  scale_x_continuous(name = 'time (h)') +
  scale_y_continuous(name = 'OD600') +  # can also use this to do axes transforms such as log
  scale_colour_discrete(name = 'strain') +  # label colour legend 
  ggtitle("spent CDM35")+
  theme_bw()

# Yeast and LAB growth in CDM35+Q+S
ggplot(data_csv%>%
         filter(strain_1 != "blank") %>%
         filter(media == "CDM35+Q+S"),
       aes(x=time/(60*60),
           y=OD600,
           colour=name,
           group=well)) +
  geom_line() +
  scale_x_continuous(name = 'time (h)') +
  scale_y_continuous(name = 'OD600') +  # can also use this to do axes transforms such as log
  scale_colour_discrete(name = 'strain') +  # label colour legend 
  ggtitle("CDM35 + Q + S")+
  theme_bw()

# LAB growth in CDM35, spent CDM35 and CDM35+Q+S
lab <- ggplot(data_csv%>%
         filter(strain_1 != "blank") %>%
         filter(name != "Yeast") %>%
         filter((media == "CDM35") | (media == "CDM35+Q+S") | (media == "spCDM35")),
       aes(x=time/(60*60),
           y=OD600,
           colour=media,
           group=well)) +
  geom_point(size = 0.3) +
  facet_wrap(~name) +
  scale_x_continuous(name = 'time (h)') +
  scale_y_continuous(name = 'OD600') +  # can also use this to do axes transforms such as log
  scale_colour_discrete(name = 'media') +  # label colour legend 
  theme_bw(base_size = 22) +
  guides(color=guide_legend(override.aes = list(size=2)))+
  theme(legend.position="right", legend.text=element_text(size=rel(0.60)))
lab
ggsave("lab_poster.png", plot=lab, path=plot_path, width=200, height=90, units="mm")


# Yeast growth in CDM35

y <- ggplot(data_csv%>%
         filter(strain_1 != "blank") %>%
         filter(name == "Yeast") %>%
         filter(well != "C7")%>%
         filter((media == "CDM35") | (media == "CDM35+Q+S") | (media == "spCDM35")),
       aes(x=time/(60*60),
           y=OD600,
           colour=media,
           group=well)) +
  geom_point(size = 0.3) +
  scale_x_continuous(name = 'time (h)') +
  scale_y_continuous(name = 'OD600') +  # can also use this to do axes transforms such as log
  scale_colour_discrete(name = 'media') +  # label colour legend 
  theme_bw(base_size = 22) +
  guides(color=guide_legend(override.aes = list(size=2)))+
  theme(legend.position="right", legend.text=element_text(size=rel(0.60)))
y
ggsave("y_poster.png", plot=y, path=plot_path, width=190, height=90, units="mm")
