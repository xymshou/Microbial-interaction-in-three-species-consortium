library(flopr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(ggpubr)
library(GauPro)


# Set working directory: ------------------------------------------------------#
# Update working directory below
setwd("C:/Users/miran/OneDrive - University College London/data/20250129_MS_PR_SoPp-Ss/processed")


# parsing function

flopr::cytation_parse(data_csv = "C:/Users/miran/OneDrive - University College London/data/20250129_MS_PR_SoPp-Ss/processed/20250129_MS_So-Pp-Ss.xlsx", 
                      layout_csv = "C:/Users/miran/OneDrive - University College London/data/20250129_MS_PR_SoPp-Ss/metadata/20250129_PR_layout.csv", # save layout as csv
                      timeseries = TRUE)


flopr::process_plate(data_csv = "C:/Users/miran/OneDrive - University College London/data/20250129_MS_PR_SoPp-Ss/processed/20250129_MS_So-Pp-Ss_parsed.csv",
                     blank_well = c("A1","A2","A3","A4"),
                     neg_well = c(),
                     od_name = "OD600",
                     flu_names = c("GFP"),
                     af_model = "loess",
                     to_MEFL = FALSE,
                     flu_gains = 135,
                     conversion_factors_csv = "../film_parsed_cfs.csv")

normed_data <- read.csv("C:/Users/miran/OneDrive - University College London/data/20250129_MS_PR_SoPp-Ss/processed/20250129_MS_So-Pp-Ss_parsed.csv")
normed_data <- normed_data %>%
  mutate(time = time/60/60)


#plot raw OD of whole plate
raw_OD_plate_plot <- ggplot(normed_data)+
  geom_path(aes(time, OD600))+
  scale_x_continuous("time (h)", breaks = c(seq(0, 24, 8)), labels = seq(0, 24, 8)) +
  facet_grid(row~column)
raw_OD_plate_plot

normed_OD_plate_plot <- ggplot(normed_data)+
  geom_path(aes(time, normalised_OD))+
  scale_x_continuous("time (h)", breaks = c(seq(0, 24, 8)), labels = seq(0, 24, 8)) +
  facet_grid(row~column)
normed_OD_plate_plot

ggsave(plot = normed_OD_plate_plot, filename="normed_OD_plate_plot.pdf", width = 200, height = 140, units = "mm")



# Gompertz model function
gompertz_sim <- function(params_fit, time_range){
  mu <- params_fit[1]
  A <- params_fit[2]
  lambda <- params_fit[3]
  
  y <- A*exp(-exp((( mu*exp(1))/A )*(lambda - time_range) +1 ) )
  
  return(y)
}

# Fit Gompertz to data
####No carbon
#So_LB
So_LB <- subset(normed_data, normed_data$strain_1 == "S.oneidensis" & normed_data$strain_2 == "blank" & normed_data$media == "LB" & normed_data$anomalous == "no")
time <- So_LB$time # 'x' data.
AB <- So_LB$normalised_OD # 'y' data.
startvalues <- list(mu = 0.1, A = 0.7, lambda = 1) # Guess initial parameter values
Ss_LBfit <- nls(AB~A*exp(-exp((( mu*exp(1))/A )*(lambda - time) +1 ) ), start=startvalues, control = list(maxiter=500))
So_LBparams_fit <- coef(So_LBfit)

So_LBtime_range <- seq(0, max(So_LB$time),0.1) # Time range to simulate gompertz fit over.
So_LBsimAB <- gompertz_sim(So_LBparams_fit,So_LBtime_range)
So_LBsimAB <- head(So_LBsimAB,-1)
So_LBtime_range  <- head(So_LBtime_range,-1)

#So_LB
So_LB <- subset(normed_data, normed_data$strain_1 == "S.oneidensis" & normed_data$strain_2 == "blank" & normed_data$media == "LB" & normed_data$anomalous == "no")
time <- So_LB$time # 'x' data.
AB <- So_LB$normalised_OD # 'y' data.
startvalues <- list(mu = 0.1, A = 0.7, lambda = 1) # Guess initial parameter values
Ss_LBfit <- nls(AB~A*exp(-exp((( mu*exp(1))/A )*(lambda - time) +1 ) ), start=startvalues, control = list(maxiter=500))
So_LBparams_fit <- coef(So_LBfit)

So_LBtime_range <- seq(0, max(So_LB$time),0.1) # Time range to simulate gompertz fit over.
So_LBsimAB <- gompertz_sim(So_LBparams_fit,So_LBtime_range)
So_LBsimAB <- head(So_LBsimAB,-1)
So_LBtime_range  <- head(So_LBtime_range,-1)

#Pp_LB
So_LB <- subset(normed_data, normed_data$strain_1 == "S.oneidensis" & normed_data$strain_2 == "blank" & normed_data$media == "LB" & normed_data$anomalous == "no")
time <- So_LB$time # 'x' data.
AB <- So_LB$normalised_OD # 'y' data.
startvalues <- list(mu = 0.1, A = 0.7, lambda = 1) # Guess initial parameter values
Ss_LBfit <- nls(AB~A*exp(-exp((( mu*exp(1))/A )*(lambda - time) +1 ) ), start=startvalues, control = list(maxiter=500))
So_LBparams_fit <- coef(So_LBfit)

So_LBtime_range <- seq(0, max(So_LB$time),0.1) # Time range to simulate gompertz fit over.
So_LBsimAB <- gompertz_sim(So_LBparams_fit,So_LBtime_range)
So_LBsimAB <- head(So_LBsimAB,-1)
So_LBtime_range  <- head(So_LBtime_range,-1)



#Get parameter values
summary(So_LBfit)
summary(Ss_LBfit)
summary(Pp_LBfit) 


######## GRAPHS


#Growth curves
#Theme
my_theme <-   
  theme_classic() +
  theme(legend.position = "none") +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.3, size = 8)) +
  theme(axis.text.y = element_text(size = 8)) +
  theme(text = element_text(size=9)) +
  theme(axis.title = element_text(face="bold", size=8))+
  theme(plot.title = element_text(hjust = 0.5))


#Bar plot of growth rates with EA or B12 or EA+B12 no glucose

gompertzfits_data <- read.csv("monocultureinLB_parameter-values.csv")

growth_test_gompertz_mu <- ggplot() +
  geom_bar(data = gompertzfits_data, aes(x = strain, y = mu, fill = condition), position = "dodge", stat = "identity", width = 0.75, size = 0.15, colour = "black") +
  geom_errorbar(data = gompertzfits_data, aes(x = strain, ymin = (mu-mu_SE), ymax = (mu+mu_SE), fill = condition), width = 0.5, size = 0.15, position = position_dodge(0.75), stat = "identity", colour = "black") +
  #geom_point(data = gompertzfits_data, aes(x = strain, y = mu, colour = condition, shape = strain), size = 1, position = position_dodge(0.75), stat = "identity") +
  scale_fill_manual(values=c("#6FAE6E","#FF9F1C","#2AB7CA")) + 
  #scale_shape_manual(values = c(15,16,17)) +
  xlab("Strain") + ylab("Estimated max growth rate /hr") +
  scale_y_continuous(expand = c(0, 0),limits=c(0,0.2),oob = rescale_none) +
  #ggtitle("+- EA B12 (no glucose)") +
  my_theme
growth_test_gompertz_mu



ggsave(plot = growth_test_gompertz_mu, filename="gomp_mu_plot_mono.pdf", width = 75, height = 50, units = "mm")

growth_test_gompertz_A <- ggplot() +
  geom_bar(data = gompertzfits_data, aes(x = strain, y = A, fill = condition), position = "dodge", stat = "identity", width = 0.75, size = 0.15, colour = "black") +
  geom_errorbar(data = gompertzfits_data, aes(x = strain, ymin = (A-A_SE), ymax = (A+A_SE), fill = condition), width = 0.5, size = 0.15, position = position_dodge(0.75), stat = "identity", colour = "black") +
  #geom_point(data = gompertzfits_data, aes(x = strain, y = A, colour = condition, shape = strain), size = 1, position = position_dodge(0.75), stat = "identity") +
  scale_fill_manual(values=c("#6FAE6E","#FF9F1C","#2AB7CA")) + 
  #scale_shape_manual(values = c(15,16,17)) +
  xlab("Strain") + ylab("Estimated carry capacity") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.55)) +
  #ggtitle("+- EA + B12 (no glucose)") +
  my_theme
growth_test_gompertz_A

ggsave(plot = growth_test_gompertz_A, filename="gomp_A_plot_mono.pdf", width = 75, height = 50, units = "mm")


## Seperate and facet
# Plot mean and SE of data, and overlay fitted curve on top, each condition as a seperate plot
So_LBfac <- So_LB %>% 
  select(replicate,time,normalised_OD) %>%
  pivot_wider(names_from = "replicate", values_from = "normalised_OD") %>% 
  rowwise() %>%
  mutate(
    mean = mean(c_across(2:5)),
    sd = sd(c_across(2:5)),
    se = sd/sqrt(4),
    ymin = mean - se,
    ymax = mean + se
  )

Pp_LBfac <- Pp_LB %>% 
  select(replicate,time,normalised_OD) %>%
  pivot_wider(names_from = "replicate", values_from = "normalised_OD") %>% 
  rowwise() %>%
  mutate(
    mean = mean(c_across(2:5)),
    sd = sd(c_across(2:5)),
    se = sd/sqrt(4),
    ymin = mean - se,
    ymax = mean + se
  )

Ss_LBfac <- Ss_LB %>% 
  select(replicate,time,normalised_OD) %>%
  pivot_wider(names_from = "replicate", values_from = "normalised_OD") %>% 
  rowwise() %>%
  mutate(
    mean = mean(c_across(2:5)),
    sd = sd(c_across(2:5)),
    se = sd/sqrt(4),
    ymin = mean - se,
    ymax = mean + se
  )

monoplotcombo <- ggplot() +
  
  geom_ribbon(data = So_LBfac, aes(ymin=ymin, ymax=ymax, x = time), fill = "#6FAE6E", alpha = 0.2) +
  geom_line(data = So_LBfac, aes(y=mean, x = time), colour = "#6FAE6E", size = 0.3, alpha = 0.5) +
  geom_ribbon(data = Pp_LBfac, aes(ymin=ymin, ymax=ymax, x = time), fill = "#FF9F1C", alpha = 0.2) +
  geom_line(data = Pp_LBfac, aes(y=mean, x = time), colour = "#FF9F1C", size = 0.3, alpha = 0.5) +
  geom_ribbon(data = Ss_LBfac, aes(ymin=ymin, ymax=ymax, x = time), fill = "#2AB7CA", alpha = 0.2) +
  geom_line(data = Ss_LBfac, aes(y=mean, x = time), colour = "#2AB7CA", size = 0.3, alpha = 0.5) +
  geom_line(aes(x = So_LBtime_range, y = So_LBsimAB), colour = "#6FAE6E", size = 0.5, linetype = "dashed") +
  geom_line(aes(x = Pp_LBtime_range, y = Pp_LBsimAB), colour = "#FF9F1C", size = 0.5, linetype = "dashed") +
  geom_line(aes(x = Ss_LBtime_range, y = Ss_LBsimAB), colour = "#2AB7CA", size = 0.5, linetype = "dashed") +
  scale_x_continuous(expand = c(0, 0), limits = c(0,16.1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.65), breaks=seq(0,0.65,by=0.2)) +
  my_theme +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())
monoplotcombo

ggsave(plot = monoplotcombo, filename = "monoplotcombo.pdf", width = 50, height = 50, units = "mm")
