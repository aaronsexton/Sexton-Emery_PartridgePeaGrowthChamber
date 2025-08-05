

# Code for:
# Plant diversity mediates soil microbe influence on floral phenology and production
# Sexton & Emery (2025)




# Regression analyses ----

# Library
library(tidyverse)
library(lme4)
library(sjPlot)
library(ggpubr)

# Response dataset
responses <- read.csv("../Data/PhD/GC/gc_responses.csv")
str(responses)

# Convert to categories to factors
responses$Diversity <- as.factor(responses$Diversity)
responses$Microbe <- as.factor(responses$Microbe)
responses$site <- as.factor(responses$site)
responses$Microbe <- relevel(responses$Microbe, ref = "Sterile")
responses$Diversity <- relevel(responses$Diversity, ref = "Low")

# Production GLMs
flwr_glm <- glmer(sumflowers ~ Diversity + Microbe + Diversity:Microbe + (1|site),
                  data = responses, family = "poisson")
abv_glm <- lmer(Aboveground ~ Diversity + Microbe + Diversity:Microbe + (1|site),
                data = responses)
blw_glm <- lmer(Belowground ~ Diversity + Microbe + Diversity:Microbe + (1|site),
                data = responses)

plot_models(flwr_glm, abv_glm, blw_glm,
            show.values = T)

tab_model(flwr_glm, abv_glm, blw_glm)


# Save tables of pairwise differences
flwr_emm <- emmeans::emmeans(flwr_glm, list(pairwise ~ Diversity + Microbe))
flwr_emm <- as.data.frame(flwr_emm$`pairwise differences of Diversity, Microbe`)
abv_emm <- emmeans::emmeans(abv_glm, list(pairwise ~ Diversity + Microbe))
abv_emm <- as.data.frame(abv_emm$`pairwise differences of Diversity, Microbe`)
blw_emm <- emmeans::emmeans(blw_glm, list(pairwise ~ Diversity + Microbe))
blw_emm <- as.data.frame(blw_emm$`pairwise differences of Diversity, Microbe`)

# Join them
flwr_emm$response <- "Floral Production"
abv_emm$response <- "Aboveground Biomass"
blw_emm$response <- "Belowground Biomass"

flwr_emm <- rename(flwr_emm, Comparison = `1`)
abv_emm <- rename(abv_emm, Comparison = `1`)
blw_emm <- rename(blw_emm, Comparison = `1`)

prod_emm <- full_join(flwr_emm, abv_emm)
prod_emm <- full_join(prod_emm, blw_emm)


# Plotting
a <- plot_model(flwr_glm,
                type = "pred",
                terms = c("Diversity", "Microbe"),
                show.data = F,
                title = "Floral Production",
                dot.size = 5,
                line.size = 2) +
  theme_bw() +
  theme(plot.title   = element_text(hjust = 0.5, face = "bold", size = 24)) +
  theme(axis.text.x  = element_text(size = 14)) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Total Flowers") +
  theme(legend.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  scale_color_manual(values = c("black", "mediumpurple2", "forestgreen")) +
  theme(legend.text  = element_text(size = 32)) +
  theme(legend.title = element_text(size = 38)) +
  theme(legend.key.size = unit(3, 'cm')) + 
  theme(legend.key.height = unit(1.5, 'cm')) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 20))

a
b <- plot_model(abv_glm,
                type = "pred",
                terms = c("Diversity", "Microbe"),
                show.data = F,
                title = "Aboveground Biomass",
                dot.size = 5,
                line.size = 2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Biomass (g)") +
  theme(legend.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  scale_color_manual(values = c("black", "mediumpurple2", "forestgreen")) +
  theme(legend.text = element_text(size = 32)) +
  theme(legend.title = element_text(size = 38)) +
  theme(legend.key.size = unit(3, 'cm')) + 
  theme(legend.key.height = unit(1.5, 'cm')) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 20))

c <- plot_model(blw_glm,
                type = "pred",
                terms = c("Diversity", "Microbe"),
                show.data = F,
                title = "Belowground Biomass",
                dot.size = 5,
                line.size = 2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Biomass (g)") +
  theme(legend.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
  scale_color_manual(values = c("black", "mediumpurple2", "forestgreen")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 20))

ggarrange(a, b, c, 
          ncol = 3,
          common.legend = T,
          legend = "right")



# Phenology GLMs

dur_glm   <- glmer(floweringperiod ~ Diversity + Microbe + Diversity:Microbe + (1|site),
                   data = responses, family = "poisson")
init_glm  <-  glmer(initiation ~ Diversity + Microbe + Diversity:Microbe + (1|site),
                    data = responses, family = "poisson")
girth_glm <-  glmer(d25tod50 ~ Diversity + Microbe + Diversity:Microbe + (1|site),
                    data = responses, family = "poisson")
skew_glm  <-  lmer(Skewness ~ Diversity + Microbe + Diversity:Microbe + (1|site),
                   data = responses)


summary(dur_glm)
tab_model(dur_glm)
plot_model(dur_glm, show.values = T)


plot_models(dur_glm, init_glm, girth_glm, skew_glm, show.values = T)



# Table of model outputs
tab_model(flwr_glm, abv_glm, blw_glm)
tab_model(dur_glm, init_glm, girth_glm, skew_glm)


# Save tables of pairwise differences
flwr_emm <- emmeans::emmeans(flwr_glm, list(pairwise ~ Diversity + Microbe))
flwr_emm <- as.data.frame(flwr_emm$`pairwise differences of Diversity, Microbe`)
abv_emm <- emmeans::emmeans(abv_glm, list(pairwise ~ Diversity + Microbe))
abv_emm <- as.data.frame(abv_emm$`pairwise differences of Diversity, Microbe`)
blw_emm <- emmeans::emmeans(blw_glm, list(pairwise ~ Diversity + Microbe))
blw_emm <- as.data.frame(blw_emm$`pairwise differences of Diversity, Microbe`)

# Join them
flwr_emm$response <- "Floral Production"
abv_emm$response <- "Aboveground Biomass"
blw_emm$response <- "Belowground Biomass"

flwr_emm <- rename(flwr_emm, Comparison = `1`)
abv_emm <- rename(abv_emm, Comparison = `1`)
blw_emm <- rename(blw_emm, Comparison = `1`)

prod_emm <- full_join(flwr_emm, abv_emm)
prod_emm <- full_join(prod_emm, blw_emm)











plot_models(dur_glm, init_glm, d25_glm, d50_glm, girth_glm, skew_glm, kurt_glm,
            show.values = T)

d <- plot_model(dur_glm,
                type = "pred",
                terms = c("Diversity", "Microbe"),
                show.data = F,
                title = "Floral Duration",
                dot.size = 5,
                line.size = 2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 16)) +
  labs(y = "Days from first flower to last") +
  theme(legend.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  scale_color_manual(values = c("black", "mediumpurple2", "forestgreen")) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.key.size = unit(1, 'cm')) +
  theme(axis.title.x = element_text(size = 24),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 20))
d


library(ggeffects)
pred <- ggpredict(dur_glm, terms = c("Diversity", "Microbe"))

# Create interaction label and manual x-positions
pred$combo <- interaction(pred$x, pred$group)
position_map <- c(
  "Low.Sterile"     = 1.0,
  "Low.Bacteria"    = 1.5,
  "Low.Full"        = 2.0,
  "High.Sterile"    = 3.5,
  "High.Bacteria"   = 4.0,
  "High.Full"       = 4.5
)
pred$xpos <- position_map[as.character(pred$combo)]

# Assign axis labels (only at middle points)
axis_breaks <- c(1.0, 1.5, 2.0, 3.5, 4.0, 4.5)
axis_labels <- c(" ", "Low Diversity", " ", " ", "High Diversity", " ")

# Plot
d_2 <- ggplot(pred, aes(x = xpos, y = predicted, color = group)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, size = 1) +
  scale_x_continuous(
    breaks = axis_breaks,
    labels = axis_labels,
    limits = c(0.5, 5)
  ) +
  labs(
    title = "Floral Duration",
    x = NULL,  # remove default x-axis title
    y = "Days (n)"
  ) +
  scale_color_manual(values = c("black", "mediumpurple2", "forestgreen")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20, face = "bold"),
    legend.title = element_text(face = "bold", size = 20, hjust = 0.5),
    legend.text = element_text(size = 18),
    legend.key.size = unit(1, 'cm')
  )






make_plot <- function(pred) {
  position_map <- c(
    "Low.Sterile"     = 1.0,
    "Low.Bacteria"    = 1.5,
    "Low.Full"        = 2.0,
    "High.Sterile"    = 3.5,
    "High.Bacteria"   = 4.0,
    "High.Full"       = 4.5
  )
  pred$combo <- interaction(pred$x, pred$group)
  pred$xpos <- position_map[as.character(pred$combo)]
  
  axis_breaks <- c(1.0, 1.5, 2.0, 3.5, 4.0, 4.5)
  axis_labels <- c(" ", "Low Diversity", " ", " ", "High Diversity", " ")
  
  ggplot(pred, aes(x = xpos, y = predicted, color = group)) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, size = 2.5) +
    scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(0.5, 5)) +
    labs(x = NULL, y = "Days (n)", color = "Microbe") +
    scale_color_manual(values = c("black", "mediumpurple2", "forestgreen")) +
    theme_bw() +
    theme(
      plot.title   = element_text(hjust = 0.5, face = "bold", size = 24),
      axis.text.x  = element_text(size = 20, face = "bold"),
      axis.text.y  = element_text(size = 14),
      axis.title.y = element_text(size = 20, face = "bold"),
      legend.title = element_text(face = "bold", size = 30, hjust = 0.5),
      legend.text  = element_text(size = 24),
      legend.key.size = unit(1, 'cm'),
      legend.margin = margin(t = 10, r = 40, b = 10, l = 40),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

pred_dur <- ggpredict(dur_glm, terms = c("Diversity", "Microbe"))
plot_dur <- make_plot(pred_dur) + ggtitle("Floral Duration")
# plot_dur

pred_init <- ggpredict(init_glm, terms = c("Diversity", "Microbe"))
plot_init <- make_plot(pred_init) + ggtitle("Floral Initiation")
# plot_init

pred_girth <- ggpredict(girth_glm, terms = c("Diversity", "Microbe"))
plot_girth <- make_plot(pred_girth) + ggtitle("Flowering Peak Period")
# plot_girth

pred_skew <- ggpredict(skew_glm, terms = c("Diversity", "Microbe"))
plot_skew <- make_plot(pred_skew) + ggtitle("Skewness") + labs(y = " ")
# plot_skew

pred_prod <- ggpredict(flwr_glm, terms = c("Diversity", "Microbe"))
plot_prod <- make_plot(pred_prod) + ggtitle("Floral Production") + labs(y = "Flowers (n)")
# plot_prod

pred_abv <- ggpredict(abv_glm, terms = c("Diversity", "Microbe"))
plot_abv <- make_plot(pred_abv) + ggtitle("Aboveground Biomass") + labs(y = "(g)")
# plot_abv

pred_blw <- ggpredict(blw_glm, terms = c("Diversity", "Microbe"))
plot_blw <- make_plot(pred_blw) + ggtitle("Belowground Biomass") + labs(y = "(g)")
# plot_blw





# Put them all together

# ggarrange(plot_prod, plot_abv, plot_blw,
#           plot_dur, plot_girth, plot_init, plot_skew,
#           common.legend = T)


# legend
leg <- cowplot::get_legend(plot_prod)
leg2 <- ggpubr::as_ggplot(leg)



top <- ggarrange(plot_prod, plot_abv, plot_blw, leg2,
                 ncol = 4,
                 common.legend = T,
                 legend = "none",
                 labels = c("(a)", "(b)", "(c)", " "),
                 label.x = 0,    # left-align the labels
                 label.y = 1,    # top-align
                 font.label = list(size = 20))
top


# Blank spacer between rows
# blank <- ggplot() + theme_void()


bottom <- ggarrange(plot_dur, plot_girth, plot_init, plot_skew,
                    ncol = 4,
                    common.legend = TRUE, legend = "none",
                    labels = c("(d)", "(e)", "(f)", "(g)"),
                    label.x = 0,    # left-align the labels
                    label.y = 1,    # top-align
                    font.label = list(size = 20))
bottom



both <- ggarrange(top, blank, bottom, 
                  ncol = 1, nrow = 3, 
                  heights = c(1,0.1,1))
both











e <- plot_model(init_glm,
                type = "pred",
                terms = c("Diversity", "Microbe"),
                show.data = F,
                title = "Floral Initiation",
                dot.size = 5,
                line.size = 2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 16)) +
  labs(y = "Days to first flower") +
  theme(legend.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  scale_color_manual(values = c("black", "mediumpurple2", "forestgreen")) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.key.size = unit(1, 'cm')) +
  theme(axis.title.x = element_text(size = 24),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 20))

h <- plot_model(girth_glm,
                type = "pred",
                terms = c("Diversity", "Microbe"),
                show.data = F,
                title = "Days from 25 to 50",
                dot.size = 5,
                line.size = 2)+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 16)) +
  labs(y = "Days between 25% and 50% of total floral production") +
  labs(title = "Flowering Peak Period") +
  theme(legend.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  scale_color_manual(values = c("black", "mediumpurple2", "forestgreen")) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.key.size = unit(1, 'cm')) +
  theme(axis.title.x = element_text(size = 24),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 20))

i <- plot_model(skew_glm,
                type = "pred",
                terms = c("Diversity", "Microbe"),
                show.data = F,
                title = "Skewness",
                dot.size = 5,
                line.size = 2)+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 16)) +
  labs(y = "Skewness") +
  theme(legend.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  scale_color_manual(values = c("black", "mediumpurple2", "forestgreen")) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.key.size = unit(1, 'cm')) +
  theme(axis.title.x = element_text(size = 24),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 20))



ggarrange(a, d, 
          ncol = 2,
          common.legend = T,
          legend = "right")

ggarrange(b, a, d,  h, e, i, 
          ncol = 3, nrow = 2,
          common.legend = T,
          legend = "right")

top <- ggarrange(a, b, c, 
                 ncol = 3,
                 common.legend = T,
                 legend = "right")

bottom <- ggarrange(d, h, e, i,
                    ncol = 4, nrow = 1,
                    common.legend = T,
                    legend = "none")
bottom

ggarrange(top, bottom,
          ncol = 1, nrow = 2)



leg <- cowplot::get_legend(h)
leg2 <- ggpubr::as_ggplot(leg)


top2 <- ggarrange(a, b, c, leg2,
                  ncol = 4,
                  common.legend = T,
                  legend = "none")
top2

ggarrange(top2, bottom,
          ncol = 1, nrow = 2)






# Floral synchrony ----

library(tidyverse)

# Sum the total number of flowering plants each day from each treatment
# e.g. day 35 has 12 flowers from High_Bacteria, and 4 from Low_Bacteria
# Then on days when the plant is flowering compare the number of flowers
# that are present on that day (not including their own flowers)

# Step 1 - convert the flowering df to binary
# Step 2 - sum the number of individuals flowering per day per treatment


# Read in the wide format df
df <- read.csv("../Data/PhD/GC/gc_byfirst.csv")
str(df)

# df <- df %>%
#   mutate(across(3:181, as.numeric))
# df[is.na(df)] <- 0
# 
# df <- rename(df, Treatment = microbediversity)

# Add in Treatments
trtmnts <- read.csv("../Data/PhD/GC/gc_treatment_codes.csv")
str(trtmnts)
trtmnts <- dplyr::select(trtmnts, plantid, microbediversity)
trtmnts <- dplyr::rename(trtmnts, Treatment = microbediversity)
df <- left_join(df, trtmnts)

# Convert to long format
df_long <- df %>%
  pivot_longer(cols = starts_with("d"), names_to = "Day", values_to = "Flowers") %>%
  mutate(Day = as.numeric(gsub("d", "", Day)))  # Convert "d1" to 1, "d2" to 2, etc.

# Count number of plants flowering per treatment per day
df_counts <- df_long %>%
  group_by(Treatment, Day) %>%
  summarize(Total_Flowering = sum(Flowers > 0), .groups = "drop")

# Join with original long data and remove self-count
df_result <- df_long %>%
  left_join(df_counts, by = c("Treatment", "Day")) %>%
  mutate(Other_Flowering = ifelse(Flowers > 0, Total_Flowering - 1, NA)) %>%
  group_by(plantid) %>%
  summarize(Avg_Flowering_Treatment = mean(Other_Flowering, na.rm = TRUE))

# View result
print(df_result)

df_result <- left_join(df_result, trtmnts)

# Visualize data distribution (boxplot)
ggplot(df_result, aes(x = Treatment, y = Avg_Flowering_Treatment)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(title = "Flowering Synchrony by Treatment", y = "Avg Other Flowering Plants")

# Check normality
shapiro.test(df_result$Avg_Flowering_Treatment)  # If p < 0.05, data is NOT normal

# Check variance homogeneity
bartlett.test(Avg_Flowering_Treatment ~ Treatment, data = df_result)  # If p < 0.05, variances are different

# Data normal and homogenous variance

anova_result <- aov(Avg_Flowering_Treatment ~ Treatment, data = df_result)
summary(anova_result)

# Post-hoc test (if ANOVA is significant)
TukeyHSD(anova_result)



# Convert flowers to binary and re-run
df_long <- df_long %>%
  mutate(Flowers = ifelse(Flowers > 0, 1, 0))



# Make the ggplot prettier and match the colors of the other plots
ggplot(df_result, aes(x = Treatment, y = Avg_Flowering_Treatment)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  labs(title = "Flowering Synchrony", y = "Avg. Co-flowering Plants") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16))

str(df_result)

trtmnts2 <- read.csv("../Data/PhD/GC/gc_treatment_codes.csv")
str(trtmnts2)

df_result <- left_join(df_result, trtmnts2)
str(df_result)

ggplot(df_result, aes(x = Treatment, y = Avg_Flowering_Treatment)) +
  geom_boxplot(alpha = 0.4, aes(color = microbe, fill = microbe)) +
  geom_jitter(width = 0.2, alpha = 0.8, aes(color = microbe)) +
  scale_color_manual(values = c("mediumpurple", "forestgreen", "black")) +
  scale_fill_manual(values = c("mediumpurple", "forestgreen", "black")) +
  labs(title = "Flowering Synchrony", y = "Avg. Co-flowering Plants") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        axis.text.x = element_text(size = 16, color = "black"))

df_result$Treatment <- gsub("([a-z])([A-Z])", "\\1 \\2", df_result$Treatment)

ggplot(df_result, aes(x = Treatment, y = Avg_Flowering_Treatment)) +
  geom_boxplot(alpha = 0.4, aes(color = microbe, fill = microbe)) +
  geom_jitter(width = 0.2, alpha = 0.8, aes(color = microbe)) +
  scale_color_manual(values = c("mediumpurple", "forestgreen", "black")) +
  scale_fill_manual(values = c("mediumpurple", "forestgreen", "black")) +
  scale_x_discrete(labels = function(x) gsub("([a-z])([A-Z])", "\\1\n\\2", x)) +
  labs(title = "Flowering Synchrony", y = "Avg. Co-flowering Plants (n)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        axis.text.x = element_text(size = 16, color = "black"))


#












# permanova & permdisp  ----


library(tidyverse)
library(vegan)

# Read in df with floral abundances binned into 10 day increments
flower_byfirst_binned <- read.csv("../Data/PhD/GC/flowers_byfirst_binned10days.csv")
str(flower_byfirst_binned)

# Convert plantid to rownames
rownames(flower_byfirst_binned) <- flower_byfirst_binned$plantid
flower_byfirst_binned <- select(flower_byfirst_binned, -plantid)

# Run a distance matrix
flower_dist <- vegdist(decostand(flower_byfirst_binned, "norm"), "euclidean")
flower_dist
summary(as.matrix(flower_dist))

# NMDS
NMDS <- metaMDS(flower_dist)
ordiplot (NMDS, cex = 1.5, type = 't')
stressplot (NMDS)


# Add in predictors
# Preds
preds <- read.csv("../Data/PhD/GC/gc_treatment_codes.csv")
str(preds)
rownames(preds) <- preds$plantid
preds <- dplyr::select(preds, -plantid, -site, -microbediversity)
str(flower_byfirst_binned)
flower_byfirst_binned$id <- rownames(flower_byfirst_binned)
flowers_id <- dplyr::select(flower_byfirst_binned, id)
preds$id <- rownames(preds)
preds <- left_join(flowers_id, preds)
rownames(preds) <- preds$plantid
preds <- dplyr::select(preds, -id)
preds2 <- preds
preds2$MicrobeDiversity <- paste(preds2$microbe, preds2$diversity, sep = "") 
preds3 <- preds2
preds3$MicrobeDiversity <- gsub("SterileHigh", "Sterile", as.character(preds3$MicrobeDiversity))
preds3$MicrobeDiversity <- gsub("SterileLow", "Sterile", as.character(preds3$MicrobeDiversity))
MD2 <- preds3$MicrobeDiversity


# Conduct and environmental vector fit analysis
ef <- envfit(NMDS, preds2)
ordiplot(NMDS, cex = 1.5, type = "t")
plot(ef)
ef$factors

# PERMANOVA
permanova <- adonis2(flower_dist ~ MD2, permutations = 999)
permanova

# PERMDISP
bd_md <- betadisper(flower_dist, MD2)
bd_md$group.distances
hist(bd_md$group.distances)
plot(bd_md)
anova(bd_md)
boxplot(bd_md)
































