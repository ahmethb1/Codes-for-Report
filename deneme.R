library(readxl)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(ggprism)

infnodulation <- read_excel("Microscopy.xlsx")

df_p_val1 <- infnodulation %>%
  rstatix::group_by(Day) %>%
  rstatix::t_test(Infection.Threads ~ Treatment) %>%
  rstatix::adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  rstatix::add_significance(p.col = "p.adj") %>% 
  rstatix::add_xy_position(x = "Day", dodge = 0.8) %>% # important for positioning!
  filter(p.adj < 0.05)          # here we have chosen an alpha of 0.05

df_p_val2 <- rstatix::t_test(infnodulation, Infection.Threads ~ Day, 
                             p.adjust.method = "bonferroni") %>% 
  rstatix::add_xy_position() %>%
  filter(p.adj < 0.05) # here we have chosen an alpha of 0.05

p <- ggplot(infnodulation, aes(x = factor(Day), y = Infection.Threads)) + 
  
  labs(y= "Infection threads per plant", x= "Days post-inoculation",
       fill= "Treatment", tag = "A") +
  
  geom_boxplot(aes(fill = Treatment)) + 
  geom_dotplot(aes(fill = Treatment), binaxis='y', stackdir='center', 
               dotsize=1.7, binwidth = 0.5, stackratio = 0.7,  
               position=position_dodge(0.75)) +
  
  theme_prism() + 
  theme(legend.position = "top",
        legend.background = element_rect(fill =  "white"),
        legend.text = element_text(size=12, family = "sans"),
        legend.title = element_text(size=12, family = "sans"),
        axis.text.x = element_text(size=14, face = "bold", family = "sans"),
        axis.text.y = element_text(size=14, face = "bold", family = "sans"),
        axis.title = element_text(face = "bold", family = "sans"))  +
 
  scale_fill_manual(values = c("#F8766D","#FF00FF", "#619CFF"),
                    labels = c("Mock", expression(italic("Pseudomonas sp")%.%"QLc11A"), 
                               expression(italic("Mesorhizobium sp")%.%"Qb1E3-1" + italic("Pseudomonas sp")%.%"QLc11A"))) + 
  
  coord_cartesian(ylim = c(0, 45))

p + add_pvalue(df_p_val1, 
               xmin = "xmin", 
               xmax = "xmax",
               label = "p = {p.adj.signif}",
               tip.Infection.Threadsgth = 0) + 
  add_pvalue(df_p_val2,
             label = "p = {p.adj.signif}",
             tip.Infection.Threadsgth = 0.01,
             bracket.nudge.y = 2,
             step.increase = 0.035)

######################nodulation################################

df_p_val1 <- infnodulation %>%
  rstatix::group_by(Day) %>%
  rstatix::t_test(Nodules ~ Treatment) %>%
  rstatix::adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  rstatix::add_significance(p.col = "p.adj") %>% 
  rstatix::add_xy_position(x = "Day", dodge = 0.8) %>% # important for positioning!
  filter(p.adj < 0.05)          # here we have chosen an alpha of 0.05

df_p_val2 <- rstatix::t_test(infnodulation, Nodules ~ Day, 
                             p.adjust.method = "bonferroni") %>% 
  rstatix::add_xy_position() %>%
  filter(p.adj < 0.05) # here we have chosen an alpha of 0.05

p <- ggplot(infnodulation, aes(x = factor(Day), y = Nodules)) + 
  
  labs(y= "Nodules per plant", x= "Days post-inoculation",
       fill= "Treatment", tag = "A") +
  
  geom_boxplot(aes(fill = Treatment)) + 
  geom_dotplot(aes(fill = Treatment), binaxis='y', stackdir='center', 
               dotsize=1.2, binwidth = 0.3, stackratio = 0.7,  
               position=position_dodge(0.75)) +
  
  theme_prism() + 
  theme(legend.position = "top",
        legend.background = element_rect(fill =  "white"),
        legend.text = element_text(size=12, family = "sans"),
        legend.title = element_text(size=12, family = "sans"),
        axis.text.x = element_text(size=14, face = "bold", family = "sans"),
        axis.text.y = element_text(size=14, face = "bold", family = "sans"),
        axis.title = element_text(face = "bold", family = "sans"))  +
  
  scale_fill_manual(values = c("#F8766D","#FF00FF", "#619CFF"),
                    labels = c("Mock", expression(italic("Pseudomonas sp")%.%"QLc11A"), 
                               expression(italic("Mesorhizobium sp")%.%"Qb1E3-1" + italic("Pseudomonas sp")%.%"QLc11A"))) + 
  
  coord_cartesian(ylim = c(0, 20))

p + add_pvalue(df_p_val1, 
               xmin = "xmin", 
               xmax = "xmax",
               label = "p = {p.adj.signif}",
               tip.Infection.Threadsgth = 0) + 
  add_pvalue(df_p_val2,
             label = "p = {p.adj.signif}",
             tip.Infection.Threadsgth = 0.01,
             bracket.nudge.y = 2,
             step.increase = 0.035)

##########supplementary figure 1################

timecourse <- read_excel("TimeCourse.xlsx")

df_p_val1 <- timecourse %>%
  rstatix::group_by(Day) %>%
  rstatix::t_test(Shoot.Length ~ Treatment) %>%
  rstatix::adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  rstatix::add_significance(p.col = "p.adj") %>% 
  rstatix::add_xy_position(x = "Day", dodge = 0.8) # important for positioning!

p <- ggplot(timecourse, aes(x = factor(Day), y = Shoot.Length)) + 
  
  labs(y= "Shoot length (cm)", x= "Days post-inoculation",
       fill= "Treatment", tag = "A") +
  
  geom_violin(aes(fill = Treatment), trim = F) + 
  
  theme_prism() + 
  theme(legend.position = "top",
        legend.background = element_rect(fill =  "white"),
        legend.text = element_text(size=12, family = "sans"),
        legend.title = element_text(size=12, family = "sans"),
        axis.text.x = element_text(size=14, face = "bold", family = "sans"),
        axis.text.y = element_text(size=14, face = "bold", family = "sans"),
        axis.title = element_text(face = "bold", family = "sans"))  +
  
  scale_fill_manual(values = c("#F8766D","#FF00FF", "#619CFF"),
                    labels = c("Mock", expression(italic("Pseudomonas sp")%.%"QLc11A"), 
                               expression(italic("Mesorhizobium sp")%.%"Qb1E3-1" + italic("Pseudomonas sp")%.%"QLc11A"))) + 
  
  coord_cartesian()

p + add_pvalue(df_p_val1, 
               xmin = "xmin", 
               xmax = "xmax",
               label = "p = {p.adj.signif}",
               tip.Shoot.Lengthgth = 0)

######

df_p_val1 <- timecourse %>%
  rstatix::group_by(Day) %>%
  rstatix::t_test(Root.Length ~ Treatment) %>%
  rstatix::adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  rstatix::add_significance(p.col = "p.adj") %>% 
  rstatix::add_xy_position(x = "Day", dodge = 0.8) # important for positioning!

p <- ggplot(timecourse, aes(x = factor(Day), y = Root.Length)) + 
  
  labs(y= "Root length (cm)", x= "Days post-inoculation",
       fill= "Treatment", tag = "B") +
  
  geom_violin(aes(fill = Treatment), trim = F) + 
  
  theme_prism() + 
  theme(legend.position = "top",
        legend.background = element_rect(fill =  "white"),
        legend.text = element_text(size=12, family = "sans"),
        legend.title = element_text(size=12, family = "sans"),
        axis.text.x = element_text(size=14, face = "bold", family = "sans"),
        axis.text.y = element_text(size=14, face = "bold", family = "sans"),
        axis.title = element_text(face = "bold", family = "sans"))  +
  
  scale_fill_manual(values = c("#F8766D","#FF00FF", "#619CFF"),
                    labels = c("Mock", expression(italic("Pseudomonas sp")%.%"QLc11A"), 
                               expression(italic("Mesorhizobium sp")%.%"Qb1E3-1" + italic("Pseudomonas sp")%.%"QLc11A"))) + 
  
  coord_cartesian()

p + add_pvalue(df_p_val1, 
               xmin = "xmin", 
               xmax = "xmax",
               label = "p = {p.adj.signif}",
               tip.Root.Lengthgth = 0)

######

df_p_val1 <- timecourse %>%
  rstatix::group_by(Day) %>%
  rstatix::t_test(Root.Weight ~ Treatment) %>%
  rstatix::adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  rstatix::add_significance(p.col = "p.adj") %>% 
  rstatix::add_xy_position(x = "Day", dodge = 0.8) # important for positioning!

p <- ggplot(timecourse, aes(x = factor(Day), y = Root.Weight)) + 
  
  labs(y= "Root weight (mg)", x= "Days post-inoculation",
       fill= "Treatment", tag = "C") +
  
  geom_violin(aes(fill = Treatment), trim = F) + 
  
  theme_prism() + 
  theme(legend.position = "top",
        legend.background = element_rect(fill =  "white"),
        legend.text = element_text(size=12, family = "sans"),
        legend.title = element_text(size=12, family = "sans"),
        axis.text.x = element_text(size=14, face = "bold", family = "sans"),
        axis.text.y = element_text(size=14, face = "bold", family = "sans"),
        axis.title = element_text(face = "bold", family = "sans"))  +
  
  scale_fill_manual(values = c("#F8766D","#FF00FF", "#619CFF"),
                    labels = c("Mock", expression(italic("Pseudomonas sp")%.%"QLc11A"), 
                               expression(italic("Mesorhizobium sp")%.%"Qb1E3-1" + italic("Pseudomonas sp")%.%"QLc11A"))) + 
  
  coord_cartesian()

p + add_pvalue(df_p_val1, 
               xmin = "xmin", 
               xmax = "xmax",
               label = "p = {p.adj.signif}",
               tip.Root.Lengthgth = 0)

#########pseudomonas inoculation#######

df_p_val <- rstatix::t_test(hypo3_df, Root.Weight ~ Treatment, ref.group = "Mock") %>% 
  rstatix::add_xy_position()


p <- ggplot(hypo3_df, aes(x = factor(Treatment, levels = c("Mock","PLb12A","QLb11B", "LLb11B","Root68", "BP11-1-1", 
                                                           "LLb12B", "Lb2C-1","QLb11B-75mL", "PLb12A-75mL")), y = Root.Weight)) + 
  
  labs(y= "Root weight (mg)", x= "Treatment",
       fill= "Treatment", tag = "A") +
  
  geom_violin(fill='#A4A4A4',trim = F) + 
  
  theme_prism() + 
  theme(legend.position = "none",
        legend.background = element_rect(fill =  "white"),
        legend.text = element_text(size=12, family = "sans"),
        legend.title = element_text(size=12, family = "sans"),
        axis.text.x = element_text(size=14, face = "bold", family = "sans", angle = 45),
        axis.text.y = element_text(size=14, face = "bold", family = "sans"),
        axis.title = element_text(face = "bold", family = "sans"))  +
  
  coord_cartesian()

p + add_pvalue(df_p_val, 
               label = "p = {p.adj}",
               remove.bracket = TRUE, y.position = 220)

###########

df_p_val1 <- timecourse %>%
  rstatix::group_by(Day) %>%
  rstatix::t_test(Root.Weight ~ Treatment) %>%
  rstatix::adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  rstatix::add_significance(p.col = "p.adj") %>% 
  rstatix::add_xy_position(x = "Day", dodge = 0.8) # important for positioning!

p <- ggplot(timecourse, aes(x = factor(Day), y = Root.Weight)) + 
  
  labs(y= "Root weight (mg)", x= "Days post-inoculation",
       fill= "Treatment", tag = "C") +
  
  geom_violin(aes(fill = Treatment), trim = F) + 
  
  theme_prism() + 
  theme(legend.position = "top",
        legend.background = element_rect(fill =  "white"),
        legend.text = element_text(size=12, family = "sans"),
        legend.title = element_text(size=12, family = "sans"),
        axis.text.x = element_text(size=14, face = "bold", family = "sans"),
        axis.text.y = element_text(size=14, face = "bold", family = "sans"),
        axis.title = element_text(face = "bold", family = "sans"))  +
  
  scale_fill_manual(values = c("#F8766D","#FF00FF", "#619CFF"),
                    labels = c("Mock", expression(italic("Pseudomonas sp")%.%"QLc11A"), 
                               expression(italic("Mesorhizobium sp")%.%"Qb1E3-1" + italic("Pseudomonas sp")%.%"QLc11A"))) + 
  
  coord_cartesian()

p + add_pvalue(df_p_val1, 
               xmin = "xmin", 
               xmax = "xmax",
               label = "p = {p.adj.signif}",
               tip.Root.Lengthgth = 0)

#########pseudomonas inoculation#######

df_p_val <- rstatix::t_test(hypo3_df, Root.Length ~ Treatment, ref.group = "Mock") %>% 
  rstatix::add_xy_position()


p <- ggplot(hypo3_df, aes(x = factor(Treatment, levels = c("Mock","PLb12A","QLb11B", "LLb11B","Root68", "BP11-1-1", 
                                                           "LLb12B", "Lb2C-1","QLb11B-75mL", "PLb12A-75mL")), y = Root.Length)) + 
  
  labs(y= "Root Length (cm)", x= "Treatment",
       fill= "Treatment", tag = "B") +
  
  geom_violin(fill='#A4A4A4',trim = F) + 
  
  theme_prism() + 
  theme(legend.position = "none",
        legend.background = element_rect(fill =  "white"),
        legend.text = element_text(size=12, family = "sans"),
        legend.title = element_text(size=12, family = "sans"),
        axis.text.x = element_text(size=14, face = "bold", family = "sans", angle = 45),
        axis.text.y = element_text(size=14, face = "bold", family = "sans"),
        axis.title = element_text(face = "bold", family = "sans"))  +
  
  coord_cartesian()

p + add_pvalue(df_p_val, 
               label = "p = {p.adj}",
               remove.bracket = TRUE, y.position = 20)
