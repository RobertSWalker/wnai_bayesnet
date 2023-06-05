library(bnlearn);library(readxl);library(dplyr);library(ggplot2);library(tidyr)#install.packages("BiocManager") BiocManager::install("Rgraphviz")
join_df <- read.csv("df.csv")

#create supernatural
join_df$supernatural <- join_df$magic + join_df$shamans + join_df$Spirits + join_df$illness

#create ritual
join_df$ritual <- join_df$ceremony + join_df$lifecycle

#create agriculture
join_df$Agriculture <- join_df$Agricultrual.production
join_df$ag <- factor(ifelse(join_df$Agriculture == 1, "Hunter-gatherer", 
                            ifelse(join_df$Agriculture < 5, "Incipient horticulture",
                                   ifelse(join_df$Agriculture < 6, "Non-food agriculture",
                                          ifelse(join_df$Agriculture < 7, "Agriculture <50%", "Agriculture >50%")))),
                     levels = c("Hunter-gatherer", "Incipient horticulture", "Non-food agriculture",'Agriculture <50%', 'Agriculture >50%' ))
levels(join_df$ag)
table(join_df$ag)

join_df$"Population_density" <- join_df$pop.density

#create map
library(ggplot2);library(sf);library(rnaturalearth);library(dplyr)
library(rnaturalearthdata);library(ggspatial);library(cowplot);theme_set(theme_cowplot())
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world);library(maps)
ggplot(data = world) +
   geom_sf(fill="antiquewhite") + #[!is.na(df$rivtype),] 
   #geom_sf(data = white, colour = "red") +
   #geom_sf(data = black, colour = "black") +
   #geom_sf(data = merged[merged$River_Order > 7,], colour = "lightblue") +
   geom_point(data = join_df, aes(x = -Long, y = Lat, fill=ag, size = Population_density) , #material_culture) , #div_of_labor) , #Population_density),
              colour="black",pch=21, alpha = .8, alpha=.7) +
   scale_fill_manual(values=c('green','blue', "gray50", "yellow", 'red'), name="Agricultural intensity") + 
   #scale_color_continuous( name="Population density bins") + 
   # geom_sf(data = st_as_sf(river3), colour = "blue") +
   #geom_tile(data = dfpred, mapping = aes(x = x, y = y, fill=Prediction), alpha = .7, size = 1) +
   coord_sf(xlim = c(-140, -103), ylim = c(29.2, 60), expand = FALSE) +
   annotation_scale(location = "bl", width_hint = 0.4) +
   #annotation_north_arrow(location = "bl", which_north = "true", 
   #                       pad_x = unit(.3, "in"), pad_y = unit(.3, "in"),
   #                       style = north_arrow_fancy_orienteering) +
   xlab("") + ylab("") +
   #ggtitle("Manioc map", subtitle = "(with phosphorus)") +
   theme(legend.position = c(0.01, 0.3),
         axis.title=element_blank(),
         axis.text=element_blank(),
         axis.ticks=element_blank(),
         #panel.background = element_blank(),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         panel.border = element_rect(colour = "black", fill=NA, size=1),
         #panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), 
         panel.background = element_rect(fill = "aliceblue"))
ggsave("map.pdf")

#keep all culture nodes
culture.nodes <- c('Agriculture', 'technology','material_culture', 'Subsistence', 'ritual', 'supernatural', 
                   'pop.density', 'div_of_labor', 'econ_dist', 'marriage',
                   'Politics', 'war', 'property')
all <- join_df[, culture.nodes  ]
all <- data.frame(lapply(all, as.numeric))

#look at corr matrix
library(corrplot)
res <- cor(all)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#rename nodes
names(all) <- c("Agricultural intensity", 'Technology', 'Material culture', 'Subsistence', 'Ritual',
                'Supernatural', "Population density", "Division of labor", "Economic distribution", 'Marriage',
                "Politics", "War", "Property")

#bootstrap multiple network structures
set.seed(1)
boot <- boot.strength(all, R = 10001, cpdag = FALSE,
                      algorithm = "tabu")
boot[boot$strength > 0.8 & boot$direction >= 0.5, ]
avg.boot <- averaged.network(boot, threshold = .8)
plot(boot)
avg.boot

#plot strength plot
strength.plot(avg.boot, boot, threshold=.8, #highlight = c('Politics'),
              layout = "dot", #dot, neato, twopi, circo and fdp
              shape = "rectangle") #circle ellipse rectangle

fit = bn.fit(avg.boot, all)
fit
print(bn.cv(all, bn = avg.boot, k = nrow(all)) )

#compare algorithms
usedf2 <- all
print(bn.cv(usedf2, 'hc', k = nrow(usedf2) ))
print(bn.cv(usedf2, 'tabu', k = nrow(usedf2)) )
print(bn.cv(usedf2, 'mmhc', k = nrow(usedf2)) )
print(bn.cv(usedf2, 'h2pc', k = nrow(usedf2)) )
print(bn.cv(usedf2, 'rsmax2', k = nrow(usedf2)) )
print(bn.cv(usedf2, 'chow.liu', k = nrow(usedf2)) )
print(bn.cv(usedf2, 'aracne', k = nrow(usedf2)) )
print(bn.cv(usedf2, 'pc.stable', k = nrow(usedf2)) )
print(bn.cv(usedf2, 'chow.liu', k = nrow(usedf2)) )
print(bn.cv(usedf2, 'gs', k = nrow(usedf2)) )
print(bn.cv(usedf2, 'iamb.fdr', k = nrow(usedf2)) )

#run bayesian path model
library(brms);library(rstan);options(mc.cores = parallel::detectCores());rstan_options(auto_write = TRUE)

ritualmod <- bf(ritual ~ 1  + war + Subsistence + pop.density +Agriculture + (1|phyla) + gp(Lat, Long))
warmod <- bf(war ~ 1  + Politics + (1|phyla) + gp(Lat, Long))
propmod <- bf(property ~ 1  + econ_dist + Politics + (1|phyla) + gp(Lat, Long))
politicsmod <- bf(Politics ~ 1  + pop.density + (1|phyla) + gp(Lat, Long))
sharemod <- bf(econ_dist ~ 1  + marriage + pop.density + (1|phyla) + gp(Lat, Long))
marriagemod <- bf(marriage ~ 1  + Subsistence + (1|phyla) + gp(Lat, Long))
snmod <- bf(supernatural ~ 1  + Agriculture + (1|phyla) + gp(Lat, Long))
dolmod <- bf(div_of_labor ~ 1  + material_culture + Subsistence + Agriculture + (1|phyla) + gp(Lat, Long))
techmod <- bf(technology ~ 1 + Agriculture + material_culture + (1|phyla) + gp(Lat, Long)) 

mv <- mvbf( techmod + sharemod + marriagemod + ritualmod + snmod + dolmod + propmod + politicsmod + warmod,
            rescor=FALSE )
m <- brm(data = join_df, family = gaussian(link = "identity"),
      mv, prior = set_prior("normal(0,.5)", class = "b"), # group = ""),
      iter =  2e3, chains = 2, cores = 3, #save_all_pars = TRUE,
      control = list(adapt_delta = .999, max_treedepth = 20),
      seed = 14, backend = "cmdstanr")
prior_summary(m)
m
pl <- plot(m, N = 4, ask = FALSE) #Trace and Density Plots for MCMC Samples
posterior_summary(m)
bayes_R2(m) #  , nug = .000001) #tech.79, 
conditional_effects(m, points=T)
saveRDS(m,"m.Rds")
m <- readRDS("m.Rds") 
ranef(m)

