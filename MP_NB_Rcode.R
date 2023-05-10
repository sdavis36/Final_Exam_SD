
library(tidyverse)
library(dplyr)
library(magrittr)

#load in plastic particle data
all_plastic <- read.csv("Master Plastics Analysis.csv")

#Filter the data to keep only the relevant environmental samples (no control samples)
net_samples <- filter(all_plastic, SampleType == "NS")

#summarize the number of particles per sample event, normalized for the amount of water sampled 
particles_per_event <- net_samples %>%
  group_by(Date, SampleEvent, Coord_Starting_X, Coord_Starting_Y, Total_Water_Sampled_m3, Season, Site) %>%
  summarise(total_particles_sampleevent = n())

particles_per_event <- particles_per_event %>%
  mutate(Particles_per_meter_cubed = total_particles_sampleevent/Total_Water_Sampled_m3)

particles_per_event$Date <- as.Date(particles_per_event$Date, format = "%m/%d/%Y")

#rename the sites according to their numerical label for map comparison
particles_per_event$Site <- gsub("BLS", "6",
                                 gsub("CPO", "1",
                                      gsub("HOP", "3",
                                           gsub("GIO", "4",
                                                gsub("WDI", "5",
                                                     gsub("HOG", "2", particles_per_event$Site))))))

#calculate the average particles/m3 across both years at each site, while considering the standard error.
particles_per_site_season <- particles_per_event %>%
  group_by(Season, Site) %>%
  summarise(mean = mean(Particles_per_meter_cubed), 
            mean_se = mean_se(Particles_per_meter_cubed))

print(particles_per_site_season)

#plot the average outcomes for each season by site in a box plot to show variation, with error bars for standard error included
box_particles_per_site_season_plot <- ggplot(particles_per_event, aes(x=Site, y=Particles_per_meter_cubed, fill=Season)) +
  geom_boxplot(width=0.5, coef=2, position=position_dodge(0.9)) +
  labs(x="Site", y="Average MPs per m3 of Seawater", title="Average MP Concentrations in Narragansett Bay", fill="Season") +
  scale_fill_manual(values = c("#FFCA99", "#CC5800", "#1E8E99")) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_blank(),
        panel.spacing = unit(0.1, "lines"))


plot(box_particles_per_site_season_plot)

#plot individual seasons for easier visual comparison
#Spring
box_spring_only_particles_plot <- ggplot(subset(particles_per_event, Season == "Spring"), aes(x=Site, y=Particles_per_meter_cubed)) +
  geom_boxplot(fill="#FFCA99") +
  labs(x="Site", y="Average MPs per m3 Seawater",title="Spring MPs in Narragansett Bay") +
  scale_y_continuous(limits = c(0, 1.9)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_blank(),
        panel.spacing = unit(0.1, "lines"))

plot(box_spring_only_particles_plot)

#Summer/Fall
box_summerfall_only_particles_plot <- ggplot(subset(particles_per_event, Season == "Summer/Fall"), aes(x=Site, y=Particles_per_meter_cubed)) +
  geom_boxplot(fill="#CC5800") +
  labs(x="Site", y="Average MPs per m3 Seawater") +
  scale_y_continuous(limits = c(0, 1.9)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_blank(),
        panel.spacing = unit(0.1, "lines"))

plot(box_summerfall_only_particles_plot)
#Winter
box_winter_only_particles_plot <- ggplot(subset(particles_per_event, Season == "Winter"), aes(x=Site, y=Particles_per_meter_cubed)) +
  geom_boxplot(fill="#1E8E99") +
  labs(x="Site", y="Average MPs per m3 Seawater") +
  scale_y_continuous(limits = c(0, 1.9)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_blank(),
        panel.spacing = unit(0.1, "lines"))

plot(box_winter_only_particles_plot)

# Sort data by date
particles_per_event <- particles_per_event %>% arrange(Date)

#summarize and visualize polymer data by season
polymer_season <- net_samples %>%
  filter(!is.na(material_ID)) %>%
  group_by(Season) %>%
  summarise(
    PE = sum(material_ID == "PE"),
    PP = sum(material_ID =="PP"),
    Nylon = sum(material_ID == "Nylon"),
    PVC = sum(material_ID == "PVC"),
    EVA = sum(material_ID == "EVA"),
    PS = sum(material_ID == "PS"), 
    Other = sum(material_ID %in% c("ABS", "Acrylic", "CR", "Diisooctyl Adepate", "Ethylene Propylene Diene Terpolymer", "PA", "PAN", "PC", "PET", "PET.1", "PEVA", "Polyester", "Polyisoprene", "PVA"))
  ) %>%
  transform(Total = PE + PP + Nylon + EVA + PVC + PS + Other)


#calculate proportions of total for each polymer type
polymer_season$PP <- polymer_season$PP / polymer_season$Total * 100
polymer_season$PE <- polymer_season$PE / polymer_season$Total * 100
polymer_season$Nylon <- polymer_season$Nylon / polymer_season$Total * 100
polymer_season$PVC <- polymer_season$PVC / polymer_season$Total * 100
polymer_season$EVA <- polymer_season$EVA / polymer_season$Total * 100
polymer_season$PS <- polymer_season$PS / polymer_season$Total * 100
polymer_season$Other <- polymer_season$Other / polymer_season$Total *100

#make the data long
polymer_season_long <- polymer_season %>%
  select(-Total) %>%
  gather(key = "category", value = "count", -Season)

#install and use color pallete for plot
library(paletteer)
polymer_palette <- paletteer_d("ggsci::default_locuszoom")
                               
#visualize polymer data
polymer_season_plot <- ggplot(polymer_season_long, aes(x = Season, y = count, fill = reorder(category, count))) +
  geom_bar(stat = "identity") +
  labs(x = "Season", y = "Percent of Total MPs", fill = "Polymer Type") +
  scale_fill_manual(values = polymer_palette) +
  theme(panel.background = element_blank()) +
  theme(axis.line = element_line(color = "black", linewidth = 0.5),
        axis.title = element_text(color = "black"))

plot(polymer_season_plot)

#summarize and visualize polymer data by site
polymer_site <- net_samples %>%
  filter(!is.na(material_ID)) %>%
  group_by(Site) %>%
  summarise(
    PE = sum(material_ID == "PE"),
    PP = sum(material_ID =="PP"),
    Nylon = sum(material_ID == "Nylon"),
    PVC = sum(material_ID == "PVC"),
    EVA = sum(material_ID == "EVA"),
    PS = sum(material_ID == "PS"), 
    Other = sum(material_ID %in% c("ABS", "Acrylic", "CR", "Diisooctyl Adepate", "Ethylene Propylene Diene Terpolymer", "PA", "PAN", "PC", "PET", "PET.1", "PEVA", "Polyester", "Polyisoprene", "PVA"))
  ) %>%
  transform(Total = PE + PP + Nylon + EVA + PVC + PS + Other)

#calculate proportions of total for each polymer type
polymer_site$PP <- polymer_site$PP / polymer_site$Total * 100
polymer_site$PE <- polymer_site$PE / polymer_site$Total * 100
polymer_site$Nylon <- polymer_site$Nylon / polymer_site$Total * 100
polymer_site$PVC <- polymer_site$PVC / polymer_site$Total * 100
polymer_site$EVA <- polymer_site$EVA / polymer_site$Total * 100
polymer_site$PS <- polymer_site$PS / polymer_site$Total * 100
polymer_site$Other <- polymer_site$Other / polymer_site$Total *100

#make the data long
polymer_site_long <- polymer_site %>%
  select(-Total) %>%
  gather(key = "category", value = "count", -Site)

#rename the sites to numerical for map comparison
polymer_site_long$Site <- gsub("BLS", "1",
                                       gsub("CPO", "4",
                                            gsub("HOP", "5",
                                                 gsub("GIO", "2",
                                                      gsub("WDI", "6",
                                                           gsub("HOG", "3", polymer_site_long$Site))))))

polymer_site_plot <- ggplot(polymer_site_long, aes(x=Site, y = count, fill = reorder(category, count))) +
  geom_bar(stat = "identity") +
  labs(x = "Site", y = "Percent of Total MPs", fill = "Polymer Type") +
  scale_fill_manual(values = polymer_palette) +
  theme(panel.background = element_blank()) +
  theme(axis.line = element_line(color = "black", linewidth = 0.5),
        axis.title = element_text(color = "black"))

plot(polymer_site_plot)

#summarize and visualize color by site
color_summary_site <- net_samples %>%
  filter(!is.na(Color)) %>%
  group_by(Site) %>%
  summarise(
    Clear = sum(Color == "Clear"),
    White = sum(Color == "White"),
    Black = sum(Color == "Black"),
    Blue = sum(Color == "Blue"),
    Yellow = sum(Color == "Yellow"),
    Red = sum(Color == "Red"),
    Orange = sum(Color == "Orange"),
    Tan = sum(Color == "Tan/Beige"),
    Pink = sum(Color == "Pink"),
    Violet = sum(Color == "Violet"),
    Brown = sum(Color == "Brown"),
    Grey = sum(Color == "Grey"),
    Green = sum(Color == "Green")
  ) %>%
  transform(Total = Clear + White + Black + Blue + Yellow + Red + Orange + Tan + Pink + Violet + Brown + Grey + Green)

color_summary_site$Clear <- color_summary_site$Clear / color_summary_site$Total * 100
color_summary_site$White <- color_summary_site$White / color_summary_site$Total * 100
color_summary_site$Black <- color_summary_site$Black / color_summary_site$Total * 100
color_summary_site$Blue <- color_summary_site$Blue / color_summary_site$Total * 100
color_summary_site$Yellow <- color_summary_site$Yellow / color_summary_site$Total * 100
color_summary_site$Red <- color_summary_site$Red / color_summary_site$Total * 100
color_summary_site$Orange <- color_summary_site$Orange / color_summary_site$Total * 100
color_summary_site$Tan <- color_summary_site$Tan / color_summary_site$Total * 100
color_summary_site$Pink <- color_summary_site$Pink / color_summary_site$Total * 100
color_summary_site$Violet <- color_summary_site$Violet / color_summary_site$Total * 100
color_summary_site$Brown <- color_summary_site$Brown / color_summary_site$Total * 100
color_summary_site$Grey <- color_summary_site$Grey / color_summary_site$Total * 100
color_summary_site$Green <- color_summary_site$Green / color_summary_site$Total * 100

color_site_long <- color_summary_site %>%
  select(-Total) %>%
  gather(key = "category", value = "count", -Site)

color_site_long$Site <- gsub("BLS", "1",
                               gsub("CPO", "4",
                                    gsub("HOP", "5",
                                         gsub("GIO", "2",
                                              gsub("WDI", "6",
                                                   gsub("HOG", "3", color_site_long$Site))))))

particle_colors <- c("Clear" = "ghostwhite", "White" = "white", "Black" = "black", "Blue" = "blue", "Violet" = "violet", "Red" = "red", "Pink" = "pink", "Orange" = "orange", "Yellow" = "yellow", "Tan" = "tan", "Brown" = "brown", "Green" = "#4CBB17", "Grey" = "grey")

color_site_plot <- ggplot(color_site_long, aes(x = Site, y = count, fill = reorder(category, count))) +
  geom_bar(stat = "identity", color = "black") +  # add black outline to bars
  labs(x = "Site", y = "Percent of Total MPs", fill = "Category") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = particle_colors) +
  theme(panel.background = element_blank()) +
  theme(axis.line = element_line(color = "black", linewidth = 0.5),
        axis.title = element_text(color = "black"))
  
plot(color_site_plot)

#summarize and visualize color by season 
color_summary_season <- net_samples %>%
  filter(!is.na(Color)) %>%
  group_by(Season) %>%
  summarise(
    Clear = sum(Color == "Clear"),
    White = sum(Color == "White"),
    Black = sum(Color == "Black"),
    Blue = sum(Color == "Blue"),
    Yellow = sum(Color == "Yellow"),
    Red = sum(Color == "Red"),
    Orange = sum(Color == "Orange"),
    Tan = sum(Color == "Tan"),
    Pink = sum(Color == "Pink"),
    Violet = sum(Color == "Violet"),
    Brown = sum(Color == "Brown"),
    Grey = sum(Color == "Grey"),
    Green = sum(Color == "Green")
  ) %>%
  transform(Total = Clear + White + Black + Blue + Yellow + Red + Orange + Tan + Pink + Violet + Brown + Grey + Green)

color_summary_season$Clear <- color_summary_season$Clear / color_summary_season$Total * 100
color_summary_season$White <- color_summary_season$White / color_summary_season$Total * 100
color_summary_season$Black <- color_summary_season$Black / color_summary_season$Total * 100
color_summary_season$Blue <- color_summary_season$Blue / color_summary_season$Total * 100
color_summary_season$Yellow <- color_summary_season$Yellow / color_summary_season$Total * 100
color_summary_season$Red <- color_summary_season$Red / color_summary_season$Total * 100
color_summary_season$Orange <- color_summary_season$Orange / color_summary_season$Total * 100
color_summary_season$Tan <- color_summary_season$Tan / color_summary_season$Total * 100
color_summary_season$Pink <- color_summary_season$Pink / color_summary_season$Total * 100
color_summary_season$Violet <- color_summary_season$Violet / color_summary_season$Total * 100
color_summary_season$Brown <- color_summary_season$Brown / color_summary_season$Total * 100
color_summary_season$Grey <- color_summary_season$Grey / color_summary_season$Total * 100
color_summary_season$Green <- color_summary_season$Green / color_summary_season$Total * 100

color_season_long <- color_summary_season %>%
  select(-Total) %>%
  gather(key = "category", value = "count", -Season)

particle_colors <- c("Clear" = "ghostwhite", "White" = "white", "Black" = "black", "Blue" = "blue", "Violet" = "violet", "Red" = "red", "Pink" = "pink", "Orange" = "orange", "Yellow" = "yellow", "Tan" = "tan", "Brown" = "brown", "Green" = "#4CBB17", "Grey" = "grey")

color_season_plot <- ggplot(color_season_long, aes(x = Season, y = count, fill = reorder(category, count))) +
  geom_bar(stat = "identity", color = "black") +  # add black outline to bars
  labs(x = "Season", y = "Percent of Total MPs", fill = "Category") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = particle_colors) +
  theme(panel.background = element_blank()) +
  theme(axis.line = element_line(color = "black", linewidth = 0.5),
        axis.title = element_text(color = "black"))

plot(color_season_plot)

#summarize particle length by site and season
avg_length_site_season <- net_samples %>%
  filter(!is.na(L..um.)) %>%
  filter(!is.na(W..um.)) %>%
  group_by(Site, Season) %>%
  summarise(avg_length = mean(L..um.), std_error = mean_se(L..um.))
  
avg_length_site_season$Site <- gsub("BLS", "1",
                                    gsub("CPO", "4",
                                         gsub("HOP", "5",
                                              gsub("GIO", "2",
                                                   gsub("WDI", "6",
                                                        gsub("HOG", "3", avg_length_site_season$Site))))))
#visualize particle length averages
avg_length_plot <- ggplot(avg_length_site_season, aes(x=Site, y = avg_length, fill = Season)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=std_error$ymin, ymax=std_error$ymax), width=.2, position=position_dodge(.9)) +
  labs(x="Site", y="Average Particle Length (μm)", fill="Season") +
  scale_fill_manual(values = c("#FFCA99", "#CC5800", "#1E8E99")) +
  theme(panel.background = element_blank()) +
  theme(axis.line = element_line(color = "black", linewidth = 0.5),
        axis.title = element_text(color = "black"))

plot(avg_length_plot)

#summarize and visualize particle morphology data by season
morphology_summary_season_table <- net_samples %>%
  filter(!is.na(Shape.Category)) %>%
  group_by(Season) %>%
  summarise(
    Fibers = sum(Shape.Category == "Fiber"),
    Fragments = sum(Shape.Category == "Fragment"),
    Beads = sum(Shape.Category == "Bead")
  )

morphology_summary_season <- morphology_summary_season_table %>%
  transform(Total = Fibers + Fragments + Beads)

morphology_summary_season$Fibers <- morphology_summary_season$Fibers / morphology_summary_season$Total * 100
morphology_summary_season$Fragments <- morphology_summary_season$Fragments / morphology_summary_season$Total * 100
morphology_summary_season$Beads <- morphology_summary_season$Beads / morphology_summary_season$Total * 100

#summarize and visualize morphology data by site
morphology_summary_site_table <- net_samples %>%
  filter(!is.na(Shape.Category)) %>%
  group_by(Site) %>%
  summarise(
    Fibers = sum(Shape.Category == "Fiber"),
    Fragments = sum(Shape.Category == "Fragment"),
    Beads = sum(Shape.Category == "Bead")
  )
morphology_summary_site <- morphology_summary_site_table %>%
  transform(Total = Fibers + Beads + Fragments)

morphology_summary_site$Fibers <- morphology_summary_site$Fibers / morphology_summary_site$Total * 100
morphology_summary_site$Fragments <- morphology_summary_site$Fragments / morphology_summary_site$Total * 100
morphology_summary_site$Beads <- morphology_summary_site$Beads / morphology_summary_site$Total * 100

morphology_summary_site$Site <- gsub("BLS", "1",
                                     gsub("CPO", "4",
                                          gsub("HOP", "5",
                                               gsub("GIO", "2",
                                                    gsub("WDI", "6",
                                                         gsub("HOG", "3", morphology_summary_site$Site))))))



#Plot morphology by season
morphology_season_long <- morphology_summary_season %>% 
  select(-Total) %>%
  gather(key = "category", value = "count", -Season)

morphology_season_plot <- ggplot(morphology_season_long, aes(x = Season, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  labs(x = "Season", y = "Percent of Total MPs", fill = "Particle Type") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_blank()) +
  scale_fill_manual(values = c("#8F8782", "#A5ACAF", "#414451")) +
  theme(axis.line = element_line(color = "black", linewidth = 0.5),
        axis.title = element_text(color = "black"))

#Plot morphology by site
morphology_site_long <- morphology_summary_site %>%
  select(-Total) %>%
  gather(key = "category", value = "count", -Site)

morphology_site_plot <- ggplot(morphology_site_long, aes(x=Site, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  labs(x = "Site", y = "Percent of Total MPs", fill = "Particle Type") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_blank()) +
  scale_fill_manual(values = c("#8F8782", "#A5ACAF", "#414451")) +
  theme(axis.line = element_line(color = "black", linewidth = 0.5),
        axis.title = element_text(color = "black"))

plot(morphology_site_plot)
plot(morphology_season_plot)

sampling_event_data <- read.csv("sample_event_data.csv")

#model linear relationship between latitude (significant predictor variable) and MP particle yield
latitude_lm <- lm(data=sampling_event_data, Particles_per_meter_cubed~Coord_Starting_Y)

stats <- summary(latitude_lm)
print(stats)

#create a scatter plot of the linear model with the regression line
lat <- ggplot(sampling_event_data, aes(x = Coord_Starting_Y, y = Particles_per_meter_cubed)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +
  labs(x = "Latitude", y = "MP Yields", title="Relationship between Latitude and MP Concentrations in Narragansett Bay Surface Water") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))

plot(lat)

