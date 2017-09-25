
# LACA Only -------------
# png("reaction-norm-sla-draft3.png", width = 1000, height = 1200)
png("~/grad/quals/presentation/img/ch2-prelim_figs/laca_only.png", width = 1000, height = 1200)
par(oma = c(2,2.5,0,0))
plot(1, type = "n", xlim = c(0,3), ylim = c(1.9,2.7), xaxt = "n",xlab = "", ylab = "", cex.axis = 2.4)
mtext(side = 1, line = 4, cex = 3, "Competitive background")
mtext(side = 2, line = 3, cex = 3, expression(log[10]*"(Specific leaf area) (cm"^2*"/g)"))
axis(side = 1, at = c(1, 2.5), labels = c("No Competition", "Competition"), cex.axis = 2.4)

which_sp <- 7
# for (ii in 1:length(unique(individual_traits$species))) {
for (ii in which_sp) {
  current_species = unique(individual_traits$species)[ii]
  comp <- individual_traits %>% filter(species == current_species) %>%
    filter(plot_type == "comp") %>% ungroup() %>%
    select(species, log_sla_cm2_g)
  non_comp <- individual_traits %>% filter(species == current_species) %>%
    filter(plot_type == "lambda") %>% ungroup() %>%
    select(species, log_sla_cm2_g)
  
  boxplot(non_comp$log_sla_cm2_g, at = .7+((ii-1)*.04), add = T, boxwex = .2, type = "n", yaxt = "n", col = colors[ii], bty = "n", boxlwd = 4, lwd = 3, cex = 2, pch = 19, outcol = colors[ii])
  boxplot(comp$log_sla_cm2_g, at = 2.2+((ii-1)*.04), add = T, boxwex = .2, type = "n", yaxt = "n", ylab = "", col = colors[ii], bty = "n", boxlwd = 4, lwd = 3, cex =2, pch = 19, outcol = colors[ii])
  
  segments(x0 =.7+((ii-1)*.04) , y0 = median(non_comp$log_sla_cm2_g), x1 = 2.2+((ii-1)*.04), y1 = median(comp$log_sla_cm2_g), col = "black", lwd = 8)
  segments(x0 =.7+((ii-1)*.04) , y0 = median(non_comp$log_sla_cm2_g), x1 = 2.2+((ii-1)*.04), y1 = median(comp$log_sla_cm2_g), col = colors[ii], lwd = 4)
  
}

legend("topleft", bty ="n", pch = 21, pt.bg = colors[which_sp], cex = 2, pt.cex = 3.5, pt.lwd = 3, legend = toupper(unique(individual_traits$species)[which_sp]))
dev.off()

# LACA and EUPE ----
png("~/grad/quals/presentation/img/ch2-prelim_figs/laca_eupe.png", width = 1000, height = 1200)
par(oma = c(2,2.5,0,0))
plot(1, type = "n", xlim = c(0,3), ylim = c(1.9,2.7), xaxt = "n",xlab = "", ylab = "", cex.axis = 2.4)
mtext(side = 1, line = 4, cex = 3, "Competitive background")
mtext(side = 2, line = 3, cex = 3, expression(log[10]*"(Specific leaf area) (cm"^2*"/g)"))
axis(side = 1, at = c(1, 2.5), labels = c("No Competition", "Competition"), cex.axis = 2.4)

which_sp <- c(7, 5)
# for (ii in 1:length(unique(individual_traits$species))) {
for (ii in which_sp) {
  current_species = unique(individual_traits$species)[ii]
  comp <- individual_traits %>% filter(species == current_species) %>%
    filter(plot_type == "comp") %>% ungroup() %>%
    select(species, log_sla_cm2_g)
  non_comp <- individual_traits %>% filter(species == current_species) %>%
    filter(plot_type == "lambda") %>% ungroup() %>%
    select(species, log_sla_cm2_g)
  
  boxplot(non_comp$log_sla_cm2_g, at = .7+((ii-1)*.04), add = T, boxwex = .2, type = "n", yaxt = "n", col = colors[ii], bty = "n", boxlwd = 4, lwd = 3, cex = 2, pch = 19, outcol = colors[ii])
  boxplot(comp$log_sla_cm2_g, at = 2.2+((ii-1)*.04), add = T, boxwex = .2, type = "n", yaxt = "n", ylab = "", col = colors[ii], bty = "n", boxlwd = 4, lwd = 3, cex =2, pch = 19, outcol = colors[ii])
  
  segments(x0 =.7+((ii-1)*.04) , y0 = median(non_comp$log_sla_cm2_g), x1 = 2.2+((ii-1)*.04), y1 = median(comp$log_sla_cm2_g), col = "black", lwd = 8)
  segments(x0 =.7+((ii-1)*.04) , y0 = median(non_comp$log_sla_cm2_g), x1 = 2.2+((ii-1)*.04), y1 = median(comp$log_sla_cm2_g), col = colors[ii], lwd = 4)
  
}

legend("topleft", bty ="n", pch = 21, pt.bg = colors[which_sp], cex = 2, pt.cex = 3.5, pt.lwd = 3, legend = toupper(unique(individual_traits$species)[which_sp]))
dev.off()

# LACA and EUPE and PLER ----
png("~/grad/quals/presentation/img/ch2-prelim_figs/laca_eupe_pler.png", width = 1000, height = 1200)
par(oma = c(2,2.5,0,0))
plot(1, type = "n", xlim = c(0,3), ylim = c(1.9,2.7), xaxt = "n",xlab = "", ylab = "", cex.axis = 2.4)
mtext(side = 1, line = 4, cex = 3, "Competitive background")
mtext(side = 2, line = 3, cex = 3, expression(log[10]*"(Specific leaf area) (cm"^2*"/g)"))
axis(side = 1, at = c(1, 2.5), labels = c("No Competition", "Competition"), cex.axis = 2.4)

which_sp <- c(7, 5, 12)
# for (ii in 1:length(unique(individual_traits$species))) {
for (ii in which_sp) {
  current_species = unique(individual_traits$species)[ii]
  comp <- individual_traits %>% filter(species == current_species) %>%
    filter(plot_type == "comp") %>% ungroup() %>%
    select(species, log_sla_cm2_g)
  non_comp <- individual_traits %>% filter(species == current_species) %>%
    filter(plot_type == "lambda") %>% ungroup() %>%
    select(species, log_sla_cm2_g)
  
  boxplot(non_comp$log_sla_cm2_g, at = .7+((ii-1)*.04), add = T, boxwex = .2, type = "n", yaxt = "n", col = colors[ii], bty = "n", boxlwd = 4, lwd = 3, cex = 2, pch = 19, outcol = colors[ii])
  boxplot(comp$log_sla_cm2_g, at = 2.2+((ii-1)*.04), add = T, boxwex = .2, type = "n", yaxt = "n", ylab = "", col = colors[ii], bty = "n", boxlwd = 4, lwd = 3, cex =2, pch = 19, outcol = colors[ii])
  
  segments(x0 =.7+((ii-1)*.04) , y0 = median(non_comp$log_sla_cm2_g), x1 = 2.2+((ii-1)*.04), y1 = median(comp$log_sla_cm2_g), col = "black", lwd = 8)
  segments(x0 =.7+((ii-1)*.04) , y0 = median(non_comp$log_sla_cm2_g), x1 = 2.2+((ii-1)*.04), y1 = median(comp$log_sla_cm2_g), col = colors[ii], lwd = 4)
  
}

legend("topleft", bty ="n", pch = 21, pt.bg = colors[which_sp], cex = 2, pt.cex = 3.5, pt.lwd = 3, legend = toupper(unique(individual_traits$species)[which_sp]))
dev.off()

# LACA and EUPE and PLER and LOWR ----
png("~/grad/quals/presentation/img/ch2-prelim_figs/laca_eupe_pler_lowr.png", width = 1000, height = 1200)
par(oma = c(2,2.5,0,0))
plot(1, type = "n", xlim = c(0,3), ylim = c(1.9,2.7), xaxt = "n",xlab = "", ylab = "", cex.axis = 2.4)
mtext(side = 1, line = 4, cex = 3, "Competitive background")
mtext(side = 2, line = 3, cex = 3, expression(log[10]*"(Specific leaf area) (cm"^2*"/g)"))
axis(side = 1, at = c(1, 2.5), labels = c("No Competition", "Competition"), cex.axis = 2.4)

which_sp <- c(7, 5, 12, 9)
# for (ii in 1:length(unique(individual_traits$species))) {
for (ii in which_sp) {
  current_species = unique(individual_traits$species)[ii]
  comp <- individual_traits %>% filter(species == current_species) %>%
    filter(plot_type == "comp") %>% ungroup() %>%
    select(species, log_sla_cm2_g)
  non_comp <- individual_traits %>% filter(species == current_species) %>%
    filter(plot_type == "lambda") %>% ungroup() %>%
    select(species, log_sla_cm2_g)
  
  boxplot(non_comp$log_sla_cm2_g, at = .7+((ii-1)*.04), add = T, boxwex = .2, type = "n", yaxt = "n", col = colors[ii], bty = "n", boxlwd = 4, lwd = 3, cex = 2, pch = 19, outcol = colors[ii])
  boxplot(comp$log_sla_cm2_g, at = 2.2+((ii-1)*.04), add = T, boxwex = .2, type = "n", yaxt = "n", ylab = "", col = colors[ii], bty = "n", boxlwd = 4, lwd = 3, cex =2, pch = 19, outcol = colors[ii])
  
  segments(x0 =.7+((ii-1)*.04) , y0 = median(non_comp$log_sla_cm2_g), x1 = 2.2+((ii-1)*.04), y1 = median(comp$log_sla_cm2_g), col = "black", lwd = 8)
  segments(x0 =.7+((ii-1)*.04) , y0 = median(non_comp$log_sla_cm2_g), x1 = 2.2+((ii-1)*.04), y1 = median(comp$log_sla_cm2_g), col = colors[ii], lwd = 4)
  
}

legend("topleft", bty ="n", pch = 21, pt.bg = colors[which_sp], cex = 2, pt.cex = 3.5, pt.lwd = 3, legend = toupper(unique(individual_traits$species)[which_sp]))
dev.off()

# TODOS! ----
png("~/grad/quals/presentation/img/ch2-prelim_figs/allspecies.png", width = 1000, height = 1200)
par(oma = c(2,2.5,0,0))
plot(1, type = "n", xlim = c(0,3), ylim = c(1.9,2.7), xaxt = "n",xlab = "", ylab = "", cex.axis = 2.4)
mtext(side = 1, line = 4, cex = 3, "Competitive background")
mtext(side = 2, line = 3, cex = 3, expression(log[10]*"(Specific leaf area) (cm"^2*"/g)"))
axis(side = 1, at = c(1, 2.5), labels = c("No Competition", "Competition"), cex.axis = 2.4)

which_sp <- c(1:13)
# for (ii in 1:length(unique(individual_traits$species))) {
for (ii in which_sp) {
  current_species = unique(individual_traits$species)[ii]
  comp <- individual_traits %>% filter(species == current_species) %>%
    filter(plot_type == "comp") %>% ungroup() %>%
    select(species, log_sla_cm2_g)
  non_comp <- individual_traits %>% filter(species == current_species) %>%
    filter(plot_type == "lambda") %>% ungroup() %>%
    select(species, log_sla_cm2_g)
  
  boxplot(non_comp$log_sla_cm2_g, at = .7+((ii-1)*.04), add = T, boxwex = .2, type = "n", yaxt = "n", col = colors[ii], bty = "n", boxlwd = 4, lwd = 3, cex = 2, pch = 19, outcol = colors[ii])
  boxplot(comp$log_sla_cm2_g, at = 2.2+((ii-1)*.04), add = T, boxwex = .2, type = "n", yaxt = "n", ylab = "", col = colors[ii], bty = "n", boxlwd = 4, lwd = 3, cex =2, pch = 19, outcol = colors[ii])
  
  segments(x0 =.7+((ii-1)*.04) , y0 = median(non_comp$log_sla_cm2_g), x1 = 2.2+((ii-1)*.04), y1 = median(comp$log_sla_cm2_g), col = "black", lwd = 8)
  segments(x0 =.7+((ii-1)*.04) , y0 = median(non_comp$log_sla_cm2_g), x1 = 2.2+((ii-1)*.04), y1 = median(comp$log_sla_cm2_g), col = colors[ii], lwd = 4)
  
}

legend("topleft", bty ="n", pch = 21, pt.bg = colors[which_sp], cex = 2, pt.cex = 3.5, pt.lwd = 3, legend = toupper(unique(individual_traits$species)[which_sp]))
dev.off()
