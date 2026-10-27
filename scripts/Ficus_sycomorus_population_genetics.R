#==============================================================================#
# Ficus sycomorus domestication history study with ddRAD-seq data
#==============================================================================#



# R version used: 4.5.2


# Set working directory ----------------

workdir <- "path/to/working/directory/"
setwd(workdir)


# Load packages ----------------

library(dartR) # v2.1.11
library(rstatix) # v0.7.3
library(ggrepel) # v0.9.6
library(poppr) # v2.9.8
library(ape) #v5.8-1
library(magrittr) #v2.0.4
library(ggplot2) # v4.0.1
library(purrr) # v1.2.1
library(readr) # v2.1.6
library(rempsyc) # v0.2.0



# ---------------------------------------------------------------------------- #
# Import SNP data in VCF format
# ---------------------------------------------------------------------------- #



fsyc_gl_replicates <- gl.read.vcf("fsyc_snps_unique_samples_outgroups.vcf", verbose = T)
fsyc_gl_61inds <- gl.read.vcf("fsyc_snps_unique_samples_outgroups.vcf", verbose = T)



# ---------------------------------------------------------------------------- #
# Data with replicates : population info
# ---------------------------------------------------------------------------- #



# Assign populations ----------------

pop_replicates <- c("F.pet","F.pet","F.pet","F.pet","F.pet","F.pet","F.pet","AGN","AGN","AGN","AGN","AGN","AGN","AGN","AGN",
                    "ANF","F.car","F.car","AUB","AUB","AUB","AUB","CHE","CHE","COB","COB","COB","COB","COB","EG","EG","EPL",
                    "EPL","HAH","HAH","ISNI","ISNI","ISNI","ISNI","ISNI","ISTA","ISTA","ISTA","ISTA","ISTA","KE","KE","KE",
                    "LAR","LAR","LAR","LAR","LIK","LIK","LIK","LIK","LIK","LXR","LXR","LXR","LXR","LXR","M","M","M","M","M",
                    "M","M","M","M","M","M","M","MEB","MEN","MEN","MEN","MEN","MEN","MEN","MEN","MEN","MFS","MFS","MFS","MFS",
                    "MFS","SAW","SAW","SAT","SAT","SAW","SAW","SAW","SAW","SAW","SAW","SAW","SAW","SAT","SAT","SAT","SAT",
                    "SAT","SAT","SDN","SDN","SDN","SDN","SMS","SMS","SON","TRC","TRC","TRC")
cbind(pop_replicates,indNames(fsyc_gl_replicates))


# Country names ----------------

country_names <- pop_replicates
country_names[country_names == "F.pet"] <- "Mexico"
country_names[country_names == "AGN"] <- "Cyprus"
country_names[country_names == "ANF"] <- "Lebanon"
country_names[country_names == "F.car"] <- "Egypt"
country_names[country_names == "AUB"] <- "Lebanon"
country_names[country_names == "CHE"] <- "Lebanon"
country_names[country_names == "COB"] <- "Lebanon"
country_names[country_names == "EG"] <- "Egypt"
country_names[country_names == "EPL"] <- "Cyprus"
country_names[country_names == "HAH"] <- "Lebanon"
country_names[country_names == "ISNI"] <- "Israel"
country_names[country_names == "ISTA"] <- "Israel"
country_names[country_names == "KE"] <- "Kenya"
country_names[country_names == "LAR"] <- "Cyprus"
country_names[country_names == "LIK"] <- "Cyprus"
country_names[country_names == "LXR"] <- "Egypt"
country_names[country_names == "M"] <- "Egypt"
country_names[country_names == "MEB"] <- "Lebanon"
country_names[country_names == "MEN"] <- "Lebanon"
country_names[country_names == "MFS"] <- "Egypt"
country_names[country_names == "SAW"] <- "South Africa"
country_names[country_names == "SAT"] <- "South Africa"
country_names[country_names == "SDN"] <- "Sudan"
country_names[country_names == "SMS"] <- "Lebanon"
country_names[country_names == "SON"] <- "Sudan"
country_names[country_names == "TRC"] <- "Lebanon"

# change the sample from Israel that was a seed from Kenya (IS5 & IS5_r)
cbind(indNames(fsyc_gl_replicates),country_names)
country_names[41] <- "Kenya"
country_names[42] <- "Kenya"

# Check
cbind(indNames(fsyc_gl_replicates), country_names)

# Assign other information ----------------

repro_mode <- country_names
repro_mode[repro_mode == "Mexico"] <- "Reproductive"
repro_mode[repro_mode == "Lebanon"] <- "Seedless"
repro_mode[repro_mode == "Egypt"] <- "Seedless"
repro_mode[repro_mode == "Cyprus"] <- "Seedless"
repro_mode[repro_mode == "Israel"] <- "Seedless"
repro_mode[repro_mode == "Cyprus"] <- "Seedless"
repro_mode[repro_mode == "South Africa"] <- "Reproductive"
repro_mode[repro_mode == "Kenya"] <- "Reproductive"
repro_mode[repro_mode == "Sudan"] <- "Seedless"


# Assign pops
fsyc_gl_replicates$pop <- as.factor(pop_replicates)

# Set strata to genlight object
strata(fsyc_gl_replicates) <- data.frame(pop = pop(fsyc_gl_replicates), country = country_names, repro = repro_mode)

# Do we care about strata?
fsyc_gl_replicates_strata <- strata(fsyc_gl_replicates) %>%    
    group_by(pop, country, repro) %>%
    summarize(Count = n())
fsyc_gl_replicates_strata



# ---------------------------------------------------------------------------- #
# Data with 61 F. sycomorus, no replicates, outgroups: population info
# ---------------------------------------------------------------------------- #



# Assign populations ----------------

pop_61inds <- c("F.pet","F.pet","F.pet","F.pet","F.pet","F.pet","F.pet","AGN","AGN","AGN","EPL","LAR","LIK","LIK","ANF","AUB",
                "COB","COB","COB","CHE","HAH","SMS","MEB","MEN","MEN","MEN","MEN","TRC","ISNI","ISNI","ISNI","ISTA","ISTA",
                "ISTA","ISTA","KE","EG","LXR","LXR","LXR","M","M","M","M","M","M","M","M","M","M","M","M","MFS","MFS","MFS",
                "MFS","MFS","SDN","SON","SAW","SAW","SAT","SAW","SAW","SAW","SAT","SAT","SAT")
cbind(pop_61inds,indNames(fsyc_gl_61inds))


# Country names ----------------

country_names_61inds <- pop_61inds
country_names_61inds[country_names_61inds == "F.pet"] <- "Mexico"
country_names_61inds[country_names_61inds == "AGN"] <- "Cyprus"
country_names_61inds[country_names_61inds == "ANF"] <- "Lebanon"
country_names_61inds[country_names_61inds == "F.car"] <- "Egypt"
country_names_61inds[country_names_61inds == "AUB"] <- "Lebanon"
country_names_61inds[country_names_61inds == "CHE"] <- "Lebanon"
country_names_61inds[country_names_61inds == "COB"] <- "Lebanon"
country_names_61inds[country_names_61inds == "EG"] <- "Egypt"
country_names_61inds[country_names_61inds == "EPL"] <- "Cyprus"
country_names_61inds[country_names_61inds == "HAH"] <- "Lebanon"
country_names_61inds[country_names_61inds == "ISNI"] <- "Israel"
country_names_61inds[country_names_61inds == "ISTA"] <- "Israel"
country_names_61inds[country_names_61inds == "KE"] <- "Kenya"
country_names_61inds[country_names_61inds == "LAR"] <- "Cyprus"
country_names_61inds[country_names_61inds == "LIK"] <- "Cyprus"
country_names_61inds[country_names_61inds == "LXR"] <- "Egypt"
country_names_61inds[country_names_61inds == "M"] <- "Egypt"
country_names_61inds[country_names_61inds == "MEB"] <- "Lebanon"
country_names_61inds[country_names_61inds == "MEN"] <- "Lebanon"
country_names_61inds[country_names_61inds == "MFS"] <- "Egypt"
country_names_61inds[country_names_61inds == "SAW"] <- "South Africa"
country_names_61inds[country_names_61inds == "SAT"] <- "South Africa"
country_names_61inds[country_names_61inds == "SDN"] <- "Sudan"
country_names_61inds[country_names_61inds == "SMS"] <- "Lebanon"
country_names_61inds[country_names_61inds == "SON"] <- "Sudan"
country_names_61inds[country_names_61inds == "TRC"] <- "Lebanon"

cbind(indNames(fsyc_gl_61inds),country_names_61inds)
country_names_61inds[35] <- "Kenya" # This tree (IS5_r) is a seed from Kenya


# Assign other information ----------------

repro_mode_61inds <- country_names_61inds
repro_mode_61inds[repro_mode_61inds == "Mexico"] <- "Reproductive"
repro_mode_61inds[repro_mode_61inds == "Lebanon"] <- "Seedless"
repro_mode_61inds[repro_mode_61inds == "Egypt"] <- "Seedless"
repro_mode_61inds[repro_mode_61inds == "Cyprus"] <- "Seedless"
repro_mode_61inds[repro_mode_61inds == "Israel"] <- "Seedless"
repro_mode_61inds[repro_mode_61inds == "Cyprus"] <- "Seedless"
repro_mode_61inds[repro_mode_61inds == "South Africa"] <- "Reproductive"
repro_mode_61inds[repro_mode_61inds == "Kenya"] <- "Reproductive"
repro_mode_61inds[repro_mode_61inds == "Sudan"] <- "Seedless"


# Assign pops
fsyc_gl_61inds$pop <- as.factor(pop_61inds)

# Set strata to genlight object
strata(fsyc_gl_61inds) <- data.frame(pop = pop(fsyc_gl_61inds), country = country_names_61inds, repro = repro_mode_61inds)

fsyc_gl_61inds_strata <- strata(fsyc_gl_61inds) %>%    
  group_by(pop, country, repro) %>%
  summarize(Count = n())
fsyc_gl_61inds_strata



# ---------------------------------------------------------------------------- #
# Data curation
# ---------------------------------------------------------------------------- #



# Remove loci without data ----------------

fsyc_gl_replicates_filt <- gl.filter.allna(fsyc_gl_replicates)
glPlot(fsyc_gl_replicates_filt)
# Here outgroups have very few data, they will be removed

fsyc_gl_61inds_filt <- gl.filter.allna(fsyc_gl_61inds)
glPlot(fsyc_gl_61inds_filt)
# This data set contains outgroups from Mexico with more data


# Remove outgroups ----------------

# Outgroups will be used in another phylogenetic analysis only for the "no replicates" SNP file
fsyc_gl_replicates_filt <- fsyc_gl_replicates_filt[!indNames(fsyc_gl_replicates_filt) %in% c("95-12B","95-18","95-200","95-201","95-202","95-53","95-59","ASW1","ASW2")]
fsyc_gl_61inds_filt <- fsyc_gl_61inds_filt[!indNames(fsyc_gl_61inds_filt) %in% c("95-12B","95-18","95-200","95-201","95-202","95-53","95-59","ASW1","ASW2")]

# Filter loci ----------------

# Call rate
gl.report.callrate(fsyc_gl_replicates_filt)
fsyc_gl_replicates_filt <- gl.filter.callrate(fsyc_gl_replicates_filt, method="loc", threshold=0.5)

gl.report.callrate(fsyc_gl_61inds_filt)
fsyc_gl_61inds_filt <- gl.filter.callrate(fsyc_gl_61inds_filt, method="loc", threshold=0.5)

# Monomorphic loci
gl.report.monomorphs(fsyc_gl_replicates_filt)
fsyc_gl_replicates_filt <- gl.filter.monomorphs(fsyc_gl_replicates_filt)

gl.report.monomorphs(fsyc_gl_61inds_filt)
fsyc_gl_61inds_filt <- gl.filter.monomorphs(fsyc_gl_61inds_filt)


# Remove individuals with low sequencing ----------------

# Filter individuals with missing data
glPlot(fsyc_gl_replicates_filt[indNames(fsyc_gl_replicates_filt) %in% c("LAR1A","LAR1B")]) # Look at some specific individuals
fsyc_gl_replicates_filt <- gl.filter.callrate(fsyc_gl_replicates_filt, method="ind", threshold=0.3)
# This removes LAR1B[LAR], SA9[SAT], SA9_r[SAT]

fsyc_gl_61inds_filt <- gl.filter.callrate(fsyc_gl_61inds_filt, method="ind", threshold=0.3)
# SA9_r removed, kept in phylogenetic analysis


# Curated data sets ----------------

fsyc_gl_replicates_analysis <- fsyc_gl_replicates_filt
fsyc_gl_unique_samples_analysis <- fsyc_gl_61inds_filt



# ---------------------------------------------------------------------------- #
# PCA
# ---------------------------------------------------------------------------- #



# Color palette for country of sample origin ----------------

country_pal <- c("black","magenta3","royalblue4","darkorange1","springgreen","steelblue","firebrick1","grey")
country_pal <-  setNames(country_pal, unique(country_names))


# PCA ----------------

fsyc_gl_analysis.pca <- glPca(fsyc_gl_unique_samples_analysis) 
5 
fsyc_gl_analysis.pca.scores <- as.data.frame(fsyc_gl_analysis.pca$scores)
fsyc_gl_analysis.pca.scores$pop_names <- fsyc_gl_unique_samples_analysis$strata$country
fsyc_gl_analysis.pca.scores$pop_names <- factor(fsyc_gl_analysis.pca.scores$pop_names, levels=c("Mexico","Cyprus","Lebanon","Israel","Algeria","Egypt","Sudan","Kenya","South Africa"))
fsyc_gl_analysis.pca.scores$ID <- fsyc_gl_unique_samples_analysis$ind.names

# Variance 
var.pca1 <- fsyc_gl_analysis.pca$eig[1]/sum(fsyc_gl_analysis.pca$eig) # proportion of variation explained by 1st axis
var.pca2 <- fsyc_gl_analysis.pca$eig[2]/sum(fsyc_gl_analysis.pca$eig) # proportion of variation explained by 2nd axis
var.pca3 <- fsyc_gl_analysis.pca$eig[3]/sum(fsyc_gl_analysis.pca$eig) # proportion of variation explained by 3rd axis


# Plots ----------------

ggplot(fsyc_gl_analysis.pca.scores, aes(x=PC1, y=PC2,order=rev(as.numeric(factor(fsyc_gl_analysis.pca.scores$pop_names)))))  + 
  geom_jitter(aes(fill=factor(pop_names)), size = 3.7, shape = 21, stroke = 1, colour = "black",width = 0.4, height = 0.4) +
  geom_hline(yintercept=0,linetype=2) + 
  geom_vline(xintercept=0,linetype=2) +
  scale_fill_manual(name="Regions", values=country_pal) +
  theme_classic(base_size = 16) +
  theme(plot.margin = margin(15, 10, 10, 10, "pt"),
        axis.text.y=element_text(colour="black", angle = 90)) +
  xlab(paste0("Principal component 1 (",round(var.pca1*100, digits = 2),"%)")) +
  ylab(paste0("Principal component 2 (",round(var.pca2*100, digits = 2),"%)"))

ggsave("figure_4a.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 9, height = 7, units = "in", dpi = 600, limitsize=TRUE)


ggplot(fsyc_gl_analysis.pca.scores, aes(x=PC1, y=PC3))  + 
  geom_jitter(aes(fill=factor(pop_names)), size = 3.7, shape = 21, stroke = 1, colour = "black",width = 0.4, height = 0.4) +
  geom_hline(yintercept=0,linetype=2) + 
  geom_vline(xintercept=0,linetype=2) +
  scale_fill_manual(name="Regions", values=country_pal) +
  theme_classic(base_size = 16) +
  theme(plot.margin = margin(15, 10, 10, 10, "pt"),
        axis.text.y=element_text(colour="black", angle = 90)) +
  xlab(paste0("Principal component 1 (",round(var.pca1*100, digits = 2),"%)")) +
  ylab(paste0("Principal component 3 (",round(var.pca3*100, digits = 2),"%)"))

ggsave("figure_4b.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 9, height = 7, units = "in", dpi = 600, limitsize=TRUE)


# For visualization of the individual names
ggplot(fsyc_gl_analysis.pca.scores, aes(x=PC1, y=PC2,order=rev(as.numeric(factor(fsyc_gl_analysis.pca.scores$pop_names)))))  + 
  geom_point(aes(color=factor(pop_names)), size = 3, shape = 1, stroke = 1.5) + 
  geom_text_repel(aes(label = rownames(fsyc_gl_analysis.pca.scores)), size = 2, max.overlaps = 60) +
  geom_hline(yintercept=0,linetype=2) + 
  geom_vline(xintercept=0,linetype=2) +
  scale_fill_manual(name="Regions", values=country_pal) +
  theme_classic(base_size = 16) +
  theme(plot.margin = margin(15, 10, 10, 10, "pt"),
        axis.text.y=element_text(colour="black", angle = 90)) +
  xlab(paste0("Principal component 1 (",round(var.pca1*100, digits = 2),"%)")) +
  ylab(paste0("Principal component 2 (",round(var.pca2*100, digits = 2),"%)"))



# ---------------------------------------------------------------------------- #
# Re-define genetic clusters and regional clusters (phylogroups)
# ---------------------------------------------------------------------------- #



# Based on PCA results, we can re-attribute populations to some samples
phylogroups <- pop(fsyc_gl_unique_samples_analysis)

# What I mean by phylogroup is a regional group
levels(phylogroups) <- c(levels(phylogroups),"Sub-Sahara","South Africa", "Kenya","South Egypt-Sudan","North Egypt","Algeria","Levant","Cyprus") # change to "" (South Africa and Kenya) if wanted
phylogroups[phylogroups == "SAW"] <- "South Africa" # change to "Sub-Sahara" if wanted
phylogroups[phylogroups == "SAT"] <- "South Africa" # change to "Sub-Sahara" if wanted
phylogroups[phylogroups == "HAH"] <- "Levant"
phylogroups[phylogroups == "ANF"] <- "Levant"
phylogroups[phylogroups == "AUB"] <- "Levant"
phylogroups[phylogroups == "CHE"] <- "Levant"
phylogroups[phylogroups == "COB"] <- "Levant"
phylogroups[phylogroups == "MEB"] <- "Levant"
phylogroups[phylogroups == "MEN"] <- "Levant"
phylogroups[phylogroups == "SMS"] <- "Levant"
phylogroups[phylogroups == "AGN"] <- "Cyprus" # Cyprus samples seem to have a different origin
phylogroups[phylogroups == "LAR"] <- "Cyprus"
phylogroups[phylogroups == "LIK"] <- "Cyprus"
phylogroups[phylogroups == "EPL"] <- "Cyprus"
phylogroups[phylogroups == "LXR"] <- "South Egypt-Sudan" # Sudan and south Egypt samples are in the same cluster
phylogroups[phylogroups == "EG"] <- "South Egypt-Sudan"
phylogroups[phylogroups == "SDN"] <- "South Egypt-Sudan"
phylogroups[phylogroups == "SON"] <- "South Egypt-Sudan"
phylogroups[phylogroups == "MFS"] <- "North Egypt"
phylogroups[phylogroups == "M"] <- "North Egypt"
phylogroups[phylogroups == "ISNI"] <- "Levant"
phylogroups[phylogroups == "ISTA"] <- "Levant"
phylogroups[phylogroups == "KE"] <- "Kenya" # change to "Sub-Sahara" if wanted
phylogroups[phylogroups == "TRC"] <- "Levant"
cbind(indNames(fsyc_gl_unique_samples_analysis),as.vector(phylogroups))
phylogroups[28] <- "Kenya" # We change the seed that comes from Kenya (IS5_r), change to "Sub-Sahara" if wanted
# phylogroups[20] <- "North Egypt" # MEN03 This tree is in Levant but clusters with North Egypt (but will be assigned to geno_clust)

cbind(indNames(fsyc_gl_unique_samples_analysis),as.vector(phylogroups))
# Check if IS5_r and MEN03 are assigned correcly? Good!
# EDIT: we don't assign MEN03 here anymore but in geno_clust

# We create another object where Cyprus samples (and MEN03) are correctly assigned to their genetic clusters
# These are:
# - AGN1B,AGN2B,AGN3A,LAR1C with Levant
# - EPL1B,LIK1B with North Egypt
# - LIK2A with South Egypt-Sudan

geno_clust <- phylogroups
geno_clust[20] <- "North Egypt" # Here we change MEN03 to the North Egypt cluster
cbind(indNames(fsyc_gl_unique_samples_analysis),as.vector(geno_clust))
geno_clust[c(1:3,5)] <- "Levant"
geno_clust[c(4,6)] <- "North Egypt"
geno_clust[7] <- "South Egypt-Sudan"


# Add to gl object
strata(fsyc_gl_unique_samples_analysis) <- data.frame(pop = pop(fsyc_gl_unique_samples_analysis), phylo = phylogroups, geno_clust = geno_clust, repro = fsyc_gl_unique_samples_analysis$strata$repro, country = fsyc_gl_unique_samples_analysis$strata$country )
cbind(indNames(fsyc_gl_unique_samples_analysis),strata(fsyc_gl_unique_samples_analysis))


# Palettes ----------------

# Country palette
country_pal <- c("black","magenta3","royalblue4","darkorange1","springgreen","steelblue","firebrick1","grey")
country_pal <-  setNames(country_pal, unique(country_names))
# Done previously but redone if PCA skipped

country_slices <- rep(1,length(country_pal))
names(country_slices) <- names(country_pal)    
pie(country_slices, col=country_pal)

# Phylogroup palette
phylo_pal <- c("magenta3","turquoise","steelblue","darkgreen","gold1","firebrick1") #darkorange1 
phylo_pal <-  setNames(phylo_pal, unique(phylogroups))

phylo_slices <- rep(1,length(phylo_pal))
names(phylo_slices) <- names(phylo_pal)    
pie(phylo_slices, col=phylo_pal)

# Genetic cluster palette
geno_clust_pal <- c("turquoise","gold1","darkgreen","steelblue","firebrick1")
geno_clust_pal <-  setNames(geno_clust_pal, unique(geno_clust))

geno_clust_slices <- rep(1,length(geno_clust_pal))
names(geno_clust_slices) <- names(geno_clust_pal)    
pie(geno_clust_slices, col=geno_clust_pal)

# Save palettes for phylogenetic trees
saveRDS(country_pal,"country_pal.RDS")
saveRDS(phylo_pal,"phylo_pal.RDS")
saveRDS(geno_clust_pal,"geno_clust_pal.RDS")

# Set populations to analyse
pop(fsyc_gl_unique_samples_analysis) <- fsyc_gl_unique_samples_analysis$strata$phylo
# If I want to do population genetics comparing geographic regions, I need to change this to "country" or create a new vector "region".
# We will mainly study population genetics among regional groups and genetic clusters, not countries.



# ---------------------------------------------------------------------------- #
# Distances
# ---------------------------------------------------------------------------- #



# Euclidean distances among individuals ----------------

fsyc_dist_inds <- gl.dist.ind(fsyc_gl_unique_samples_analysis, method='euclidean',scale=FALSE)
fsyc_dist_inds_mat <- as.matrix(fsyc_dist_inds)
fsyc_dist_inds_mat[upper.tri(fsyc_dist_inds_mat)] <- NA

# Export for supplementary table
write.csv(fsyc_dist_inds_mat,"Fsyc_individuals_euclidean_genetic_distance_matrix.csv")
write.table(fsyc_dist_inds_mat,"Fsyc_individuals_euclidean_genetic_distance_matrix.txt", sep = "\t" )

# Now we measure genomic distance among individuals in phylogroups
fsyc_dist_inds_SA <- gl.dist.ind(fsyc_gl_unique_samples_analysis[fsyc_gl_unique_samples_analysis$strata$phylo == "South Africa"], method='euclidean',scale=FALSE)
fsyc_dist_inds_Ken <- gl.dist.ind(fsyc_gl_unique_samples_analysis[fsyc_gl_unique_samples_analysis$strata$phylo == "Kenya"], method='euclidean',scale=FALSE)
fsyc_dist_inds_EgC <- gl.dist.ind(fsyc_gl_unique_samples_analysis[fsyc_gl_unique_samples_analysis$strata$phylo == "North Egypt"], method='euclidean',scale=FALSE)
fsyc_dist_inds_SEg <- gl.dist.ind(fsyc_gl_unique_samples_analysis[fsyc_gl_unique_samples_analysis$strata$phylo == "South Egypt-Sudan"|fsyc_gl_unique_samples_analysis$pop == "South Egypt"], method='euclidean',scale=FALSE)
fsyc_dist_inds_Cyp <- gl.dist.ind(fsyc_gl_unique_samples_analysis[fsyc_gl_unique_samples_analysis$strata$phylo == "Cyprus"], method='euclidean',scale=FALSE)
fsyc_dist_inds_Lev <- gl.dist.ind(fsyc_gl_unique_samples_analysis[fsyc_gl_unique_samples_analysis$strata$phylo == "Levant"], method='euclidean',scale=FALSE)

# Among populations
fsyc_dist_pop <- gl.dist.pop(fsyc_gl_unique_samples_analysis, method='euclidean',scale=FALSE)
fsyc_dist_pop
write.csv(fsyc_dist_pop, "fsyc_dist_pop.csv")


# Summaries (to report in the manuscript) ----------------

mean(fsyc_dist_inds_SA)
sd(fsyc_dist_inds_SA)
mean(fsyc_dist_inds_Ken)
sd(fsyc_dist_inds_Ken)
mean(fsyc_dist_inds_SEg)
sd(fsyc_dist_inds_SEg)
mean(fsyc_dist_inds_EgC)
sd(fsyc_dist_inds_EgC)
mean(fsyc_dist_inds_Lev)
sd(fsyc_dist_inds_Lev) 
mean(fsyc_dist_inds_Cyp)
sd(fsyc_dist_inds_Cyp)


# Make a table ----------------

dist_mean_sd_df <- data.frame("Cyprus" = c(mean(fsyc_dist_inds_Cyp),sd(fsyc_dist_inds_Cyp)),
                              "Levant" = c(mean(fsyc_dist_inds_Lev),sd(fsyc_dist_inds_Lev)),
                              "North Egypt" = c(mean(fsyc_dist_inds_EgC),sd(fsyc_dist_inds_EgC)),
                              "South Egypt-Sudan" = c(mean(fsyc_dist_inds_SEg),sd(fsyc_dist_inds_SEg)),
                              "Kenya" = c(mean(fsyc_dist_inds_Ken),sd(fsyc_dist_inds_Ken)),
                              "South Africa" = c(mean(fsyc_dist_inds_SA),sd(fsyc_dist_inds_SA)), check.names=FALSE)
rownames(dist_mean_sd_df) <- c("Mean","Standard deviation")
t(dist_mean_sd_df)
write.csv(t(dist_mean_sd_df), "fsyc_dist_mean_sd.csv")



# ---------------------------------------------------------------------------- #
# Create clone/replicate lists & genetic distance graph
# ---------------------------------------------------------------------------- #



# This function allows to transform a distance matrix ("dist" class) into a tree columns matrix (pairs and their value)
distListFun <- function(inDist) {
  if (class(inDist) != "dist") stop("wrong input type")
  A <- attr(inDist, "Size")
  B <- if (is.null(attr(inDist, "Labels"))) sequence(A) else attr(inDist, "Labels")
  if (isTRUE(attr(inDist, "Diag"))) attr(inDist, "Diag") <- FALSE
  if (isTRUE(attr(inDist, "Upper"))) attr(inDist, "Upper") <- FALSE
  data.frame(
    row = B[unlist(lapply(sequence(A)[-1], function(x) x:A))],
    col = rep(B[-length(B)], (length(B)-1):1),
    value = as.vector(inDist))
}


# Compare distance calculation methods ----------------

# dist()
x.mat <- as.data.frame(as.matrix(fsyc_gl_unique_samples_analysis))
rownames(x.mat)
dist_distances <- dist(as.matrix(x.mat))
head(as.matrix(dist_distances))

# gl.dist.ind()
gl.dist.ind_distances <- gl.dist.ind(fsyc_gl_unique_samples_analysis, method='euclidean',scale=FALSE)
head(as.matrix(gl.dist.ind_distances))

# bitwise.dist() the snpclone object is computed later in the script
bitwise.dist_distances <- bitwise.dist(fsyc_gl_unique_samples_analysis, euclidean = TRUE)
head(as.matrix(bitwise.dist_distances))

# dist() is to be avoided, both bitwise.dist and gl.dist.ind() calculates distances similarly (NAs are treated the same way)
rm(dist_distances, gl.dist.ind_distances, bitwise.dist_distances)

# Remove one problematic sample
fsyc_gl_replicates_analysis <- fsyc_gl_replicates_analysis[!indNames(fsyc_gl_replicates_analysis) %in% c("LIK2B")]
fsyc_gl_replicates_analysis
# This sample has it's replicate very distant. These have a too large genetic distance, it's probably not the same tree


# Compute distances ----------------

# Distance list of non duplicates
dis <- gl.dist.ind(fsyc_gl_unique_samples_analysis, method='euclidean',scale=FALSE)
dislist <- distListFun(dis)
# View(dislist)
hist(dislist$value, breaks = 100)


# # Distance list of duplicates 
disclones <- gl.dist.ind(fsyc_gl_replicates_analysis, method='euclidean',scale=FALSE)
discloneslist <- distListFun(disclones)
# View(discloneslist)

realclone <- subset(discloneslist, col == "AGN1A" & row == "AGN1B" | 
                      col == "AGN1A_r" & row == "AGN1B_r" | 
                      col == "AGN3A" & row == "AGN3B" | 
                      col == "AGN1A" & row == "AGN1B" | 
                      col == "AUB01A_1" & row == "AUB01B_1" | 
                      col == "AUB01A_2" & row == "AUB01B_2" | 
                      col == "COB2A" & row == "COB2B" | 
                      col == "COB3A" & row == "COB3B" | 
                      col == "KE1-1" & row == "KE1-2" |
                      col == "LAR1A" & row == "LAR1B" |
                      col == "LAR1A" & row == "LAR1C" |
                      col == "LIK1A" & row == "LIK1B" | 
                      col == "LXR1A" & row == "LXR1B_1" |
                      col == "LXR3A" & row == "LXR3B" |
                      col == "MEN01A_1" & row == "MEN01B_1" |
                      col == "SDN2A_1" & row == "SDN2B_1" |
                      col == "SDN2A_2" & row == "SDN2B_2" |
                      col == "SDN2B_1" & row == "SDN2B_2")

replicates <- subset(discloneslist, col == "AGN1A" & row == "AGN1A_r" | 
                       col == "AGN1B" & row == "AGN1B_r" |
                       col == "AUB01A_1" & row == "AUB01A_2" | 
                       col == "AUB01B_1" & row == "AUB01B_2" | 
                       col == "CHE01_1" & row == "CHE01_2" | 
                       col == "EG1" & row == "EG1_r" |
                       col == "HAH01_1" & row == "HAH01_2" |
                       col == "IS1" & row == "IS1_r" |
                       col == "IS3" & row == "IS3_r" |
                       col == "IS5" & row == "IS5_r" |
                       col == "KE1-2" & row == "KE1-2_r" |
                       col == "LAR1B" & row == "LAR1B_2" |
                       col == "LIK1A" & row == "LIK1A_r" |
                       col == "MEN01B_1" & row == "MEN01B_2" |
                       col == "MEN02_1" & row == "MEN02_2" |
                       col == "MEN04_1" & row == "MEN04_2" |
                       col == "SA1" & row == "SA1_r" |
                       col == "SA10" & row == "SA10_r" |
                       col == "SA2" & row == "SA2_r" |
                       col == "SA3" & row == "SA3_r" |
                       col == "SA4" & row == "SA4_r" |
                       col == "SA5" & row == "SA5_r" |
                       col == "SA7" & row == "SA7_r" |
                       col == "SA8" & row == "SA8_r" |
                       col == "SA9" & row == "SA9_r" |
                       col == "SDN2A_1" & row == "SDN2A_2" |
                       col == "SDN2B_1" & row == "SDN2B_2" |
                       col == "SMS01_1" & row == "SMS01_2" |
                       col == "TRC01" & row == "TRC01_1" |
                       col == "TRC01" & row == "TRC01_2")


# Format ----------------

realclonedist <- realclone$value
replicatesdist <- replicates$value
disgg <- as.data.frame(dislist$value)
clonegg <- as.data.frame(realclonedist)
replicatesgg <- as.data.frame(replicatesdist)
disgg$data <- "different tree"
clonegg$data <- "identical tree"
replicatesgg$data <- "pcr replicates"
colnames(disgg) <- c("dist", "data")
colnames(clonegg) <- c("dist", "data")
colnames(replicatesgg) <- c("dist", "data")
all_dist <- rbind(clonegg,replicatesgg,disgg)
all_dist$data <- as.factor(all_dist$data )
all_dist$data<- relevel(all_dist$data, "different tree")


# Plot ----------------

ggplot(all_dist) + 
  geom_histogram(aes(dist, fill=data), binwidth=.2, position="identity", color="black",alpha=0.8) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 15), 
        axis.text.y = element_text(angle = 90, size = 15), 
        plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'), 
        legend.position=c(.15, .9), 
        axis.title.x = element_text(colour="black",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="black",size=15,angle=90,hjust=.5,vjust=.5,face="plain"),
        legend.title=element_text(size=13), 
        legend.text=element_text(size=11)) + 
  labs(title = NULL, x = "Euclidian genetic distance",  y = "Count") +
  geom_vline(xintercept = 20, color = "red", size = 2, alpha = 0.5, lty = 2) +
  scale_fill_manual(values = c("lightsteelblue","magenta","green"), name = NULL, breaks=c("different tree", "identical tree", "pcr replicates"), labels=c("Non-replicates", "Sample replicates", "PCR replicates")) + # , name="Euclidian genetic distance between pairs"
  scale_y_continuous(expand = c(0,0), limits = c(0, 60)) 

ggsave("figure_5a.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 9, height = 6, units = "in", dpi = 600, limitsize=TRUE)



# ---------------------------------------------------------------------------- #
# Calculate the error rate among clones, replicates and putative clonal populations
# ---------------------------------------------------------------------------- #



# Replicates (PCR replicates) ----------------

# From: https://groups.google.com/g/dartr/c/9b9jgw3CXp0
errorrate_replicates <- tibble(as.data.frame(replicates[,1:2]))

for (i in 1:nrow(errorrate_replicates)) {
  replicate_pair_gl <- gl.keep.ind(fsyc_gl_replicates_analysis, ind.list = c(errorrate_replicates[[i, 1]], errorrate_replicates[[i, 2]]))
  tmp <- as.matrix(replicate_pair_gl)
  mismatches <- sum(tmp[1, ] != tmp[2, ], na.rm = TRUE) # Count positions where genotypes differ
  total_loci <- ncol(tmp) # Total number of loci
  errorrate_replicates[[i, "Error Rate (%)"]] <- (mismatches / total_loci) * 100
}

# View(errorrate)
mean(errorrate_replicates$`Error Rate (%)`)
sd(errorrate_replicates$`Error Rate (%)`)


# Clones (samples from the same tree, two leaves) ----------------

errorrate_clones <- tibble(as.data.frame(realclone[,1:2]))

for (i in 1:nrow(errorrate_clones)) {
  replicate_pair_gl <- gl.keep.ind(fsyc_gl_replicates_analysis, ind.list = c(errorrate_clones[[i, 1]], errorrate_clones[[i, 2]]))
  tmp <- as.matrix(replicate_pair_gl)
  mismatches <- sum(tmp[1, ] != tmp[2, ], na.rm = TRUE) # Count positions where genotypes differ
  total_loci <- ncol(tmp) # Total number of loci
  errorrate_clones[[i, "Error Rate (%)"]] <- (mismatches / total_loci) * 100
}

# View(errorrate)
mean(errorrate_clones$`Error Rate (%)`)
sd(errorrate_clones$`Error Rate (%)`)


# All replicates ----------------

errorrate_all_replicates <- rbind(errorrate_replicates,errorrate_clones)

for (i in 1:nrow(errorrate_all_replicates)) {
  replicate_pair_gl <- gl.keep.ind(fsyc_gl_replicates_analysis, ind.list = c(errorrate_all_replicates[[i, 1]], errorrate_all_replicates[[i, 2]]))
  tmp <- as.matrix(replicate_pair_gl)
  mismatches <- sum(tmp[1, ] != tmp[2, ], na.rm = TRUE) # Count positions where genotypes differ
  total_loci <- ncol(tmp) # Total number of loci
  errorrate_all_replicates[[i, "Error Rate (%)"]] <- (mismatches / total_loci) * 100
}

# View(errorrate_all_replicates)
mean(errorrate_all_replicates$`Error Rate (%)`)
sd(errorrate_all_replicates$`Error Rate (%)`)


# Clonal populations (genomic clusters) ----------------

cbind(indNames(fsyc_gl_unique_samples_analysis),strata(fsyc_gl_unique_samples_analysis)["geno_clust"])

# Levant
errorrate_Levant<- tibble(as.data.frame(t(combn(c("ANF01","AUB01B_2","COB1B","COB2A","COB3B","CHE01_2",
                                                  "HAH01_1","SMS01_1","MEB01B","MEN01A_1","MEN04_1","MEN02_2",
                                                  "TRC01","IS1","IS2","IS3","Israel1","Israel2","Israel4"), 2)))) # Remember "MEN03" is not in this genetic cluster

for (i in 1:nrow(errorrate_Levant)) {
  replicate_pair_gl <- gl.keep.ind(fsyc_gl_replicates_analysis, ind.list = c(errorrate_Levant[[i, 1]], errorrate_Levant[[i, 2]]))
  tmp <- as.matrix(replicate_pair_gl)
  mismatches <- sum(tmp[1, ] != tmp[2, ], na.rm = TRUE) # Count positions where genotypes differ
  total_loci <- ncol(tmp) # Total number of loci
  errorrate_Levant[[i, "Error Rate (%)"]] <- (mismatches / total_loci) * 100
}

# View(errorrate_Levant)
mean(errorrate_Levant$`Error Rate (%)`)
sd(errorrate_Levant$`Error Rate (%)`)

# North Egypt
errorrate_NE<- tibble(as.data.frame(t(combn(c("M10","M11","M12","M13","M6","M7","M8","M14","M2","M3","M4","M5_2",
                                              "MFS27","MFS28","MFS29","MFS20","MFS26","MEN03","LIK1B","EPL1B"), 2)))) # Don't forget MEN03

for (i in 1:nrow(errorrate_NE)) {
  replicate_pair_gl <- gl.keep.ind(fsyc_gl_replicates_analysis, ind.list = c(errorrate_NE[[i, 1]], errorrate_NE[[i, 2]]))
  tmp <- as.matrix(replicate_pair_gl)
  mismatches <- sum(tmp[1, ] != tmp[2, ], na.rm = TRUE) # Count positions where genotypes differ
  total_loci <- ncol(tmp) # Total number of loci
  errorrate_NE[[i, "Error Rate (%)"]] <- (mismatches / total_loci) * 100
}

# View(errorrate_Levant)
mean(errorrate_NE$`Error Rate (%)`)
sd(errorrate_NE$`Error Rate (%)`)

# South Egypt-Sudan
errorrate_SES<- tibble(as.data.frame(t(combn(c("SDN2A_1","SON001","EG1","LXR3A","LXR1B_1","LXR2B","LIK2A"),2))))

for (i in 1:nrow(errorrate_SES)) {
  replicate_pair_gl <- gl.keep.ind(fsyc_gl_replicates_analysis, ind.list = c(errorrate_SES[[i, 1]], errorrate_SES[[i, 2]]))
  tmp <- as.matrix(replicate_pair_gl)
  mismatches <- sum(tmp[1, ] != tmp[2, ], na.rm = TRUE) # Count positions where genotypes differ
  total_loci <- ncol(tmp) # Total number of loci
  errorrate_SES[[i, "Error Rate (%)"]] <- (mismatches / total_loci) * 100
}

# View(errorrate_SES)
mean(errorrate_SES$`Error Rate (%)`)
sd(errorrate_SES$`Error Rate (%)`)

# Kenya
errorrate_Ke<- tibble(as.data.frame(t(combn(c("IS5_r","KE1-1"),2))))

for (i in 1:nrow(errorrate_Ke)) {
  replicate_pair_gl <- gl.keep.ind(fsyc_gl_replicates_analysis, ind.list = c(errorrate_Ke[[i, 1]], errorrate_Ke[[i, 2]]))
  tmp <- as.matrix(replicate_pair_gl)
  mismatches <- sum(tmp[1, ] != tmp[2, ], na.rm = TRUE) # Count positions where genotypes differ
  total_loci <- ncol(tmp) # Total number of loci
  errorrate_Ke[[i, "Error Rate (%)"]] <- (mismatches / total_loci) * 100
}

# View(errorrate_Ke)
mean(errorrate_Ke$`Error Rate (%)`)
sd(errorrate_Ke$`Error Rate (%)`)

# South Africa
errorrate_SA<- tibble(as.data.frame(t(combn(c("SA4_r","SA5","SA10","SA2","SA3","SA1","SA8","SA7"),2))))

for (i in 1:nrow(errorrate_SA)) {
  replicate_pair_gl <- gl.keep.ind(fsyc_gl_replicates_analysis, ind.list = c(errorrate_SA[[i, 1]], errorrate_SA[[i, 2]]))
  tmp <- as.matrix(replicate_pair_gl)
  mismatches <- sum(tmp[1, ] != tmp[2, ], na.rm = TRUE) # Count positions where genotypes differ
  total_loci <- ncol(tmp) # Total number of loci
  errorrate_SA[[i, "Error Rate (%)"]] <- (mismatches / total_loci) * 100
}

# View(errorrate_SA)
mean(errorrate_SA$`Error Rate (%)`)
sd(errorrate_SA$`Error Rate (%)`)



# Statistical tests - unpaired sample T-tests ----------------

# Summary table
errorrate_df_summary <- tibble(group = c("All replicates","PCR replicates","Sample replicates","Levant","North Egypt","South Egypt-Sudan","Kenya", "South Africa"),
                           means = c(mean(errorrate_all_replicates$`Error Rate (%)`),mean(errorrate_replicates$`Error Rate (%)`),mean(errorrate_clones$`Error Rate (%)`),mean(errorrate_Levant$`Error Rate (%)`),
                                     mean(errorrate_NE$`Error Rate (%)`),mean(errorrate_SES$`Error Rate (%)`),mean(errorrate_Ke$`Error Rate (%)`),mean(errorrate_SA$`Error Rate (%)`)),
                           sd = c(sd(errorrate_all_replicates$`Error Rate (%)`),sd(errorrate_replicates$`Error Rate (%)`),sd(errorrate_clones$`Error Rate (%)`),sd(errorrate_Levant$`Error Rate (%)`),
                                  sd(errorrate_NE$`Error Rate (%)`),sd(errorrate_SES$`Error Rate (%)`),sd(errorrate_Ke$`Error Rate (%)`),sd(errorrate_SA$`Error Rate (%)`)))
write.csv(errorrate_df_summary, "errorrates_summaries.csv")

# Create table
errorrate_all_replicates$group <- "All replicates"
errorrate_replicates$group <- "PCR replicates"
errorrate_clones$group <- "Sample replicates"
errorrate_Levant$group <- "Levant"
errorrate_NE$group <- "North Egypt"
errorrate_SES$group <- "South Egypt-Sudan"
errorrate_Ke$group <- "Kenya"
errorrate_SA$group <- "South Africa"

errorrate_df <- rbind(errorrate_all_replicates[,3:4],errorrate_replicates[,3:4],errorrate_clones[,3:4],errorrate_Levant[,3:4],errorrate_NE[,3:4],
                      errorrate_SES[,3:4],errorrate_Ke[,3:4],errorrate_SA[,3:4])
colnames(errorrate_df) <- c("errorrate","group")
errorrate_df$group <- factor(errorrate_df$group,levels =c("All replicates","PCR replicates","Sample replicates","Levant","North Egypt","South Egypt-Sudan","Kenya","South Africa"))

# Plot
ggplot(errorrate_df, aes(x=group, y=errorrate)) +
  geom_boxplot(fill='white', color="black")+
  theme_classic() +
  theme(axis.text.x = element_text(color="black",angle = 90, vjust = 0.3, hjust = 1)) +
  xlab(NULL) +
  ylab("Error Rate")
ggsave("figure_S4.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 5, height = 4, units = c("in"), dpi = 600, limitsize = TRUE)

# T test
fsyc_errorrate_t_test <- t_test(data = errorrate_df,formula = errorrate ~ group, 
  comparisons = list(c("All replicates","Levant"), c("All replicates","North Egypt"),c("All replicates", "South Egypt-Sudan"), 
                     c("All replicates","South Africa"))) # , c("All replicates","Kenya")
write.csv(fsyc_errorrate_t_test, "fsyc_errorrate_t_test.csv")

# ANOVA
fsyc_errorrate_anova <- anova_test(data = subset(subset(errorrate_df, group !="Kenya"), group != "South Africa"), errorrate ~ group)
write.csv(fsyc_errorrate_anova, "fsyc_errorrate_anova.csv")



# ---------------------------------------------------------------------------- #
# Population genetics: Heterozygosity, FIS, LD, FST and AMOVA
# ---------------------------------------------------------------------------- #



# We assign the strata "genetic cluster" to "population in the gl object
fsyc_gl_unique_samples_analysis_clust <- fsyc_gl_unique_samples_analysis
pop(fsyc_gl_unique_samples_analysis_clust) <- fsyc_gl_unique_samples_analysis_clust$strata$geno_clust


# Heterozygosity and FIS ----------------

fsyc_het <- gl.report.heterozygosity(fsyc_gl_unique_samples_analysis, method='pop', verbose = 3) #  nboots not implemented?
ggsave("fsyc_Het_phylo_FIS.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 9, height = 6, units = "in", dpi = 600, limitsize=TRUE)
fsyc_het <- fsyc_het[match(c("Cyprus","Levant","North Egypt","South Egypt-Sudan","Kenya", "South Africa"),fsyc_het$pop), ]
write.csv(fsyc_het, "fsyc_het_phylo.csv")
fsyc_het_test <- gl.test.heterozygosity(fsyc_gl_unique_samples_analysis,nreps=1000, plot.out =TRUE, verbose=2)
write.csv(fsyc_het_test, "fsyc_het_phylo_test.csv")

fsyc_het <- gl.report.heterozygosity(fsyc_gl_unique_samples_analysis_clust, method='pop', verbose = 3) #  nboots not implemented?
ggsave("fsyc_Het_geno_clust_FIS.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 9, height = 6, units = "in", dpi = 600, limitsize=TRUE)
fsyc_het <- fsyc_het[match(c("Levant","North Egypt","South Egypt-Sudan","Kenya", "South Africa"),fsyc_het$pop), ]
write.csv(fsyc_het, "fsyc_het_geno_clust.csv")
fsyc_het_test <- gl.test.heterozygosity(fsyc_gl_unique_samples_analysis_clust,nreps=1000, plot.out =TRUE, verbose=2)
write.csv(fsyc_het_test, "fsyc_het_geno_clust_test.csv")


# Linkage disequilibrium ----------------

# We firs impute missing data because missing data prevent from calculate LD
fsyc_gl_unique_samples_analysis_imp <- gl.impute(fsyc_gl_unique_samples_analysis,method = "neighbour")

# R²
fsyc_ld_phylo <- gl.report.ld.map(fsyc_gl_unique_samples_analysis_imp, ind.limit = 1) 
aggregate(fsyc_ld_phylo[, 5], list(fsyc_ld_phylo$pop), mean)
aggregate(fsyc_ld_phylo[, 5], list(fsyc_ld_phylo$pop), sd)

ggplot(fsyc_ld_phylo, aes(x = pop, y = ld_stat, color = pop)) + 
  geom_boxplot() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

fsyc_ld_phylo_summaries <- cbind(aggregate(fsyc_ld[, 5], list(fsyc_ld_phylo$pop), mean), aggregate(fsyc_ld_phylo[, 5], list(fsyc_ld_phylo$pop), sd)$x)
colnames(fsyc_ld_phylo_summaries) <- c("Phylogroup","Mean","SD")
fsyc_ld_phylo_summaries[match(c("Cyprus","Levant","North Egypt","South Egypt-Sudan","Kenya", "South Africa"), fsyc_ld_phylo_summaries$Phylogroup),]
write.csv(fsyc_ld_phylo_summaries, "fsyc_ld_summaries_phylo.csv")

fsyc_gl_unique_samples_analysis_imp_clust <- fsyc_gl_unique_samples_analysis_imp
pop(fsyc_gl_unique_samples_analysis_imp_clust) <- fsyc_gl_unique_samples_analysis_imp_clust$strata$geno_clust

fsyc_ld_clust <- gl.report.ld.map(fsyc_gl_unique_samples_analysis_imp_clust, ind.limit = 1) 
aggregate(fsyc_ld_clust[, 5], list(fsyc_ld_clust$pop), mean)
aggregate(fsyc_ld_clust[, 5], list(fsyc_ld_clust$pop), sd)

ggplot(fsyc_ld_clust, aes(x = pop, y = ld_stat, color = pop)) + 
  geom_boxplot() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

fsyc_ld_clust_summaries <- cbind(aggregate(fsyc_ld_clust[, 5], list(fsyc_ld_clust$pop), mean), aggregate(fsyc_ld_clust[, 5], list(fsyc_ld_clust$pop), sd)$x)
colnames(fsyc_ld_clust_summaries) <- c("Genomic cluster","Mean","SD")
fsyc_ld_clust_summaries[match(c("Cyprus","Levant","North Egypt","South Egypt-Sudan","Kenya","South Africa"), fsyc_ld_clust_summaries$Phylogroup),]
write.csv(fsyc_ld_clust_summaries, "fsyc_ld_summaries_geno_clust.csv")


# FSTs with dartR ----------------
fsyc_gl_analysis.pwfst <- gl.fst.pop(fsyc_gl_unique_samples_analysis, nboots = 999, percent = 95, nclusters = 1)
fsyc_gl_analysis.pwfst$Fsts
fsyc_gl_analysis.pwfst$Pvalues
write.csv(fsyc_gl_analysis.pwfst$Fsts, "Fsyc_pariwise_phylogroups_FSTs.csv")
write.csv(fsyc_gl_analysis.pwfst$Pvalues, "Fsyc_pariwise_phylogroups_FSTs_pvalues.csv")

fsyc_gl_analysis.pwfst <- gl.fst.pop(fsyc_gl_unique_samples_analysis_clust, nboots = 999, percent = 95, nclusters = 1)
fsyc_gl_analysis.pwfst$Fsts
fsyc_gl_analysis.pwfst$Pvalues
write.csv(fsyc_gl_analysis.pwfst$Fsts, "Fsyc_pariwise_geno_clust_FSTs.csv")
write.csv(fsyc_gl_analysis.pwfst$Pvalues, "Fsyc_pariwise_geno_clust_FSTs_pvalues.csv")


# AMOVA with dartR ----------------
fsyc_amova <- gl.amova(fsyc_gl_unique_samples_analysis, distance = fsyc_dist_inds, permutations = 999, verbose = 2)
write.csv(fsyc_amova$tab, "Fsyc_AMOVA_tab_phylo.csv")
write.csv(fsyc_amova$varcomp, "Fsyc_AMOVA_varcomp_phylo.csv")
write.csv(fsyc_amova$varcoef, "Fsyc_AMOVA_varcoef_phylo.csv")

fsyc_amova <- gl.amova(fsyc_gl_unique_samples_analysis_clust, distance = fsyc_dist_inds, permutations = 999, verbose = 2)
write.csv(fsyc_amova$tab, "Fsyc_AMOVA_tab_clust.csv")
write.csv(fsyc_amova$varcomp, "Fsyc_AMOVA_varcomp_clust.csv")
write.csv(fsyc_amova$varcoef, "Fsyc_AMOVA_varcoef_clust.csv")


# MAF ----------------
fsyc_gl_unique_samples_analysis <- gl.report.maf(fsyc_gl_unique_samples_analysis)
fsyc_unique_maf <- fsyc_gl_unique_samples_analysis@other$loc.metrics$maf
# Plot
ggplot(data.frame(MAF=fsyc_unique_maf), aes(x=MAF)) +
  geom_histogram(binwidth = 0.01, fill = "steelblue", color = "black") +
  theme_classic() +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0)) +
  xlab("Minor allele frenquency") +
  ylab("Count")
ggsave("figure_S5.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in", dpi = 600, limitsize=TRUE)



# ---------------------------------------------------------------------------- #
# Multilocus genotypes with POPPR
# ---------------------------------------------------------------------------- #



# Multilocus genotypes ----------------

fsyc_snpclone <- as.snpclone(fsyc_gl_unique_samples_analysis, parallel = FALSE)
saveRDS(fsyc_snpclone, "fsyc_snpclone.RDS")
fsyc_snpclone_clust <- as.snpclone(fsyc_gl_unique_samples_analysis_clust, parallel = FALSE)

thresh <- 20 # We deduct this from the pairwise distance graph with replicates

# Filter snpclone
mlg.filter(fsyc_snpclone, distance = bitwise.dist, euclidean = TRUE, missing = "mean") <- thresh
mll(fsyc_snpclone)
mlg(fsyc_snpclone)
mll(fsyc_snpclone)

# Clone correct
fsyc_snpclone_cc <- clonecorrect(fsyc_snpclone, strata = 1, combine = FALSE, keep = 1)
pop(fsyc_snpclone_cc) <- fsyc_snpclone_cc$strata$phylo
# Not usefull for our study

# Distance tree
pop_tree <- aboot(fsyc_snpclone, distance = bitwise.dist, euclidean = TRUE, sample = 1000, cutoff = 50)
saveRDS(pop_tree, "pop_tree_aboot.RDS")
ggtree(pop_tree)
# ggsave("pop_tree_tmp.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 2.5, height = 8, units = "in", dpi = 600, limitsize=TRUE)

fsyc_ggtree <- ggtree(pop_tree) # + geom_treescale()

# Create tip labels
tipcategories <- as.data.frame(cbind(as.data.frame(indNames(fsyc_snpclone)), as.data.frame(fsyc_snpclone$strata$phylo)))
colnames(tipcategories) <- c("ind.names","phylo")

# Plot distance tree
fsyc_ggtree %<+% tipcategories + 
#  geom_tiplab(aes(fill = factor(phylo)), color = "black",  geom = "label", label.padding = unit(0.15, "lines"), label.size = 1) +
  geom_tippoint(aes(fill=factor(phylo), x=x+0.2), size=3, alpha=1, shape=21,color="black") +
  geom_nodelab(size = 2, nudge_x = 0.7, nudge_y = 0) + 
  theme_tree2(legend.position = 'centre') +
  labs(caption="Euclidean genetic distance") +
  scale_x_ggtree(breaks = seq(0,20,2)) +
  geom_vline(xintercept = 8.2, color = "red", size = 2, alpha = 0.5, lty = 2) +
  scale_fill_manual(values = phylo_pal) 
ggsave("figure_5b.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 2.5, height = 8, units = "in", dpi = 600, limitsize=TRUE)


# Minimum spanning network ----------------

fsyc_msn <- poppr.msn(fsyc_snpclone, bitwise.dist(fsyc_snpclone,euclidean = TRUE)) # 
plot_poppr_msn(fsyc_snpclone, fsyc_msn, palette = phylo_pal, inds = "n", quantiles = F, gadj =5, gweight =2, nodescale = 35)
fsyc_msn_cc <- poppr.msn(fsyc_snpclone_cc, bitwise.dist(fsyc_snpclone_cc,euclidean = TRUE)) # 
plot_poppr_msn(fsyc_snpclone_cc, fsyc_msn_cc, palette = phylo_pal, inds = "n", quantiles = F, gadj =5, gweight =2, nodescale = 35)

# Plot
png(filename = "Fsyc_msn.png", width = 7, height = 5, units = "in", res = 600)
plot_poppr_msn(fsyc_snpclone, fsyc_msn, palette = phylo_pal, inds = "n", quantiles = F, gadj =5, gweight =2, nodescale = 35)
dev.off()
png(filename = "Fsyc_msn_cc.png", width = 7, height = 5, units = "in", res = 600)
plot_poppr_msn(fsyc_snpclone_cc, fsyc_msn_cc, palette = phylo_pal, inds = "n", quantiles = F, gadj =5, gweight =2, nodescale = 35)
dev.off()


# AMOVA ----------------

fsyc_amova_phylo_poppr <- poppr.amova(fsyc_snpclone, ~phylo)
fsyc_amova_phylo_poppr$results
fsyc_amova_phylo_poppr$componentsofcovariance
fsyc_amova_phylo_poppr$statphi
fsyc_amova_phylo_poppr_signif   <- randtest(fsyc_amova_phylo_poppr, nrepet = 999)
plot(fsyc_amova_phylo_poppr_signif)

# Plot
png(filename = "fsyc_amova_randtest_phylo.png", width = 6, height = 10, units = "in", res = 600)
plot(fsyc_amova_phylo_poppr_signif)
dev.off()

# Export (AMOVA was done earlier, check if similar)
write.csv(fsyc_amova_phylo_poppr$results, "Fsyc_AMOVA_poppr_results_phylo.csv")
write.csv(fsyc_amova_phylo_poppr$componentsofcovariance, "Fsyc_AMOVA_poppr_componentsofcovariance_phylo.csv")
write.csv(fsyc_amova_phylo_poppr$statphi, "Fsyc_AMOVA_poppr_statphi_phylo.csv")
write.csv(fsyc_amova_phylo_poppr$expvar, "Fsyc_AMOVA_poppr_expect_phylo.csv")
write_lines(fsyc_amova_phylo_poppr_signif,"Fsyc_AMOVA_poppr_signif_phylo.txt")

fsyc_amova_geno_clust_poppr <- poppr.amova(fsyc_snpclone_clust, ~geno_clust)
fsyc_amova_geno_clust_poppr$results
fsyc_amova_geno_clust_poppr$componentsofcovariance
fsyc_amova_geno_clust_poppr$statphi
fsyc_amova_geno_clust_poppr_signif   <- randtest(fsyc_amova_geno_clust_poppr, nrepet = 999)
plot(fsyc_amova_geno_clust_poppr_signif)

png(filename = "figure_5c.png", width = 6, height = 10, units = "in", res = 600)
plot(fsyc_amova_geno_clust_poppr_signif)
dev.off()

# Export (AMOVA was done earlier, check if similar)
write.csv(fsyc_amova_geno_clust_poppr$results, "fsyc_amova_poppr_results_geno_clust.csv")
write.csv(fsyc_amova_geno_clust_poppr$componentsofcovariance, "fsyc_amova_poppr_componentsofcovariance_geno_clust.csv")
write.csv(fsyc_amova_geno_clust_poppr$statphi, "fsyc_amova_poppr_statphi_geno_clust.csv")
write.csv(fsyc_amova_geno_clust_poppr$expvar, "fsyc_amova_poppr_expect_geno_clust.csv")
write_lines(fsyc_amova_geno_clust_poppr_signif,"fsyc_amova_poppr_signif_geno_clust.txt")



# ---------------------------------------------------------------------------- #
# End of script
# ---------------------------------------------------------------------------- #