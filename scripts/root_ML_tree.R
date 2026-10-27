library(ape)
library(TreeTools)

ML_tree <- read.tree("/home/finn/Work/IQTREEv3/ml_tree_61inds_2026.09.08/fsyc_61inds_with_outgroups_no_algeria.fas.varsites.phy.treefile")
plot(ML_tree)
TipLabels(ML_tree)

# I do not know why IQTREE added "_" to the tip bales
# ML_tree$tip.label <- gsub("_", "", ML_tree$tip.label)
# TipLabels(ML_tree)
# I will not remove them now, because I might need the same labels for further analysis with IQTREE

outgroup <- c("95-12B____", "95-201____", "95-18_____", "95-53_____", "95-200____", "95-59_____", "95-202____")
ML_tree_rooted <- root(ML_tree, outgroup = outgroup, resolve.root = TRUE)
is.rooted(ML_tree_rooted)
plot(ML_tree_rooted)

# Save (nwk)
write.tree(ML_tree_rooted, file = "/home/finn/Desktop/fsyc_61inds_with_outgroups_no_algeria.fas.varsites.phy.treefile.rooted.nwk")
TipLabels(ML_tree_rooted)
