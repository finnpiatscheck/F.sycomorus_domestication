# Software: dartR v2.9.9.5

library(dartR)

# Import SNP data
fsyc_gl_61inds <- gl.read.vcf(
    "fsyc_snps_unique_samples_outgroups.vcf",
    verbose = TRUE
)

# Assign population labels in the same order as the individuals
pop_61inds <- c(
    "F.pet", "F.pet", "F.pet", "F.pet", "F.pet", "F.pet", "F.pet",
    "AGN", "AGN", "AGN", "EPL", "LAR", "LIK", "LIK", "ANF", "AUB",
    "COB", "COB", "COB", "CHE", "HAH", "SMS", "MEB",
    "MEN", "MEN", "MEN", "MEN", "TRC",
    "ISNI", "ISNI", "ISNI",
    "ISTA", "ISTA", "ISTA", "ISTA",
    "KE", "EG", "LXR", "LXR", "LXR",
    "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M",
    "MFS", "MFS", "MFS", "MFS", "MFS",
    "SDN", "SON", "SAW", "SAW", "SAT", "SAW", "SAW", "SAW",
    "SAT", "SAT", "SAT"
)

# Check population labels against the imported individuals
stopifnot(length(pop_61inds) == nInd(fsyc_gl_61inds))

pop(fsyc_gl_61inds) <- pop_61inds

# Export PLINK files
gl2plink(
    fsyc_gl_61inds,
    outfile = "gl_plink"
)
