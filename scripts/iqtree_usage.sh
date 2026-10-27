#!/usr/bin/env bash

# Software: IQ-TREE 3.1.3; PAML 4.10.8.iq2mc

# 1. Maximum-likelihood tree inference
iqtree3 \
    -s fsyc_alignment.varsites.phy \
    -st DNA \
    -m MFP+ASC \
    -bb 10000 \
    -alrt 1000

# 2. Posterior divergence-time analysis
iqtree3 \
    -s fsyc_alignment.varsites.phy \
    -st DNA \
    -m TPM3u+ASC+R2 \
    -te fsyc_alignment.fas.varsites.phy.treefile.rooted_no_bootstrap_calibration.nwk \
    --dating mcmctree \
    --mcmc-clock IND \
    --mcmc-iter 100000,100,100000 \
    --prefix sycomorus_Gardner42

mcmctree sycomorus_Gardner42.mcmctree.ctl

# 3. Prior-only divergence-time analysis
iqtree3 \
    -s fsyc_alignment.varsites.phy \
    -st DNA \
    -m TPM3u+ASC+R2 \
    -te fsyc_61inds_with_outgroups_no_algeria.fas.varsites.phy.treefile.rooted_no_bootstrap_calibration.nwk \
    --dating mcmctree \
    --mcmc-clock IND \
    --mcmc-iter 10000,10,10000 \
    --prefix sycomorus_prior

# Set usedata = 0 in sycomorus_prior.mcmctree.ctl before proceeding.
mcmctree sycomorus_prior.mcmctree.ctl
