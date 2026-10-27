#!/usr/bin/env bash

# Software: PLINK v1.9; fastStructure v1.0

# Convert PLINK PED/MAP files to binary BED format
plink \
    --ped fsyc_gl_unique_samples_analysis_plink.ped \
    --map fsyc_gl_unique_samples_analysis_plink.map \
    --allow-no-sex \
    --allow-extra-chr \
    --chr-set 95 \
    --make-bed \
    --out gl_plink

# Run fastStructure for K = 2 to 10
for k in {2..10}
do
    python ./fastStructure \
        -K "$k" \
        --input=gl_plink \
        --output=fast_out
done

# Extract marginal likelihoods
grep "Marginal Likelihood" fast_out.*.log
