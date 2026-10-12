#!/bin/bash
# Runs scripts/acic/acic_run.R for ACIC 2022 dataset indices 1..3400 with at most NJOBS concurrent jobs.
# Run from the repository root. Each job seeds its own RNG (L'Ecuyer-CMRG, 533 + index), so the
# results do not depend on the number of concurrent jobs.
NJOBS=${NJOBS:-4}
mkdir -p logs/acic
for i in $(seq 1 3400); do
  nohup Rscript scripts/acic/acic_run.R "$i" > "logs/acic/acic_$i.log" 2>&1 &
  while [ "$(jobs -rp | wc -l)" -ge "$NJOBS" ]; do sleep 50; done
done
wait
