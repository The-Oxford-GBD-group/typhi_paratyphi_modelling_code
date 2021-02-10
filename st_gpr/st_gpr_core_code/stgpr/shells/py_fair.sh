#!/bin/sh
#$ -S /bin/sh

source /ihme/code/st_gpr/miniconda3/bin/activate stgpr
python "$@"
