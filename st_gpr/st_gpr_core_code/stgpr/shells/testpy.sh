#!/bin/sh
#$ -S /bin/sh

source /homes/$USER/miniconda3/bin/activate my_stgpr
python "$@"