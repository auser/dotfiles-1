#!/bin/bash

PATH="/path/to/your/miniconda3/bin:$PATH"

eval "$(conda shell.bash hook)"
conda activate jupyter

cd ~ && /path/to/your/miniconda3/envs/jupyter/bin/python -m jupyter lab --port 8888 --no-browser
