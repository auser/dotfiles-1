# Add conda-forge and activate strict
conda config --add channels conda-forge
conda config --set channel_priority strict
conda config --set show_channel_urls True

# Make conda faster ny disabeling checks.
conda config --set safety_checks disabled

# Do not actiavte base.
conda config --set auto_activate_base False

# Install these packages by default when using `conda create`.
conda config --append create_default_packages ipykernel \
             --append create_default_packages jupyter \
             --append create_default_packages jupyter_contrib_nbextensions \
             --append create_default_packages pip \
             --append create_default_packages "blas=*=openblas"  # help with the mkl vs openblas issue
