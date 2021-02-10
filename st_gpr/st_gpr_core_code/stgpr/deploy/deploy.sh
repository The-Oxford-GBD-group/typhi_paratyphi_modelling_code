CONDA_DIR='/ihme/code/st_gpr/miniconda3'
CONDA_BIN="$CONDA_DIR/bin/conda"
ACTIVATE="$CONDA_DIR/bin/activate"
DEACTIVATE="$CONDA_DIR/bin/deactivate"


save_yml(){
    # The environment to export must be activated
    current_env=$CONDA_DEFAULT_ENV
    conda env export --no-builds > "$CONDA_DIR/envs/$current_env/environment.yml"
}

build_env(){
    old_env="$1"
    new_env="$2"
    $ACTIVATE "$old_env"
    save_yml "$old_env"
    $DEACTIVATE "$old_env"
    $CONDA_BIN create --name "$new_env" --clone "$old_env"
}

update_symlink(){
    # The new environment must be activated
    new_env=$CONDA_DEFAULT_ENV
    ln -nfsv "$new_env" "$CONDA_DIR/envs/stgpr"
}
