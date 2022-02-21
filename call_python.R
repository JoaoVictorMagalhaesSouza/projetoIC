library(reticulate)
use_condaenv("C:/Users/JoaoV/anaconda3/envs/Eldorado_Deus", required = TRUE)
py_config()

py_run_file(file="real_time.py")
