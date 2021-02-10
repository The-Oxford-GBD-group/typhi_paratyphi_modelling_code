###Author: Patrick Liu
### Date: 1/26/2015
### Project: ST-GPR
### Purpose: Cluster tools
###########################################################

####################################################################################################################################################
#                                                                Table of Contents
####################################################################################################################################################

## Job submission
    ## job_hold

## Append scripts
    ## append_csv
    ## append_load
    ## append_pdf

## Other utility
    ## split_args
  ## h5read.py

####################################################################################################################################################
#                                                                 Job Submission
####################################################################################################################################################


job_hold <- function(job_name, file_list=NULL, obj=NULL, resub=0) {

        ## Give it a sec to launch
        Sys.sleep(5)

        ## Start timer
        start.time <- proc.time()

        ## Wait for job to finish
        flag <-  0
        while (flag == 0) {
            ## Check if job is done
            if (system(paste0("qstat -r | grep ", job_name, "|wc -l"), intern=T) == 0) {
                ## If so, set flag to 1
                flag <- 1
            } else {
                Sys.sleep(5)
            }
        }

        ## End Timer
        job.runtime <- proc.time() - start.time
        job.runtime <- job.runtime[3]

        ## Give it another sec
        Sys.sleep(10)


        ## Check for the file list
        if (!is.null(file_list)) {    
            missing_list <- NULL
            for (file in file_list) {
                ## Ensure that all files are there
                if (!file.exists(file)) {    
                    missing_list <- rbind(missing_list, file)
                ## Check obj if hdf
                } else {
                    if (grepl(".h5", file_list[1])) {
                        if (!(obj %in% h5ls(file_list)$name)) {
                            missing_list <- rbind(missing_list, file)
                        }
                    } 
                }
            }
    
            ## If missing_list > 0, break
            if (length(missing_list) > 0) {
                if (resub == 0) {
                    stop(paste0("Job failed: ", job_name, 
                                "\nTime elapsed: ", job.runtime,
                                "\nYou are missing the following files: ", toString(missing_list)))
                    } else {
                        return(1)
                    }
            } else {
                return(0)
            }
        }

        ## Complete
        print(paste0("Job ", job_name, " has completed. Time elapsed: ", job.runtime))
    }


####################################################################################################################################################
#                                                                 Append Scripts
####################################################################################################################################################



append_csv <- function(root, output_path, output_name) {

    ## Run bash script to append
    script <- paste0(stgpr_path("shells_root"), "/append_csv.sh")
    cmd <- paste("sh", script, root, output_path, output_name, sep = " ")
    system(cmd)

    print(paste0("Append complete. Ouput location: ", output_path, "/", output_name, ".csv"))
}


append_load <- function(root, output_path, output_name, rm=FALSE) {

    ## Append files and load
    append_csv(root, output_path, output_name)
    df <- fread(paste0(output_path, "/", output_name, ".csv"))

    ## If remove, clear file
    if (rm) unlink(paste0(output_path, "/", output_name, ".csv"))

    return(df)
}

append_pdf <- function(input, output, rm=FALSE) {
    cmd <- paste0("/usr/bin/ghostscript -dBATCH -dSAFER -dNOGC -DNOPAUSE -dNumRenderingThreads=4 -q -sDEVICE=pdfwrite -sOutputFile=", output, " ", input)
    system(cmd)    
    if (rm) unlink(dirname(input[1]), recursive=T)
}

####################################################################################################################################################
#                                                                 Utility
####################################################################################################################################################

split_args <- function(x, n) {
  f <- ceiling(seq_along(x)/(length(x)/n))
  if(any(unique(f) != c(1:n))){
    f[f > n] <- n
  }
    list <- split(x, f)
    lapply(seq(1, n), function(x) list(head(list[[x]], n=1), tail(list[[x]], n=1))) %>% rbindlist %>% data.frame
}

####################################################################################################################################################
#                                                              AUGHHHHHHHHHH
####################################################################################################################################################

h5read.py = function(h5File, name) {
  
  listing = h5ls(h5File)
  
  if (!(name %in% listing$name)) stop(paste0(name, " not in HDF5 file"))
  
  # only take requested group (df) name
  listing = listing[listing$group==paste0('/',name),]
  
  # Find all data nodes, values are stored in *_values and corresponding column
  # titles in *_items
  data_nodes = grep("_values", listing$name)
  name_nodes = grep("_items", listing$name)
  
  data_paths = paste(listing$group[data_nodes], listing$name[data_nodes], sep = "/")
  name_paths = paste(listing$group[name_nodes], listing$name[name_nodes], sep = "/")
  
  columns = list()
  for (idx in seq(data_paths)) {
    data <- data.frame(t(h5read(h5File, data_paths[idx])))
    names <- t(h5read(h5File, name_paths[idx]))
    
    if (length(names) != length(data)){
      print("There were issues with loading this sooo I skipped the problem columns. Hope they don't matter!")
    }else{
      entry <- data.frame(data)
      colnames(entry) <- names
      columns <- append(columns, entry)
    }
  }
  
  data <- data.frame(columns)
  
  return(data)
}


