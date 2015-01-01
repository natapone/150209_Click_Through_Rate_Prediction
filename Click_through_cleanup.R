# source("Click_through_cleanup.R")

source("Click_through.R")


# data = read_data('train',data_source="web", limit=0)

# USAGE
# col_class_list = get_single_col_class_list("hour", "train")

# clean_hour <- function(data) {
#     
#     # check if web or 
#     
# }

clean_hour <- function(file_name="train", data_source="web") {
    col_class = get_single_col_class_list("hour", "train")
    data = read_data(file_name, limit=0, col_class_list=col_class)
    
    print("epoch to hour")
    data = apply(data,1,FUN = function(t) {
            as.POSIXlt(t, origin="1970-01-01", tz="GMT")$hour
        }
    )
    
    file_name = "clean/col_hour.RData"
    cat("Write to file")
    print(file_name)
    
    saveRDS(data, file = file_name,compress = F)
    
    cat("Total ")
    cat(length(data))
    print(" rows")
    NULL
}

get_single_col_class_list <- function(col_name, file_name) {
    
    if (col_name == "hour") {
        
        if (file_name == 'train') {
            col_class_list = c(
                "NULL",
                "NULL",
                "numeric"
            )
        } else {
            col_class_list = c(
                "NULL",
                "numeric"
            )
        }
        # not read
        col_class_list = c(col_class_list, rep("NULL",times = 21))
    }
    
    col_class_list
}