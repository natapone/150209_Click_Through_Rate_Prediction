# source("Click_through_cleanup.R")

source("Click_through.R")


# data = read_data('train',data_source="web", limit=0)

# USAGE
# clean_hour()
# clean_site_traffic_level()

clean_app_traffic_level <- function(file_name="train") {
    col_class = get_single_col_class_list("app_domain", file_name)
    data = read_data(file_name, limit=1000, col_class_list=col_class)
    #return(data)
    file_name = "rdata/app_traffic_count.RData"
    traffic_level = cal_traffic_level(data, file_name)
    
    data = data_by_traffic_level(data, traffic_level)
    
    file_name = "clean/col_app_traffic.RData"
    cat("Write to file")
    print(file_name)
    
    saveRDS(data, file = file_name,compress = F)
    
    cat("Total ")
    cat(length(data))
    print(" rows")
    NULL
}

clean_site_traffic_level <- function(file_name="train") {
    col_class = get_single_col_class_list("site_domain", file_name)
    data = read_data(file_name, limit=0, col_class_list=col_class)
#     return(data)
    file_name = "rdata/site_traffic_count.RData"
    traffic_level = cal_traffic_level(data, file_name)
    
    data = data_by_traffic_level(data, traffic_level)
    
    file_name = "clean/col_site_traffic.RData"
    cat("Write to file")
    print(file_name)
    
    saveRDS(data, file = file_name,compress = F)
    
    cat("Total ")
    cat(length(data))
    print(" rows")
    NULL
}

data_by_traffic_level <- function(data, traffic_level) {
    
    # recreate data list
    traffic = rep(0, nrow(data))
    
    # list all traffic by domain
    domains = names(traffic_level)
    
    for(domain in domains) {
#         cat(domain)
#         print(traffic_level[[domain]])
        
        traffic_index = data[, data == domain ]
        traffic[traffic_index] = traffic_level[[domain]]
    }
    
    traffic
}

cal_traffic_level <- function(data, file_name) {
    
    print("Count frequency")
    traffic_level = as.list(table(data))
    
    print("Cal traffic log level")
    traffic_log_level = lapply(traffic_level, FUN = function(x) round(log(x)))
    
    print("Remove level zero")
    traffic_log_level = lapply(traffic_log_level, FUN = function(x) if(x>0){x})
    traffic_log_level = traffic_log_level[!sapply(traffic_log_level, is.null)]
    
    
    cat("Save level ")
    print(file_name)
    saveRDS(traffic_log_level, file = file_name)
    
    traffic_log_level
}

clean_hour <- function(file_name="train") {
    col_class = get_single_col_class_list("hour", file_name)
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
    } else if (col_name == "site_domain") {
        
        col_class_list = c(rep("NULL",times = 5))
        
        if (file_name == 'train') {
            col_class_list = c(
                col_class_list, 
                "NULL",
                "character"
            )
        } else {
            col_class_list = c(
                col_class_list,
                "character"
            )
        }
        # not read
        col_class_list = c(col_class_list, rep("NULL",times = 17))
    } else if (col_name == "app_domain") {
        col_class_list = c(rep("NULL",times = 8))
        
        if (file_name == 'train') {
            col_class_list = c(
                col_class_list, 
                "NULL",
                "character"
            )
        } else {
            col_class_list = c(
                col_class_list,
                "character"
            )
        }
        # not read
        col_class_list = c(col_class_list, rep("NULL",times = 14))
    } else {
        print("Column name not found!")
    }
    
    col_class_list
}