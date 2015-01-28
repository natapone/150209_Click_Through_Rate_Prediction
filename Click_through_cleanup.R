# source("Click_through_cleanup.R")

source("Click_through.R")


# data = read_data('train',data_source="web", limit=0)

# USAGE
# clean_hour()
# clean_site_traffic_level()
# clean_rare_category("C1", "train", "web")
# clean_rare_category("banner_pos", "train", "app")
# clean_rare_category("site_category", "train", "web")
# clean_rare_category("app_category", "train", "app")
# clean_rare_category("device_model", "train", "web")
# clean_rare_category("device_type", "train", "web")
# clean_rare_category("device_conn_type", "train", "web")
# clean_rare_category("C14", "train", "web")
# clean_rare_category("C15", "train", "web")
# clean_rare_category("C16", "train", "web")
# clean_rare_category("C17", "train", "web")
# clean_rare_category("C18", "train", "web")
# clean_rare_category("C19", "train", "web")
# clean_rare_category("C20", "train", "web")
# clean_rare_category("C21", "train", "web")
# clean_click("web")
# data = clean_traffic("train", "web")

# Create Test set
# split_test_id()

# Raplace RARE
# C1,banner_pos,site_category,app_category
# device_model,device_type,device_conn_type
# C14,C15,C16,C17,C18,C19,C20,C21

# fix previous clean: split web/app for hour, traffic_level

split_test_id <- function() {
    
    file_name = "test"
    col_class = get_single_col_class_list("id", file_name)
    
    # id web
    data_source = "web"
    data = read_data(file_name, data_source , limit=0, col_class_list=col_class)
    
    # save file
    file_path = paste("col",file_name,data_source,"id",sep="_")
    file_path = paste(file_path, "RData", sep=".")
    file_path = paste("clean", file_path, sep="/")
    
    cat("Write to file")
    print(file_path)
    saveRDS(data$id, file = file_path,compress = F)
    
    # id app
    data_source = "app"
    data = read_data(file_name, data_source , limit=0, col_class_list=col_class)
    
    # save file
    file_path = paste("col",file_name,data_source,"id",sep="_")
    file_path = paste(file_path, "RData", sep=".")
    file_path = paste("clean", file_path, sep="/")
    
    cat("Write to file")
    print(file_path)
    saveRDS(data$id, file = file_path,compress = F)
    
    1
}

# save click as separated file for training
clean_click <- function(data_source="web") {
    col_class = get_single_col_class_list("click", "train")
    data = read_data("train", data_source , limit=0, col_class_list=col_class)
#     return(data)
    
    # save file
    file_name = paste("col","train",data_source,"click",sep="_")
    file_name = paste(file_name, "RData", sep=".")
    file_name = paste("clean", file_name, sep="/")
    
    cat("Write to file")
    print(file_name)
    saveRDS(data$click, file = file_name,compress = F)
    return(data$click)
}

clean_rare_category <- function(cat_name, file_name="train", data_source="web") {
    
    # read single column data(train/test)
    col_class = get_single_col_class_list(cat_name, file_name)
    data = read_data(file_name, data_source , limit=0, col_class_list=col_class)
#     return(data)
    
    # read model probability to get RARE level
    model_file = paste("rdata/model_intersect_prob_", data_source, ".RData", sep="")
    model_prob = readRDS(model_file)
    
    col_classes = model_prob[[cat_name]]
    
    # loop class in column
    not_rare_index = c()
    for (col_class in names(col_classes)) {
        if(col_class != "RARE") {
            cat(cat_name)
            cat(" - ")
            cat(col_class)
            print("--- indexed!")
            
            i = which(data == col_class)
            not_rare_index = c(not_rare_index, i)
            not_rare_index = unique(not_rare_index)
        }
        
    }
    
    # replace with RARE
    print("Replace with RARE")
    data[-not_rare_index] = "RARE"
    data = data[[cat_name]] # convert to vector
    
    # col_train_web_C1.RData
    file_name = paste("col",file_name,data_source,cat_name,sep="_")
    file_name = paste(file_name, "RData", sep=".")
    file_name = paste("clean", file_name, sep="/")
    
    cat("Write to file")
    print(file_name)
    
    saveRDS(data, file = file_name,compress = F)
    
    cat("Total ")
    cat(length(data))
    print(" rows")
#     NULL
    return (data)
}

# skip creating traffic level
clean_traffic <- function(file_name="train", data_source="web") {
    
    if(data_source == "web") {
        level_file_name = "rdata/site_traffic_count.RData"
        cat_name = "site_domain"
    } else if (data_source == "app") {
        level_file_name = "rdata/app_traffic_count.RData"
        cat_name = "app_domain"
    } else {
        print("Choose data source web or app")
        return()
    }
    
    # read data
    col_class = get_single_col_class_list(cat_name, file_name)
    data = read_data(file_name, data_source , limit=0, col_class_list=col_class)
#     return(data)
    
    # read levels from file
    cat("Read level ")
    print(level_file_name)
    traffic_level = readRDS(level_file_name)
#     return(traffic_level)
    
    # replace
    data = data_by_traffic_level(data, traffic_level)
    
    # save to file
    file_name = paste("col",file_name,data_source,"traffic",sep="_")
    file_name = paste(file_name, "RData", sep=".")
    file_name = paste("clean", file_name, sep="/")
    
    cat("Write to file")
    print(file_name)
    
    saveRDS(data, file = file_name,compress = F)
    
    cat("Total ")
    cat(length(data))
    print(" rows")
    
    return(data)
}

clean_app_traffic_level <- function(file_name="train") {
    col_class = get_single_col_class_list("app_domain", file_name)
    data = read_data(file_name, limit=1000, col_class_list=col_class)
    #return(data)
    file_name = "rdata/app_traffic_count.RData"
    traffic_level = cal_traffic_level(data, file_name)
    
#     data = data_by_traffic_level(data, traffic_level)
#     
#     file_name = "clean/col_app_traffic.RData" # change to col_train_app_traffic.RData
#     cat("Write to file")
#     print(file_name)
#     
#     saveRDS(data, file = file_name,compress = F)
#     
#     cat("Total ")
#     cat(length(data))
#     print(" rows")
#     NULL
}

clean_site_traffic_level <- function(file_name="train") {
    col_class = get_single_col_class_list("site_domain", file_name)
    data = read_data(file_name, limit=0, col_class_list=col_class)
#     return(data)
    file_name = "rdata/site_traffic_count.RData"
    traffic_level = cal_traffic_level(data, file_name)
    
#     data = data_by_traffic_level(data, traffic_level)
#     
#     file_name = "clean/col_site_traffic.RData"
#     cat("Write to file")
#     print(file_name)
#     
#     saveRDS(data, file = file_name,compress = F)
#     
#     cat("Total ")
#     cat(length(data))
#     print(" rows")
#     NULL
}

data_by_traffic_level <- function(data, traffic_level) {
    
    # recreate data list
    traffic = rep(0, nrow(data))
    
    # list all traffic by domain
    domains = names(traffic_level)
    
    for(domain in domains) {
#         cat(domain)
#         print(traffic_level[[domain]])
        
        traffic_index = which(data == domain)
        if(length(traffic_index) > 0) {
            traffic[traffic_index] = traffic_level[[domain]]
        }
        
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

split_col_hour <- function(file_name="train") {
    # read hour data
    # col_train_hour.RData
    hour_file_name = paste("col", file_name, "hour", sep="_")
    hour_file_name = paste(hour_file_name, "RData", sep=".")
    hour_file_name = paste("clean", hour_file_name, sep="/")
    
    hour_data = readRDS(hour_file_name)
    print(hour_file_name)
    return(hour_data)
    # read app index
    
    # split
    
    # save to file
    
    ### all hour in train in 5, just skip
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
    } else if (col_name == "C1") {
        col_class_list = c(rep("NULL",times = 2))
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
        col_class_list = c(col_class_list, rep("NULL",times = 20))
    } else if (col_name == "banner_pos") {
        col_class_list = c(rep("NULL",times = 3))
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
        col_class_list = c(col_class_list, rep("NULL",times = 19))
    } else if (col_name == "site_category") {
        col_class_list = c(rep("NULL",times = 6))
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
        col_class_list = c(col_class_list, rep("NULL",times = 16))
    } else if (col_name == "app_category") {
        col_class_list = c(rep("NULL",times = 9))
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
        col_class_list = c(col_class_list, rep("NULL",times = 13))
    } else if (col_name == "device_model") {
        col_class_list = c(rep("NULL",times = 12))
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
        col_class_list = c(col_class_list, rep("NULL",times = 10))
    } else if (col_name == "device_type") {
        col_class_list = c(rep("NULL",times = 13))
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
        col_class_list = c(col_class_list, rep("NULL",times = 9))
    } else if (col_name == "device_conn_type") {
        col_class_list = c(rep("NULL",times = 14))
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
        col_class_list = c(col_class_list, rep("NULL",times = 8))
    } else if (col_name == "C14") {
        col_class_list = c(rep("NULL",times = 15))
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
        col_class_list = c(col_class_list, rep("NULL",times = 7))
    } else if (col_name == "C15") {
        col_class_list = c(rep("NULL",times = 16))
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
        col_class_list = c(col_class_list, rep("NULL",times = 6))
    } else if (col_name == "C16") {
        col_class_list = c(rep("NULL",times = 17))
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
        col_class_list = c(col_class_list, rep("NULL",times = 5))
    } else if (col_name == "C17") {
        col_class_list = c(rep("NULL",times = 18))
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
        col_class_list = c(col_class_list, rep("NULL",times = 4))
    } else if (col_name == "C18") {
        col_class_list = c(rep("NULL",times = 19))
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
        col_class_list = c(col_class_list, rep("NULL",times = 3))
    } else if (col_name == "C19") {
        col_class_list = c(rep("NULL",times = 20))
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
        col_class_list = c(col_class_list, rep("NULL",times = 2))
    } else if (col_name == "C20") {
        col_class_list = c(rep("NULL",times = 21))
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
        col_class_list = c(col_class_list, rep("NULL",times = 1))
    } else if (col_name == "C21") {
        col_class_list = c(rep("NULL",times = 22))
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
    } else if (col_name == "click") {
        col_class_list = c(
            "NULL",
            "numeric"
        )
        # not read
        col_class_list = c(col_class_list, rep("NULL",times = 22))
    } else if (col_name == "id") {
        
        if (file_name == 'train') {
            col_class_list = c(
                "character",
                "NULL"
            )
        } else {
            col_class_list = c(
                "character"
            )
        }
        # not read
        col_class_list = c(col_class_list, rep("NULL",times = 22))
    } else {
        print("Column name not found!")
    }

    col_class_list
}
