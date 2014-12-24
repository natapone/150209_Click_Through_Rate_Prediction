# USAGE
# source("Click_through.R")
# train_set = read_data('train',1000)
# test_set = read_data('test')
# validate_count_unique(train_set)
# test_without_train(train_set, test_set)

require(data.table)

int_model_simple <- function(data, type) {
    print ("This is Simple!")
    
    # simple model
    # banner_pos, site_category, app_category, device_type, device_conn_type
    
    col_list = c(
        "banner_pos",
        "site_category",
        "app_category",
        "device_type",
        "device_conn_type"
    )
    
    if (type == "train") {
        col_list = c(col_list, "click")
    }
    
    data[, col_list, with=FALSE]
}

interpret_data <- function( data, model="simple", type="train") {
    
    if (model == "simple") {
        data = int_model_simple(data, type)
    }
    
    data
}

read_data <- function(file_name, limit=0) {
    # read data by type
    file_path = paste("Raw", file_name, sep = "/")
    
    if (limit) {
        data = fread(file_path, nrows=limit)
    } else {
        data = fread(file_path)
    }
    
    data
}

validate_count_unique <- function(data) {
    # loop through columns
    
    for (col_name in colnames(data)) {
        # print(col_name)
        
        col_data = data[[col_name]]
        col_data = unique(col_data) # only unique
        
        #print (length(data[[col_name]])) # count data
        print( c(col_name , length(col_data)) )
        
        
        # test[,i]
    }
    
}

test_without_train <- function(train, test) {
    
    field_list = c(
        "C1",
        "banner_pos",
        "site_id",
        "site_domain",
        "site_category",
        "app_id",
        "app_domain",
        "app_category",
        "device_id",
        "device_ip",
        "device_model",
        "device_type",
        "device_conn_type",
        "C14",
        "C15",
        "C16",
        "C17",
        "C18",
        "C19",
        "C20",
        "C21"
    )
    
    for (col_name in field_list) {
        
        col_data_train = train[[col_name]]
        col_data_train = unique(col_data_train)
        
        col_data_test = test[[col_name]]
        col_data_test = unique(col_data_test)
        
        # test that not in train
        miss = setdiff(col_data_test, col_data_train)
        
        print ( 
            c(
                col_name,
                length(miss)
            )
        )
    }
    
}


