# USAGE
# source("Click_through.R")
# train_set = read_data('train',1000)
# train_set = read_data('train',data_source="web", limit=0)
# test_set = read_data('test')

# Count
# validate_count_unique(train_set)
# test_without_train(train_set, test_set)
# all_level = list_all_level()

# Clean up
# clean_data(train_set)

# RARE fix: reduce level of factor and support new value
# > agg = replace_agg_rare(agg,p_rare=0.1)

# Test Data
# d = head(train_set, 100)
# t = interpret_data(train_set)
# plot_aggregate_to_click(train_set)
# plot_aggregate_to_click_banner_C14(train_set)

# Train
# k = caret_train(train_set)
# m = train_model_intersect_prob(train_set)

# Proof
# proof_split_site_app_domain()

# = Train Columns =
# id
# click **
# hour
# C1
# banner_pos
# site_id
# site_domain
# site_category
# app_id
# app_domain
# app_category
# device_id
# device_ip
# device_model
# device_type
# device_conn_type
# C14
# C15
# C16
# C17
# C18
# C19
# C20
# C21


require(data.table)
library(caret)
library(ggplot2)

train_model_intersect_prob <- function(train_set, file_name = "intersect_prob", p_rare=0.1) {
    
    # validate if data is from web or app
    app_index = which(train_set$site_domain == "c4e18dd6")
    if (length(app_index) > 0) {
        data_source = "app"
    } else {
        data_source = "web"
    }
    
    file_name = paste("model",file_name, data_source, sep="_")
    file_name = paste(file_name, "RData", sep=".")
    file_name = paste("rdata", file_name, sep="/")
    
    cat("data_source = ")
    print(data_source)
    
    # refactoring from plot_aggregate_to_click
    if (data_source == "app") {
        col_list = c(
            "C1",
            "banner_pos",
    #         "site_id",
    #         "site_domain",
    #         "site_category",
    #         "app_id",
            "app_domain",
            "app_category",
    #         "device_id",
    #         "device_ip",
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
    } else {
        col_list = c(
            "C1",
            "banner_pos",
    #         "site_id",
            "site_domain",
            "site_category",
    #         "app_id",
    #         "app_domain",
    #         "app_category",
    #         "device_id",
    #         "device_ip",
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
    }
    
    all_row = nrow(train_set)
    
    cat("Rare if frequency <")
    cat(p_rare)
    print(" %")
    
    model = list()
    for (col_name in col_list) {
        print ( paste("Cal click -", col_name) )
        
        agg = aggregate(train_set$click, list(train_set[[col_name]]),   
                        FUN=function(x) c(
                            "avg" =mean(x) * 100, # return %
                            "count"  =length(x) / all_row * 100 # total rows
                        ) # return as matrix
        )
        #         return(agg)
        names(agg) = c(col_name, "summary")
        agg = agg[order(-agg$summary[,"avg"]),]
        
        agg = replace_agg_rare(agg)
        
        #         return(agg)
        
        # save to model as list()
        
        f = list()
        for(n in 1:nrow(agg)) {
            a = agg[n,]
            # f = convert_freq(a)
            # print(agg[1])
            
            f[[ agg[n, 1] ]] = agg[n, 2]
            
        }
        # to read avg
        # f["1005"][[1]][1]
        # or
        # a = f["1005"]
        # a[[1]][1]
        # or 
        # m$C21$RARE[1]
        
        model[[col_name]] = f
    }
    
    # If no key is matched, use total CTR
    model[["CTR"]] = mean(train_set$click)
    
    # save to file
    saveRDS(model, file = file_name)
    cat("Save model to ")
    print(file_name)
    
    model
}

create_dummy_var <- function(data) {
    # read levels from file
    all_level = readRDS("rdata/all_level.RData")
    
    # loop transform column data in to dummy var
    list_factors = c()
    for (col_name in colnames(data)) {
        # print(col_name)
        
        col_data = data[[col_name]]
        
        if ( !is.null(all_level[[col_name]])) {
            print( c(col_name , "HIT!") )
            
            # all levels
            levels(data[[col_name]]) = all_level[[col_name]]
            list_factors <- c(list_factors, col_name)
        }
        
    }
    list_factors
    # variable as formular
    f = as.formula(paste("~",paste(list_factors,collapse="+")))
    
    # keep "click" if Training set
    click = NULL
    if ( "click" %in% colnames(data)) {
        click = as.factor(data$click) # force caret to do classification
    }
    
    print ("-- start model.matrix")
    data = model.matrix(f, data) # replace instead of create new
    
    print ("-- convert to data.table")
    data = as.data.table(data)
    
    # return click if Training set
    if ( !is.null(click) ) {
        data = cbind(data, click)
    }

    data
}

# prove data is split into 2 group by site and app
# Mark NULL: site_domain = c4e18dd6, app_domain = 7801e8d9
proof_split_site_app_domain <- function(train_set) {
    site_index = which(train_set$site_domain == "c4e18dd6") # 15121739 entries
    cat("Site domain c4e18dd6 count = ")
    print (length(site_index))
    
    not_site  = train_set[-site_index,]
    cat("NOT site count = ")
    print (nrow(not_site))
    
    proof_index = which(not_site$app_domain == "7801e8d9")
    cat("App domain 7801e8d9 count =")
    print (length(proof_index))
    
    if(length(proof_index) == nrow(not_site)) {
        print("== Number not site match number of app ==")
        print("## proof data is splited between APP and SITE ##")
    } else {
        print("== no good! ==")
    }
}

plot_aggregate_to_click_banner_C14 <- function(train_set) {
    
    col_list = c(
        "banner_pos",
        "C14"        
    )
    
    agg = aggregate(train_set$click, list(train_set$banner_pos, train_set$C14),   
                    FUN=function(x) c(
                        "avg" =mean(x) * 100, # return %
                        "count"  =length(x) / 40428967 * 100 # total rows
                    ) # return as matrix
    )
    
    names(agg) = c("banner_pos", "C14", "summary")
    agg = agg[order(-agg$summary[,"avg"]),]
    
    # Plot
    file_name = "plot/aggregate_to_click_banner_C14.png"
    
    banner_pos_C14 = paste(agg$banner_pos, agg$C14, sep="-")
    agg = cbind(agg, banner_pos_C14)
    
#     print(agg)
#     agg

    ggplot(data=agg, aes_string(x="banner_pos_C14", y="summary[,'avg']", colour="summary[,'count']", size="summary[,'count']")) + 
        geom_point( alpha=0.7) +
        scale_colour_gradientn( colours=rainbow(7)) +
        #             scale_colour_gradient2(low="blue", high="red", mid="green", midpoint=0) +
        geom_hline(yintercept=80, colour="darkgreen", linetype = "longdash") +
        geom_hline(yintercept=5, colour="red", linetype = "longdash") +
        theme(
            panel.background=element_rect(fill="grey97")
        )
    
    ggsave(file=file_name, width=10, height=6)
}

plot_aggregate_to_click <- function(train_set) {
    # mean click by fields 
    
    col_list = c(
        
                "C1",
                "banner_pos",
#                 "site_id",
                "site_domain",
                "site_category",
#                 "app_id",
                "app_domain",
                "app_category",
#                 "device_id",
#                 "device_ip",
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
    
    
    # multiple
    # aggregate(train_set$click, list(train_set$banner_pos, train_set$app_category), mean  )
    
    all_row = nrow(train_set)
    
    # Single
    for (col_name in col_list) {
        print ( c("Aggregate click -", col_name), quote=FALSE )
        
        agg = aggregate(train_set$click, list(train_set[[col_name]]),   
                    FUN=function(x) c(
                        "avg" =mean(x) * 100, # return %
                        "count"  =length(x) / all_row * 100 # total rows
                    ) # return as matrix
            )
#         return(agg)
        names(agg) = c(col_name, "summary")
        agg = agg[order(-agg$summary[,"avg"]),]
        
        agg = replace_agg_rare(agg)
        
#         return(agg)
        print(agg)
        
        # Plot
        file_name = paste("aggregate_to_click",col_name,sep="-")
        file_name = paste(file_name,"png",sep=".")
        file_name = paste("plot",file_name,sep="/")
        
        ggplot(data=agg, aes_string(x=col_name, y="summary[,'avg']", colour="summary[,'count']", size="summary[,'count']")) + 
            geom_point( alpha=0.7) +
            scale_colour_gradientn( colours=rainbow(7)) +
#             scale_colour_gradient2(low="blue", high="red", mid="green", midpoint=0) +
            geom_hline(yintercept=80, colour="darkgreen", linetype = "longdash") +
            geom_hline(yintercept=5, colour="red", linetype = "longdash") +
            theme(
                panel.background=element_rect(fill="grey97")
            )

        ggsave(file=file_name, width=10, height=6)
        
    }
}

# Model list http://topepo.github.io/caret/modelList.html
caret_train <- function(data, model_seed=32343, data_model="simple", caret_model="glm") {
    set.seed(model_seed)
    
    # map field to model
    data = interpret_data(data, data_model, "train")
    
    inTrain = createDataPartition(y=data$click, p=0.1, list=FALSE)
    
    training = data[inTrain[,1],] # first column is row index
    testing  = data[-inTrain[,1],]
    
    ###
    summary(training)
    return (testing)
    
    cat ("Train data dimension ") 
    print (dim(training))
    
    cat ("Test data dimension ") 
    print (dim(testing))
    
    drops = c("click") # separate test result column
#     x = data[,!(names(data) %in% drops), with=FALSE]
    
    modelFit <- train(click ~.,data=t, method="glm") # ada
    
knnFit1 <- train(click ~.,data=d,
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))
    
    modelFit <- train(
                click ~.,
                data=training, 
                method = "nnet",
                preProcess = "range", 
                tuneLength = 2,
                trace = FALSE,
                maxit = 100
            )
    

    modelFit <- train(
                click ~.,
#                 data= as.data.frame.matrix(training), 
                data=training, 
                preProcess = "range", 
                tuneLength = 2,
#                 trace = FALSE,
                maxit = 50,
                method = "nnet"
            )
   
# call nnet directly
# nnetFit <- nnet(
#     click ~.,
#     data=training, 
# #     rang = 0.1,
# #     decay = 0.01, 
#     size = 10,
# #     softmax=TRUE,
# #     linout=TRUE,
#     entropy=TRUE,
#     maxit = 10
# )



# nnetFit <- nnet(
#     training[,1:( ncol(training)-1 ),with=F], 
#     training[,"click",with=F],
#     #     rang = 0.1,
#     #     decay = 0.01, 
#     size = 1,
#     #     softmax=TRUE,
#     #     linout=TRUE,
# #     entropy=TRUE,
#     maxit = 10
# )


#====================

    modelFit
    
    
    
    
    
    return(modelFit)
    
    
#     1
}

int_model_simple <- function(data, type) {
    # simple model
    # banner_pos, site_category, app_category, device_type, device_conn_type
    
    col_list = c(
        
#         "C1",
        "banner_pos",
#         "site_id",
#         "site_domain",
        "site_category",
#         "app_id",
#         "app_domain",
        "app_category"
#         "device_id",
#         "device_ip",
#         "device_model",
#         "device_type",
#         "device_conn_type",
#         "C14",
#         "C15",
#         "C16",
#         "C17",
#         "C18",
#         "C19",
#         "C20",
#         "C21"
    )
    
    if (type == "train") {
        col_list = c(col_list, "click")
    }
    
    data = data[, col_list, with=FALSE]
    
    # create Dummy Variables
    data = create_dummy_var(data)

    data
}

interpret_data <- function( data, model="simple", type="train") {
    print ( c("Model", type, model), quote=FALSE )
    if (model == "simple") {
        data = int_model_simple(data, type)
    } else {
        return (NULL)
    }
    
    data
}

read_data <- function(file_name, data_source="all", limit=0) {
    
    # column data type ny file name
    if (file_name == 'train') {
        col_class_list = c(
            "NULL",
            "numeric",
            "numeric",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character"
        )
    } else if (file_name == 'test') {
        # no click data
        col_class_list = c(
            "NULL",
            "numeric",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character",
            "character"
        )
    }
    
    # read data by type
    file_path = paste("Raw", file_name, sep = "/")
    
    #data = fread(file_path) # much faster
#     data = fread(file_path, header = TRUE, colClasses = col_class_list) # acceptable fast, 10 min
#     return(data)
    # end test
    
    if (limit) {
        data = fread(file_path, nrows=limit, colClasses = col_class_list,
                     sep=",", stringsAsFactors=FALSE)
    } else {
        data = fread(file_path, colClasses = col_class_list,
                     sep=",", stringsAsFactors=FALSE)
    } # acceptable fast, 10 min
    
    if (data_source == "web" ) {
        app_index = which(data$site_domain == "c4e18dd6") # site_domain "c4e18dd6" = marked as NULL
        data = data[-app_index,]
    } else if (data_source == "app") {
        app_index = which(data$site_domain == "c4e18dd6")
        data = data[app_index,]
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
        "hour",
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

list_all_level <- function(test_set,limit=0) {
    col_list = c(
        
        "C1",
        "banner_pos",
        #         "site_id",
        #         "site_domain",
        "site_category",
        #         "app_id",
        #         "app_domain",
        "app_category",
        #         "device_id",
        #         "device_ip",
        #         "device_model",
        "device_type",
        "device_conn_type",
        #         "C14",
        "C15",
        "C16",
        #         "C17",
        "C18"
        #         "C19",
        #         "C20",
        #         "C21"
    )
    
    # read train
    train_set = read_data('train',limit)
    
    print("Read train set")
    train_col = list_all_level_read_col(col_list, train_set)
    print(train_col)
    remove(train_set)
    
    #     return(train_col)
    
    # read test
    test_set = read_data('test',limit)
    print("Read test set")
    test_col = list_all_level_read_col(col_list, test_set)
    print(test_col)
    remove(test_set)
    
    # merge level list
    all_level = list()
    for (col_name in col_list) {
        print (paste("Merge", col_name))
        
        l = c(train_col[[col_name]], test_col[[col_name]])
        l = unique(l)
        
        # insert
        all_level[[col_name]] = l
    }
    
    # save to file
    all_level
}

list_all_level_read_col <- function(col_list, data) {
    l = list()
    for (col_name in col_list) {
        # print(col_name)
        
        col_data = data[[col_name]]
        col_data = unique(col_data) # only unique
        
        #print (length(data[[col_name]])) # count data
        print( c(col_name , length(col_data)) )
        
        # save to list (hash)
        l[[col_name]] = col_data
        
    }
    
    l
}

clean_data <- function (data) {
    # read data
    
    col_list = c(
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
    
    # remove low frequency
    # freq < 100 = rare
    # freq < 1,000 = low
    for (col_name in col_list) {
        print( paste("Clean low fequency" , col_name) )
        
        agg = aggregate(data[[col_name]], list(data[[col_name]]), length)
        names(agg) = c("col", "count")

        # select only column : agg[agg$count <= 100,  "col"]    
        rare_col = agg[agg$count < 10,  "col"]
        low_col  = agg[(agg$count < 1000 & agg$count >= 10) ,  "col"]
        
        cat("-- rare ")
        print(length(rare_col))
        cat("-- low ")
        print(length(low_col))
        
        
#         dtQuery[,newClose:=c(101,401)]
#         dtData[dtQuery,Close:=newClose]
#         return(low_col)
        
        # replace RARE
#         if(length(rare_col)) {
# #             data[data[[col_name]] == rare_col, col_name] = "RARE"
#             for(c_val in rare_col) {
#                 data[data[[col_name]] == c_val, col_name] = "RARE"
#             }
#         }
#         
#         # replace LOW
#         if(length(low_col)) {
#             for(c_val in low_col) {
#                 data[data[[col_name]] == c_val, col_name] = "LOW"
#             }
#         }
    }
    
    # rewrite to file
#     saveRDS(data, file = 'rdata/clean.RData',compress = F)
    return(NULL)
}

replace_agg_rare <- function(agg, p_rare=1) {
    
    rare_index = which(agg$summary[,"count"] <= p_rare)
    if (length(rare_index) == 0) {
        return(agg)
    }
    
    cat("---rare_index----")
    print(length(rare_index))
    
    agg_rare  = agg[rare_index,]
    agg_normal = agg[-rare_index,]
    
    # copy 1 row of agg because it's hard to recreate the structure
    rare_row = subset(agg[1,],TRUE)
    
    # change name to RARE
    rare_row[1,1] = "RARE"
    # cal average and sum %
    rare_row$summary[,"avg"] = mean(agg_rare$summary[,"avg"])
    rare_row$summary[,"count"] =sum(agg_rare$summary[,"count"])
    
    rbind(agg_normal, rare_row )
}
