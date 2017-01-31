veg.type.A.calc <- function(veg.type){
    veg.type <- as.numeric(substr(as.character(veg.type),1,2))
    sa <- matrix(c(
        11 , 'PoorDry',  
        13 , 'MediumDry', 
        21 , 'RichDry',  
        22 , 'RichDry',  
        42 , 'RichDry',  
        44 , 'RichDry',  
        45 , 'RichDry',
        
        12 , 'PoorMedium', 
        14 , 'MediumMedium', 
        41 , 'MediumMedium',
        43 , 'MediumMedium',
        26 , 'MediumMedium',
        81 , 'MediumMedium',
        
        15 , 'RichMedium', 
        24 , 'RichMedium', 
        46 , 'RichMedium', 
        61 , 'PoorWet',
        
        
        51 , 'MediumWet',#?
        52 , 'MediumWet',
        34 , 'MediumWet',
        
        54 , 'RichWet',  
        16 , 'RichWet',  
        33 , 'RichWet',  
        31 , 'RichWet',
        72 , 'PoorWet',
        73 , 'RichWet'),
                 ncol=2,byrow=TRUE)            
    class.1 <- data.frame(code  = sa[,1],
                          name  = sa[,2])
    veg.type.A <- class.1$name[match(veg.type, class.1$code)]
    return(veg.type.A)
}

