require(tools)
require(magrittr)
require(dplyr)
require(tidyr)


#-----------------------------------------------------------[ Download dataset ]

files <- data.frame(
    name = c("cup98lrn.zip", "cup98dic.txt"),
    url = c("http://kdd.ics.uci.edu/databases/kddcup98/epsilon_mirror/cup98lrn.zip",
            "http://kdd.ics.uci.edu/databases/kddcup98/epsilon_mirror/cup98dic.txt"),
    md5sum = c("929c96c6a9c180f7b9c9574a6741bb11", "38afb4fb7f242359d6adc922bc2bec53"),
    stringsAsFactors = FALSE
)
for(i in which(!file.exists(files$name))){
    download.file(files$url[i], files$name[i])
    if(grepl("\\.zip$", files$name[i]))
        unzip(files$name[i])
}
stopifnot(all(files$md5sum == md5sum(files$name)))

donation <- read.csv("cup98LRN.txt", skipNul = TRUE)


#--------------------------------------------------------------------[ Tidy up ]

# Convert integer date fields to number of moths since 1900
date.fields <- c("DOB", grep("DATE", names(donation), value=TRUE))
donation[date.fields] <- lapply(donation[date.fields], 
    function(x) x %/% 100 * 12 + x %% 100)

# Convert boolean fields to logicals
boolean.fields <- names(donation)[sapply(donation,
    function(x) is.factor(x) && identical(levels(x), c(" ", "X")))]
donation[boolean.fields] <- lapply(donation[boolean.fields],
    function(x) x == "X")

donation %<>%
    mutate(BadAddress = MAILCODE == "B") %>%
    select(-MAILCODE)

# Split compound fields into multiple factors and integers
donation %<>%
    mutate(MDMAUD = as.character(MDMAUD)) %>%
    extract(MDMAUD, c("RecencyOfGiving", "FrequencyOfGiving", "AmountOfGiving", "MajorDonor"),
            "^(\\w)(\\w)(\\w)(\\w)$") %>%
    mutate(RecencyOfGiving = factor(RecencyOfGiving, c("C", "L", "I", "D"),
                                    c("current", "lapsed", "inactive", "dormant")),
           FrequencyOfGiving = factor(FrequencyOfGiving, c(1,2,5), ordered=TRUE),
           AmountOfGiving = factor(AmountOfGiving, c("L", "C", "M", "T"),
                                   c("<$100", "$100-499", "$500-999", ">$999"), ordered=TRUE))

donation %<>%
    mutate(DOMAIN = ifelse(DOMAIN == " ", "X0", as.character(DOMAIN))) %>%
    extract(DOMAIN, c("Urbanicity", "NeighborhoodSES"), "(\\w)(\\d)") %>%
    mutate(Urbanicity = factor(Urbanicity, c("U", "C", "S", "T", "R"),
               c("urban", "city", "suburban", "town", "rural"), ordered=TRUE),
           NeighborhoodSES = factor(c(1,3,5,1,2,4,5)[as.integer(NeighborhoodSES)
               + 3*(Urbanicity == "urban")], 1:5, ordered=TRUE))

donation %<>%
    mutate(CLUSTER = factor(CLUSTER),
           AGEFLAG = factor(AGEFLAG, c("E", "I"), c("exact", "inferred")),
           HOMEOWNR = c(` `=FALSE, H=TRUE, U=NA)[HOMEOWNR],
           GENDER = factor(GENDER, c("A", "C", "F", "J", "M")),
           DATASRCE = factor(DATASRCE, 1:3, c("MetroMail", "Polk", "Both")),
           CanBeMailed = is.na(SOLP3),
           GEOCODE = factor(GEOCODE, ordered=TRUE),
           GEOCODE2 = factor(GEOCODE, LETTERS[1:4], ordered=TRUE))

interest.fields <- c("COLLECT1", "VETERANS", "BIBLE", "CATLG", "HOMEE", "PETS",
                     "CDPLAY", "STEREO", "PCOWNERS", "PHOTO", "CRAFTS",
                     "FISHER", "GARDENIN", "BOATS", "WALKER", "KIDSTUFF",
                     "CARDS", "PLATES")
donation[interest.fields] <- lapply(donation[interest.fields], "==", "Y")

donation %<>%
    mutate(LIFESRC, factor(LIFESRC, 1:3))


#--------------------------------------------------[ Tidy up promotion history ]

RFA.columns <- grep("^RFA_\\d{1,2}$", names(donation), value=TRUE)

RFA <- donation[RFA.columns] %>%
    mutate(id = row_number()) %>%
    gather_("PromotionCode", "Value", RFA.columns) %>%
    mutate(PromotionCode = factor(sub("^RFA_", "_", PromotionCode),
                                  paste("_", 2:24, sep="")))

RFA.value <- vapply(strsplit(RFA$Value, ""), function(x){
    if(length(x) == 3) x
    else if(length(x) == 2) c(NA_character_, x)
    else if(identical(x, " ")) rep(NA_character_, 3)
    else stop(paste("Problem with", str(x)))
}, character(3))

RFA <- data.frame(
    RFA[c("id", "PromotionCode")],
    Recency = factor(RFA.value[1,], c("F", "N", "A", "L", "I", "S"),
                     c("first", "new", "active", "lapsing", "inactive", "star")),
    Frequency = as.integer(RFA.value[2,]),
    Amount = factor(RFA.value[3,], c(LETTERS[1:7]), 
                    c(sprintf("$%.2f-%.2f", c(.01, 2, 3, 5, 10, 15),
                      c(2, 3, 5, 10, 15, 25)-.01), ">=$25"), ordered=TRUE)
)

RFA <- split(RFA, RFA$PromotionCode)
RFA <- lapply(RFA, function(x){
    names(x) <- sub("(Recency|Frequency|Amount)",
                    paste("\\1", x$PromotionCode[1], sep=""), names(x))
    x %>% select(-PromotionCode)
})
RFA.id <- lapply(RFA, "[[", "id")
stopifnot(sapply(RFA.id, function(x) identical(x, RFA.id[[1]])))
RFA <- do.call(cbind, unname(lapply(RFA, "[", -1)))

donation <- cbind(donation[setdiff(names(donation), RFA.columns)], RFA)


#-----------------------------------------------------------------------[ Save ]

save(donation, file="KDD98.Rdata")

