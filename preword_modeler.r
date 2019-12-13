require(dplyr)
require(tidyr)
require(ggplot2)
require(sqldf)
require(data.table)

# Helper functions
readGramsFromDataFiles <- function(file, order, min_count=2){

    old_names <- paste0("V",1:(order+1))
    new_names <- c(paste0("word",1:order), "count")

    type    <- c(rep("character",order),"numeric")

    ngrams <- fread(input = file, sep = " ", header = FALSE, showProgress = TRUE, fill=TRUE)

    for (i in 1:(order+1)){
        setnames(ngrams, old_names[i], new_names[i],skip_absent=TRUE)
    }
    ngrams$count  <- as.integer(ngrams$count)

    ngrams <- ngrams[count>=min_count,]
    ngrams[, count:=count-1]

    return(ngrams)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# PART I. Read source files ####
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Source files consist of lines of text sampled from the Porject Gutenberg database of books.
# About 100mb
# Tokenization into 1 to 7-grams is done in previous script

startTime <- proc.time()
print(startTime)

# setwd('D:/_School/STAT400/pre')
ngram_files <- "data/"
lines1900 <- list()

lines1900$gram_1 <- readGramsFromDataFiles("data/uni_words.txt", order=1, min_count=2)
lines1900$gram_2 <- readGramsFromDataFiles("data/bi_words.txt", order=2, min_count=2)
lines1900$gram_3 <- readGramsFromDataFiles("data/tri_words.txt", order=3, min_count=2)
lines1900$gram_4 <- readGramsFromDataFiles("data/tetra_words.txt", order=4, min_count=2)
lines1900$gram_5 <- readGramsFromDataFiles("data/penta_words.txt", order=5, min_count=2)
lines1900$gram_6 <- readGramsFromDataFiles("data/hexa_words.txt", order=6, min_count=2)
lines1900$gram_7 <- readGramsFromDataFiles("data/hepta_words.txt", order=7, min_count=2)
print('Step2-1: read ngrams from datafiles')
print(proc.time() - startTime)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# PART II. Merge ngram tables & filter ####
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# We merge the above table by ngram order
# Then do some filtering keeping only alphabetic characters
# (still having difficulties handling properly unicodes with the combination windows/R/SQLite ...)
# Finally we keep only ngrams with count > 5 to limit memory footprint

gram_1 <- rbind(lines1900$gram_1) %>%
    group_by(word1) %>%
    summarise(count = sum(count)) %>%
    arrange(desc(count))
print('Step2-2.1')
print(proc.time() - startTime)

gram_2  <- rbind(lines1900$gram_2) %>%
    group_by_(.dots = lapply(c("word1","word2"), as.symbol)) %>%
    summarise(count = sum(count)) %>% ungroup %>%
    arrange(desc(count))
print('Step2-2.2')
print(proc.time() - startTime)

gram_3  <- rbind(lines1900$gram_3) %>%
    group_by_(.dots = lapply(c("word1","word2","word3"), as.symbol)) %>%
    summarise(count = sum(count)) %>% ungroup %>%
    arrange(desc(count))
print('Step2-2.3')
print(proc.time() - startTime)

gram_4  <- rbind(lines1900$gram_4) %>%
    group_by_(.dots = lapply(c("word1","word2","word3","word4"), as.symbol)) %>%
    summarise(count = sum(count)) %>% ungroup %>%
    arrange(desc(count))
print('Step2-2.4')
print(proc.time() - startTime)

gram_5  <- rbind(lines1900$gram_5) %>%
    group_by_(.dots = lapply(c("word1","word2","word3","word4","word5"), as.symbol)) %>%
    summarise(count = sum(count)) %>% ungroup %>%
    arrange(desc(count))
print('Step2-2.5')
print(proc.time() - startTime)

gram_6  <- rbind(lines1900$gram_6) %>%
    group_by_(.dots = lapply(c("word1","word2","word3","word4","word5","word6"), as.symbol)) %>%
    summarise(count = sum(count)) %>% ungroup %>%
    arrange(desc(count))
print('Step2-2.6')
print(proc.time() - startTime)

gram_7  <- rbind(lines1900$gram_7) %>%
    group_by_(.dots = lapply(c("word1","word2","word3","word4","word5","word6","word7"), as.symbol)) %>%
    summarise(count = sum(count)) %>% ungroup %>%
    arrange(desc(count))
print('Step2-2.7')
print(proc.time() - startTime)

# filter non alphabetic characters

pattern = "^[a-z]*$"
gram_1 <- gram_1 %>% filter(grepl(pattern = pattern, x = word1))
gram_2 <- gram_2 %>% filter(grepl(pattern = pattern, x = word1),
                            grepl(pattern = pattern, x = word2))
gram_3 <- gram_3 %>% filter(grepl(pattern = pattern, x = word1),
                            grepl(pattern = pattern, x = word2),
                            grepl(pattern = pattern, x = word3))
gram_4 <- gram_4 %>% filter(grepl(pattern = pattern, x = word1),
                            grepl(pattern = pattern, x = word2),
                            grepl(pattern = pattern, x = word3),
                            grepl(pattern = pattern, x = word4))
gram_5 <- gram_5 %>% filter(grepl(pattern = pattern, x = word1),
                            grepl(pattern = pattern, x = word2),
                            grepl(pattern = pattern, x = word3),
                            grepl(pattern = pattern, x = word4),
                            grepl(pattern = pattern, x = word5))
gram_6 <- gram_6 %>% filter(grepl(pattern = pattern, x = word1),
                            grepl(pattern = pattern, x = word2),
                            grepl(pattern = pattern, x = word3),
                            grepl(pattern = pattern, x = word4),
                            grepl(pattern = pattern, x = word5),
                            grepl(pattern = pattern, x = word6))
gram_7 <- gram_7 %>% filter(grepl(pattern = pattern, x = word1),
                            grepl(pattern = pattern, x = word2),
                            grepl(pattern = pattern, x = word3),
                            grepl(pattern = pattern, x = word4),
                            grepl(pattern = pattern, x = word5),
                            grepl(pattern = pattern, x = word6),
                            grepl(pattern = pattern, x = word7))

# Filter by count

threshold = 5

gram_1  <- gram_1  %>% filter(count > threshold)
gram_2  <- gram_2  %>% filter(count > threshold)
gram_3  <- gram_3  %>% filter(count > threshold)
gram_4  <- gram_4  %>% filter(count > threshold)
gram_5  <- gram_5  %>% filter(count > threshold)
gram_6  <- gram_6  %>% filter(count > threshold)
gram_7  <- gram_7  %>% filter(count > threshold)

gram_1$count <- gram_1$count - threshold
gram_2$count <- gram_2$count - threshold
gram_3$count <- gram_3$count - threshold
gram_4$count <- gram_4$count - threshold
gram_5$count <- gram_5$count - threshold
gram_6$count <- gram_6$count - threshold
gram_7$count <- gram_7$count - threshold

print('Step2-3: ngram filtering complete')
print(proc.time() - startTime)

# Backup files & clean

# save(gram_1, gram_2, gram_3, gram_4,  file = "data/ngrams.RData")
# rm(list=setdiff(ls(), c("gram_1", "gram_2", "gram_3", "gram_4")))

save(gram_1, gram_2, gram_3, gram_4, gram_5, gram_6, gram_7, file = "data/ngrams.RData")
rm(list=setdiff(ls(), c("gram_1", "gram_2", "gram_3", "gram_4", "gram_5", "gram_6", "gram_7")))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# PART III. Pre-compute probabilities ####
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# For each ngram table we compute the Maximul Likelihood estimates
# along with interpolated Kneyser-Ney propabilities (only the latter is used in final model)
# Computations are done using merging of data tables, which is the fatest wayI found so far (whole process should take just a few seconds)
# note: fixes related to start/end of sentence tags are only relevant if the latter are used, which is not the case here.

gram_1 = data.table(as.data.table(gram_1))
gram_2 = data.table(as.data.table(gram_2))
gram_3 = data.table(as.data.table(gram_3))
gram_4 = data.table(as.data.table(gram_4))
gram_5 = data.table(as.data.table(gram_5))
gram_6 = data.table(as.data.table(gram_6))
gram_7 = data.table(as.data.table(gram_7))


# set tables keys
setkey(gram_1, word1)
setkey(gram_2, word1, word2)
setkey(gram_3, word1, word2, word3)
setkey(gram_4, word1, word2, word3, word4)
setkey(gram_5, word1, word2, word3, word4, word5)
setkey(gram_6, word1, word2, word3, word4, word5, word6)
setkey(gram_7, word1, word2, word3, word4, word5, word6, word7)

print('Step2-4: setkeys for ngrams')
# print(proc.time() - startTime)

# Compute some global variables
totalCounts <- sapply(list(gram_1,gram_2,gram_3,gram_4,gram_4,gram_6,gram_7), function(x) sum(x$count))
gramCounts  <- sapply(list(gram_1,gram_2,gram_3,gram_4,gram_5,gram_6,gram_7), nrow)

# Maximum Likelihood Estimate
gram_1[, qML := count/totalCounts[1]]
#gram_1["<s>",]$qML <- 0  # optional fix if start tags are used

gram_2[, qML := count/sum(count), by = word1]
gram_3[, qML := count/sum(count), by = .(word1,word2)]
gram_4[, qML := count/sum(count), by = .(word1,word2,word3)]
gram_5[, qML := count/sum(count), by = .(word1,word2,word3,word4)]
gram_6[, qML := count/sum(count), by = .(word1,word2,word3,word4,word5)]
gram_7[, qML := count/sum(count), by = .(word1,word2,word3,word4,word5,word6)]

print('Step2-5: ngram max likelihood')
# print(proc.time() - startTime)

# Kneyser-Ney smoothed probabilities (max order is 4)

# a - continuation count N01 (up to max order - 1)

gram_1 <- merge(gram_1, gram_2[, .(N01 = .N), by=word2],
                by.x="word1", by.y="word2", all.x = TRUE)
gram_2 <- merge(gram_2, gram_3[, .(N01 = .N), by=.(word2,word3)],
                by.x=c("word1","word2"), by.y=c("word2","word3"), all.x = TRUE)
gram_3 <- merge(gram_3, gram_4[, .(N01 = .N), by=.(word2,word3,word4)],
                by.x=c("word1","word2","word3"), by.y=c("word2","word3","word4"), all.x = TRUE)
gram_4 <- merge(gram_4, gram_5[, .(N01 = .N), by=.(word2,word3,word4,word5)],
                by.x=c("word1","word2","word3","word4"), by.y=c("word2","word3","word4","word5"), all.x = TRUE)
gram_5 <- merge(gram_5, gram_6[, .(N01 = .N), by=.(word2,word3,word4,word5,word6)],
                by.x=c("word1","word2","word3","word4","word5"), by.y=c("word2","word3","word4","word5","word6"), all.x = TRUE)
gram_6 <- merge(gram_6, gram_7[, .(N01 = .N), by=.(word2,word3,word4,word5,word6,word7)],
                by.x=c("word1","word2","word3","word4","word5","word6"), by.y=c("word2","word3","word4","word5","word6","word7"), all.x = TRUE)
print('Step2-5: kneyser-ney smoothing probabilities complete')
# print(proc.time() - startTime)

gram_1["<s>", N01:=count] # use real counts for sequences starting with <s>
gram_2["<s>", N01:=count]
gram_3["<s>", N01:=count]
gram_4["<s>", N01:=count]
gram_5["<s>", N01:=count]
gram_6["<s>", N01:=count]
# gram_7["<s>", N01:=count]

gram_1[is.na(N01), N01:=1] # quick fix (NA appear because of filtering ngrams by count)
gram_2[is.na(N01), N01:=1]
gram_3[is.na(N01), N01:=1]
gram_4[is.na(N01), N01:=1]
gram_5[is.na(N01), N01:=1]
gram_6[is.na(N01), N01:=1]
# gram_7[is.na(N01), N01:=1]

# b - continuation count N10 (up to max order - 1)

gram_1 <- merge(gram_1, gram_2[, .(N10 = .N), by=word1], all.x = TRUE)
gram_2 <- merge(gram_2, gram_3[, .(N10 = .N), by=.(word1,word2)], all.x = TRUE)
gram_3 <- merge(gram_3, gram_4[, .(N10 = .N), by=.(word1,word2,word3,word4)], all.x = TRUE)
gram_4 <- merge(gram_4, gram_5[, .(N10 = .N), by=.(word1,word2,word3,word4,word5)], all.x = TRUE)
gram_5 <- merge(gram_5, gram_6[, .(N10 = .N), by=.(word1,word2,word3,word4,word5,word6)], all.x = TRUE)
gram_6 <- merge(gram_6, gram_7[, .(N10 = .N), by=.(word1,word2,word3,word4,word5,word6,word7)], all.x = TRUE)

gram_1[word1 == "</s>", N10:=1]  # assume </s> can only be followed by a start tag
gram_2[word2 == "</s>", N10:=1]
gram_3[word3 == "</s>", N10:=1]
gram_4[word4 == "</s>", N10:=1]
gram_5[word5 == "</s>", N10:=1]
gram_6[word6 == "</s>", N10:=1]

gram_1[is.na(N10), N10:=1] # quick fix (NA appear because of filtering ngrams by count)
gram_2[is.na(N10), N10:=1]
gram_3[is.na(N10), N10:=1]
gram_4[is.na(N10), N10:=1]
gram_5[is.na(N10), N10:=1]
gram_6[is.na(N10), N10:=1]

# c - continuation count N010 (up to max order - 2)

gram_1 <- merge(gram_1, gram_2[, .(N010 = sum(N01)), by=word1], all.x = TRUE)
gram_2 <- merge(gram_2, gram_3[, .(N010 = sum(N01)), by=.(word1,word2)], all.x = TRUE)
gram_3 <- merge(gram_3, gram_4[, .(N010 = sum(N01)), by=.(word1,word2,word3)], all.x = TRUE)
gram_4 <- merge(gram_4, gram_5[, .(N010 = sum(N01)), by=.(word1,word2,word3,word4)], all.x = TRUE)
gram_5 <- merge(gram_5, gram_6[, .(N010 = sum(N01)), by=.(word1,word2,word3,word4,word5)], all.x = TRUE)

gram_1[word1 == "</s>", N010:=N01]  # assume N010 = N01 since N10 is always 1 in this case
gram_2[word2 == "</s>", N010:=N01]
gram_3[word3 == "</s>", N010:=N01]
gram_4[word4 == "</s>", N010:=N01]
gram_5[word5 == "</s>", N010:=N01]

# d - discount values (fixed discount here, could also implement 'modified' kneyser-Ney by tweeking this part)

n11 <- gram_1[count=="1", .N]
n12 <- gram_2[count=="1", .N]
n13 <- gram_3[count=="1", .N]
n14 <- gram_4[count=="1", .N]
n15 <- gram_5[count=="1", .N]
n16 <- gram_6[count=="1", .N]
n17 <- gram_7[count=="1", .N]

n21 <- gram_1[count=="2", .N]
n22 <- gram_2[count=="2", .N]
n23 <- gram_3[count=="2", .N]
n24 <- gram_4[count=="2", .N]
n25 <- gram_5[count=="2", .N]
n26 <- gram_6[count=="2", .N]
n27 <- gram_7[count=="2", .N]

D <- unlist(c(n11/(n11+2*n21),
              n12/(n12+2*n22),
              n13/(n13+2*n23),
              n14/(n14+2*n24),
              n15/(n15+2*n25),
              n16/(n16+2*n26),
              n17/(n17+2*n27)))

print('Step2-6: smoothing and counting complete')
# print(proc.time() - startTime)

# e - Compute the probabilities ( first compute lowest/higher/highest order results and then chain up)

# lowest order result
KN_low <- gram_1[, .(word1, qML, PKN=N01/sum(gram_1$N01, na.rm = TRUE))]

# higher order results
KN_higher2 <- merge(gram_2[, .(word1, word2, N01)], gram_1[, .(word1, N010, N10)])[
    , .(word1, word2, alpha = pmax(N01 - D[2], 0)/N010, gamma = D[2]*N10/N010)]

KN_higher3 <- merge(gram_3[, .(word1, word2, word3, N01)], gram_2[, .(word1, word2, N010, N10)])[
    , .(word1, word2, word3, alpha = pmax(N01 - D[3], 0)/N010, gamma = D[3]*N10/N010)]

#special need for 'allow.cartesian=TRUE' for some stupid reason
KN_higher4 <- merge(gram_4[, .(word1, word2, word3, word4, N01)], gram_3[, .(word1, word2, word3, N010, N10)], allow.cartesian=TRUE)[
    , .(word1, word2, word3, word4, alpha = pmax(N01 - D[4], 0)/N010, gamma = D[4]*N10/N010)]

KN_higher5 <- merge(gram_5[, .(word1, word2, word3, word4, word5, N01)], gram_4[, .(word1, word2, word3, word4, N010, N10)])[
    , .(word1, word2, word3, word4, word5, alpha = pmax(N01 - D[5], 0)/N010, gamma = D[5]*N10/N010)]

KN_higher6 <- merge(gram_6[, .(word1, word2, word3, word4, word5, word6, N01)], gram_5[, .(word1, word2, word3, word4, word5, N010, N10)])[
    , .(word1, word2, word3, word4, word5, word6, alpha = pmax(N01 - D[6], 0)/N010, gamma = D[6]*N10/N010)]


# highest order results
KN_highest2 <- merge(gram_2[, .(word1, word2, total_count = count)], gram_1[, .(word1, count, N10)])[
    , .(word1, word2, alpha = pmax(total_count - D[2], 0)/count, gamma = D[2]*N10/count)]

KN_highest3 <- merge(gram_3[, .(word1, word2, word3, total_count = count)], gram_2[, .(word1, word2, count, N10)])[
    , .(word1, word2, word3, alpha = pmax(total_count - D[3], 0)/count, gamma = D[3]*N10/count)]

#special need for 'allow.cartesian=TRUE' for some stupid reason
KN_highest4 <- merge(gram_4[, .(word1, word2, word3, word4, total_count = count)], gram_3[, .(word1, word2, word3, count, N10)], allow.cartesian=TRUE)[
    , .(word1, word2, word3, word4, alpha = pmax(total_count - D[4], 0)/count, gamma = D[4]*N10/count)]

KN_highest5 <- merge(gram_5[, .(word1, word2, word3, word4, word5, total_count = count)], gram_4[, .(word1, word2, word3, word4, count, N10)])[
    , .(word1, word2, word3, word4, word5, alpha = pmax(total_count - D[5], 0)/count, gamma = D[5]*N10/count)]

KN_highest6 <- merge(gram_6[, .(word1, word2, word3, word4, word5, word6, total_count = count)], gram_5[, .(word1, word2, word3, word4, word5, count, N10)])[
    , .(word1, word2, word3, word4, word5, word6, alpha = pmax(total_count - D[6], 0)/count, gamma = D[6]*N10/count)]

KN_highest7 <- merge(gram_7[, .(word1, word2, word3, word4, word5, word6, word7, total_count = count)], gram_6[, .(word1, word2, word3, word4, word5, word6, count, N10)])[
    , .(word1, word2, word3, word4, word5, word6, word7, alpha = pmax(total_count - D[7], 0)/count, gamma = D[7]*N10/count)]

# chain up from lowest to highest level
# (intermediate 'temp' calculations aim at avoiding recomputing the same results)

gram_1_final <- gram_1[, .(word1, qML, PKN = qML)]

gram_2_final <- merge(KN_highest2, KN_low[, .(word1, PKN)], by.x = "word2", by.y = "word1")[
    , .(word1, word2, PKN = alpha + gamma*PKN)]

temp2 <- merge(KN_higher2, KN_low[, .(word1, PKN)], by.x = "word2", by.y = "word1")[, .(word1, word2, PKN = alpha + gamma*PKN)]

gram_3_final <- merge(KN_highest3, temp2[, .(word1,word2, PKN)], by.x = c("word2","word3"), by.y = c("word1","word2"))[
    , .(word1, word2, word3, PKN = alpha + gamma*PKN)]

temp3 <- merge(KN_higher3, temp2[, .(word1,word2, PKN)], by.x = c("word2","word3"), by.y = c("word1","word2"))[
    , .(word1, word2, word3, PKN = alpha + gamma*PKN)]

#special need for 'allow.cartesian=TRUE' for some stupid reason
gram_4_final <- merge(KN_highest4, temp3[, .(word1,word2,word3, PKN)], by.x = c("word2","word3","word4"), by.y = c("word1","word2","word3"), allow.cartesian=TRUE)[
    , .(word1, word2, word3, word4, PKN = alpha + gamma*PKN)]

#special need for 'allow.cartesian=TRUE' for some stupid reason
temp4 <- merge(KN_higher4, temp3[, .(word1,word2,word3, PKN)], by.x = c("word2","word3","word4"), by.y = c("word1","word2","word3"), allow.cartesian=TRUE)[
    , .(word1, word2, word3, word4, PKN = alpha + gamma*PKN)]

#special need for 'allow.cartesian=TRUE' for some stupid reason
gram_5_final <- merge(KN_highest5, temp4[, .(word1,word2,word3,word4, PKN)], by.x = c("word2","word3","word4","word5"), by.y = c("word1","word2","word3","word4"), allow.cartesian=TRUE)[
    , .(word1, word2, word3, word4, word5, PKN = alpha + gamma*PKN)]

#special need for 'allow.cartesian=TRUE' for some stupid reason
temp5 <- merge(KN_higher5, temp4[, .(word1,word2,word3,word4, PKN)], by.x = c("word2","word3","word4","word5"), by.y = c("word1","word2","word3","word4"), allow.cartesian=TRUE)[
    , .(word1, word2, word3, word4, word5, PKN = alpha + gamma*PKN)]

#special need for 'allow.cartesian=TRUE' for some stupid reason
gram_6_final <- merge(KN_highest6, temp5[, .(word1,word2,word3,word4,word5, PKN)], by.x = c("word2","word3","word4","word5","word6"), by.y = c("word1","word2","word3","word4","word5"), allow.cartesian=TRUE)[
    , .(word1, word2, word3, word4, word5, word6, PKN = alpha + gamma*PKN)]

#special need for 'allow.cartesian=TRUE' for some stupid reason
temp6 <- merge(KN_higher6, temp5[, .(word1,word2,word3,word4,word5, PKN)], by.x = c("word2","word3","word4","word5","word6"), by.y = c("word1","word2","word3","word4","word5"), allow.cartesian=TRUE)[
    , .(word1, word2, word3, word4, word5, word6, PKN = alpha + gamma*PKN)]

#special need for 'allow.cartesian=TRUE' for some stupid reason
gram_7_final <- merge(KN_highest7, temp6[, .(word1,word2,word3,word4,word5,word6, PKN)], by.x = c("word2","word3","word4","word5","word6","word7"), by.y = c("word1","word2","word3","word4","word5","word6"), allow.cartesian=TRUE)[
    , .(word1, word2, word3, word4, word5, word6, word7, PKN = alpha + gamma*PKN)]

# and we end up with 4 interpolated KN models, one for each ngram order

# backup
save(gram_1_final, gram_2_final, gram_3_final, gram_4_final, gram_5_final, gram_6_final, gram_7_final, file = "data/ngrams_processed.RData")
# save(gram_1_final, gram_2_final, gram_3_final,  file = "data/ngrams_processed.RData")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# PART III. Store in Database ####
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

database = "data/NWP.SQLite"

conn <- dbConnect(SQLite(), database)

dbWriteTable(conn, "gram_1", value = as.data.frame(gram_1_final), row.names = FALSE, overwrite=TRUE)
dbWriteTable(conn, "gram_2", value = as.data.frame(gram_2_final), row.names = FALSE, overwrite=TRUE)
dbWriteTable(conn, "gram_3", value = as.data.frame(gram_3_final), row.names = FALSE, overwrite=TRUE)
dbWriteTable(conn, "gram_4", value = as.data.frame(gram_4_final), row.names = FALSE, overwrite=TRUE)
dbWriteTable(conn, "gram_5", value = as.data.frame(gram_5_final), row.names = FALSE, overwrite=TRUE)
dbWriteTable(conn, "gram_6", value = as.data.frame(gram_6_final), row.names = FALSE, overwrite=TRUE)
dbWriteTable(conn, "gram_7", value = as.data.frame(gram_7_final), row.names = FALSE, overwrite=TRUE)

cat("Database updated, now has the following tables: ",dbListTables(conn),"\n")
dbDisconnect(conn)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# PART IV. Run predictor interface ####
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

endTime <- proc.time()
print('endTime: ')
print(endTime)

source("preword_interface.r")
