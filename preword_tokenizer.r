require(quanteda)
require(data.table)
require(rlist)
# library(ggplot2)

startTime <- proc.time()
print('StartTime: ')
print(startTime)

# open and read data file for lines
con <- file('samples/lines/lines1900/1.csv', 'r')
lines1900 = readLines(con)
con <- file('samples/lines/lines1900/2.csv', 'r')
lines1900 = list.append(lines1900, readLines(con))
con <- file('samples/lines/lines1900/3.csv', 'r')
lines1900 = list.append(lines1900, readLines(con))
con <- file('samples/lines/lines1900/4.csv', 'r')
lines1900 = list.append(lines1900, readLines(con))
con <- file('samples/lines/lines1900/5.csv', 'r')
lines1900 = list.append(lines1900, readLines(con))
con <- file('samples/lines/lines1900/6.csv', 'r')
lines1900 = list.append(lines1900, readLines(con))
con <- file('samples/lines/lines1900/7.csv', 'r')
lines1900 = list.append(lines1900, readLines(con))
con <- file('samples/lines/lines1900/8.csv', 'r')
lines1900 = list.append(lines1900, readLines(con))
con <- file('samples/lines/lines1900/9.csv', 'r')
lines1900 = list.append(lines1900, readLines(con))
write.table(as.data.frame(lines1900), "samples/lines/lines1900/all.csv", append = FALSE, quote=FALSE, sep = " ", dec = ".",
             row.names = FALSE, col.names = FALSE)
close(con)
print('Step1: Opened datafile(s) complete')
print(proc.time() - startTime)


# creates 10% sample of time period lines file
sampleHolder1900 <- sample(length(lines1900), length(lines1900) * 0.5)
# print(head(sampleHolder1900))
lines1900 <- lines1900[sampleHolder1900]

#creates master vector and corpus
master_vector <- c(lines1900)
corp <- corpus(master_vector)
print('Step2: Create master vector and corpus complete')
print(proc.time() - startTime)

# saveRDS(corp, "_temp/corp.RDS")
# corp <- readRDS("_temp/corp.RDS")

# profanityFileName <- "profanity.txt"
# profanity <- read.csv(profanityFileName, header = FALSE, stringsAsFactors = FALSE)
# profanity[,1][1:5]

master_Tokens <- tokens(
    x = tolower(corp),
    remove_punct = TRUE,
    remove_twitter = TRUE,
    remove_numbers = TRUE,
    remove_hyphens = TRUE,
    remove_symbols = TRUE,
    remove_url = TRUE
)
# print(head(master_tokens))
print('Step3: Master tokens function complete')
print(proc.time() - startTime)

bi_gram <- tokens_ngrams(master_Tokens, n = 2)
tri_gram <- tokens_ngrams(master_Tokens, n = 3)
tetra_gram <- tokens_ngrams(master_Tokens, n = 4)
penta_gram <- tokens_ngrams(master_Tokens, n = 5)
hexa_gram <- tokens_ngrams(master_Tokens, n = 6)
hepta_gram <- tokens_ngrams(master_Tokens, n = 7)
# print(head(hepta_gram))
print('Step4: Create n-grams complete')
print(proc.time() - startTime)


# saveRDS(master_Tokens, "_temp/master_Tokens")
# saveRDS(bi_gram, "_temp/bi_gram.RDS")
# saveRDS(tri_gram, "_temp/tri_gram.RDS")

uni_DFM <- dfm(master_Tokens)
bi_DFM <- dfm(bi_gram)
tri_DFM <- dfm(tri_gram)
tetra_DFM <- dfm(tetra_gram)
penta_DFM <- dfm(penta_gram)
hexa_DFM <- dfm(hexa_gram)
hepta_DFM <- dfm(hepta_gram)
print(head(hepta_DFM))
print('Step5: Create document-feature matrices complete')
print(proc.time() - startTime)

# saveRDS(uni_DFM, "_temp/uni_DFM.RDS")
# saveRDS(bi_DFM, "_temp/bi_DFM.RDS")
# saveRDS(tri_DFM, "_temp/tri_DFM.RDS")

uni_DFM <- dfm_trim(uni_DFM, 3)
bi_DFM <- dfm_trim(bi_DFM, 3)
tri_DFM <- dfm_trim(tri_DFM, 3)
tetra_DFM <- dfm_trim(tetra_DFM, 3)
penta_DFM <- dfm_trim(penta_DFM, 3)
hexa_DFM <- dfm_trim(hexa_DFM, 3)
hepta_DFM <- dfm_trim(hepta_DFM, 3)
print(head(hepta_DFM))
print('Step6: trim document-feature matrices complete')
print(proc.time() - startTime)

# saveRDS(uni_DFM, "_temp/uni_DFMtrim.RDS")
# saveRDS(bi_DFM, "_temp/bi_DFMtrim.RDS")
# saveRDS(tri_DFM, "_temp/tri_DFMtrim.RDS")
#
# uni_DFM <- readRDS("_temp/uni_DFMtrim.RDS")
# bi_DFM <- readRDS("_temp/bi_DFMtrim.RDS")
# tri_DFM <- readRDS("_temp/tri_DFMtrim.RDS")

# Create named vectors with counts of words
sums_U <- colSums(uni_DFM)
sums_B <- colSums(bi_DFM)
sums_T <- colSums(tri_DFM)
sums_R <- colSums(tetra_DFM)
sums_P <- colSums(penta_DFM)
sums_X <- colSums(hexa_DFM)
sums_H <- colSums(hepta_DFM)
print(head(hepta_DFM))
print('Step7: Sum DFMs complete')
print(proc.time() - startTime)

index1 <- grepl("eeoss|nnum|aabrr", x = names(sums_U))
sums_U <- sums_U[!index1]
# print(proc.time() - startTime)


index2 <- grepl("eeoss|nnum|aabrr", x = names(sums_B))
sums_B <- sums_B[!index2]
# take a loop
sums_B[sample(1:length(sums_B), 10)]
# print(proc.time() - startTime)


index3 <- grepl("eeoss|nnum|aabrr", x = names(sums_T))
sums_T <- sums_T[!index3]
# take a loop
sums_T[sample(1:length(sums_T), 10)]
# print(proc.time() - startTime)


index4 <- grepl("eeoss|nnum|aabrr", x = names(sums_R))
sums_R <- sums_R[!index4]
# take a loop
sums_R[sample(1:length(sums_R), 10)]
# print(proc.time() - startTime)


index5 <- grepl("eeoss|nnum|aabrr", x = names(sums_P))
sums_P <- sums_P[!index5]
# take a loop
sums_P[sample(1:length(sums_P), 10)]
# print(proc.time() - startTime)


index6 <- grepl("eeoss|nnum|aabrr", x = names(sums_X))
sums_X <- sums_X[!index6]
# take a loop
sums_X[sample(1:length(sums_X), 10)]
# print(proc.time() - startTime)


index7 <- grepl("eeoss|nnum|aabrr", x = names(sums_H))
sums_H <- sums_H[!index7]
# take a loop
sums_H[sample(1:length(sums_H), 10)]

print('Step8: DFM sums indexes complete')
print(proc.time() - startTime)


# Requires packages

suppressPackageStartupMessages(library(data.table))

# Create data tables with individual words as columns
uni_words <- data.table(word_1 = names(sums_U), count = sums_U)
print('Step9.1')
print(proc.time() - startTime)

bi_words <- data.table(
        word_1 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[', 1),
        word_2 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[', 2),
        count = sums_B)
print('Step9.2')
print(proc.time() - startTime)

tri_words <- data.table(
        word_1 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 1),
        word_2 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 2),
        word_3 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 3),
        count = sums_T)
print('Step9.3')
print(proc.time() - startTime)

tetra_words <- data.table(
        word_1 = sapply(strsplit(names(sums_R), "_", fixed = TRUE), '[[', 1),
        word_2 = sapply(strsplit(names(sums_R), "_", fixed = TRUE), '[[', 2),
        word_3 = sapply(strsplit(names(sums_R), "_", fixed = TRUE), '[[', 3),
        word_4 = sapply(strsplit(names(sums_R), "_", fixed = TRUE), '[[', 4),
        count = sums_R)
print('Step9.4')
print(proc.time() - startTime)

penta_words <- data.table(
        word_1 = sapply(strsplit(names(sums_P), "_", fixed = TRUE), '[[', 1),
        word_2 = sapply(strsplit(names(sums_P), "_", fixed = TRUE), '[[', 2),
        word_3 = sapply(strsplit(names(sums_P), "_", fixed = TRUE), '[[', 3),
        word_4 = sapply(strsplit(names(sums_P), "_", fixed = TRUE), '[[', 4),
        word_5 = sapply(strsplit(names(sums_P), "_", fixed = TRUE), '[[', 5),
        count = sums_P)
print('Step9.5')
print(proc.time() - startTime)

hexa_words <- data.table(
        word_1 = sapply(strsplit(names(sums_X), "_", fixed = TRUE), '[[', 1),
        word_2 = sapply(strsplit(names(sums_X), "_", fixed = TRUE), '[[', 2),
        word_3 = sapply(strsplit(names(sums_X), "_", fixed = TRUE), '[[', 3),
        word_4 = sapply(strsplit(names(sums_X), "_", fixed = TRUE), '[[', 4),
        word_5 = sapply(strsplit(names(sums_X), "_", fixed = TRUE), '[[', 5),
        word_6 = sapply(strsplit(names(sums_X), "_", fixed = TRUE), '[[', 6),
        count = sums_X)
print('Step9.6')
print(proc.time() - startTime)

hepta_words <- data.table(
        word_1 = sapply(strsplit(names(sums_H), "_", fixed = TRUE), '[[', 1),
        word_2 = sapply(strsplit(names(sums_H), "_", fixed = TRUE), '[[', 2),
        word_3 = sapply(strsplit(names(sums_H), "_", fixed = TRUE), '[[', 3),
        word_4 = sapply(strsplit(names(sums_H), "_", fixed = TRUE), '[[', 4),
        word_5 = sapply(strsplit(names(sums_H), "_", fixed = TRUE), '[[', 5),
        word_6 = sapply(strsplit(names(sums_H), "_", fixed = TRUE), '[[', 6),
        word_7 = sapply(strsplit(names(sums_H), "_", fixed = TRUE), '[[', 7),
        count = sums_H)
print('Step9.7')
print(proc.time() - startTime)

# dir.create('data')
write.table(uni_words, "data/uni_words.txt", append = FALSE, quote=FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = FALSE)
write.table(bi_words, "data/bi_words.txt", append = FALSE, quote=FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = FALSE)
write.table(tri_words, "data/tri_words.txt", append = FALSE, quote=FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = FALSE)
write.table(tetra_words, "data/tetra_words.txt", append = FALSE, quote=FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = FALSE)
write.table(penta_words, "data/penta_words.txt", append = FALSE, quote=FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = FALSE)
write.table(hexa_words, "data/hexa_words.txt", append = FALSE, quote=FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = FALSE)
write.table(hepta_words, "data/hepta_words.txt", append = FALSE, quote=FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = FALSE)
print('Step10: Write tables to files complete')
print(proc.time() - startTime)

# Manually remove gutenberg references in ngram files
print('Step2-0: Start preword modeller')
source("preword_modeler.r")
