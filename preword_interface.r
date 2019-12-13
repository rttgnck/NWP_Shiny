# load the predictor funcions
source("preword_predictor.R")

require(data.table)
require(progress)
require(numbers)
require(quanteda)
require(crayon)
# library(rapport)

# setwd('D:/_School/FA19/STAT400/pre')
lib = read.csv('libs/lib1900.csv', header=TRUE, sep='|')
cat(cyan('Enter some words to predict the next word(s)',"\n"))
input.nwords <- readline(prompt='or press enter to check against a book: ')
if (input.nwords == '') {
  input.book <- as.integer(readline(prompt='Enter book number to use (1-47000): '))
  print(sprintf('Using book:: %s',lib[input.book,'title']))
  input.file <- paste0('_theData/',lib[input.book,'filename'])
  book <- file(input.file, 'r')
  lines <- readLines(book)
  close(book)
  input.line = ''
  while (input.line == '') {
    input.linenum <- as.double(readline(prompt='Enter line for predicting next word(s) (0.0000-1.0000 percent): '))
    input.linenum <- floor(input.linenum*length(lines))
    input.line <- lines[input.linenum]
    input.line = gsub("/|:|<|>|\\|\\\\|\\*|\\=|-|_", " ", input.line)
    print(sprintf('Using line:: %s',input.linenum))
    print(sprintf('Which reads:: %s',input.line))
    cat(cyan("Enter first nwords to use (should be less than 5)"))
    input.numwords <- as.integer(readline(prompt="or press enter to pick new line: "))
    if (is.na(input.numwords)) {
      input.line = ''
    }
  }
  input.words <- tokens(
      x=tolower(input.line),
      remove_punct = TRUE,
      remove_twitter = TRUE,
      remove_numbers = TRUE,
      remove_hyphens = TRUE,
      remove_symbols = TRUE,
      remove_url = TRUE
  )
  # print(head(input.words))
  input.nwords <- input.words[[1]][1:input.numwords]
  input.nwords <- paste(input.nwords, collapse = ' ')
  print(sprintf('First %s words are:: %s',input.numwords, input.nwords))
  input.lpred <- as.integer(readline(prompt="Enter length of prediction (7-prewords): "))
  input.rwords <- input.words[[1]][(input.numwords+1):7]
  if (input.lpred == 1) {
    input.npred <- as.integer(readline(prompt="Enter number for top predictions for next word: "))
  }

  if (input.lpred > 1) {
    cat(sprintf('Predicted next %s words: \n\t',input.lpred))
    cat(predictor(raw_input = input.nwords, lpred = input.lpred))
    cat('\n')
    cat('The next book words are\n\t')
    cat(input.rwords)
    cat('\n')
  } else {
    cat('Top predictions for next word: \n')
    print(predictor(raw_input = input.nwords, npred = input.npred))
    cat('\n')
  }
} else {
  input.lpred <- as.integer(readline(prompt="Enter length of prediction (7-prewords): "))
  if (input.lpred == 1) {
    input.npred <- as.integer(readline(prompt="Enter number for top predictions for next word: "))
  }

  if (input.lpred > 1) {
    cat(sprintf('Predicted next %s words: \n\t',input.lpred))
    cat(predictor(raw_input = input.nwords, lpred = input.lpred))
    cat('\n')
  } else {
    cat('Top predictions for next word: \n')
    print(predictor(raw_input = input.nwords, npred = input.npred))
    cat('\n')
  }
}
