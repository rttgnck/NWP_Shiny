
# Next Word and Beyond Prediction in R for STAT400

This project is a fork of https://github.com/duf59/NWP_Shiny with some additionally code for tokenizing from https://thiloshon.wordpress.com/2018/03/11/build-your-own-word-sentence-prediction-application-part-02/.

Using lines of text sampled from dated Project Gutenberg texts, I used the above to build language models from the sampled data and produce a word prediction command line interface in R. I expanded the language modelling of the NWP_Shiny project from 3-word n-grams up to 7-word n-grams. This change allowed me to prdict up to the next 6 words (based on a singular word input). However the prediction at such a high order is only reliable if the training data was extremely large, otherwise the number of 5, 6, or 7 n-grams found in the training data can be very low (10-100 for 7-grams in the included data).

# Files

* `preword_runner.R` : script to start the tokenization > modelling > interface process
* `preword_tokenizer.R` : script to do the tokenization
* `preword_modeller.R` : script to do the language modelling
* `preword_predictor.R` : script for doing the predictions
* `preword_interface.R` : script for command line user interface in R

### Testing

The included source code will run and build a 1900's language model. In order to build an alternative language model for the 1800's a few changes will need to be made anywhere a lib1900 or lines1900 is present in the code replacing the 1900 with 1800. I have also included the 1600's and 1700's data, but there were much fewer books to sample than the 1800's and 1900's.

The check prediction against book is horribly inaccurate in predicting the authors next words. This is because only every 25th line was sampled from the original works thus most lines are not included in the original training data. There is the possibility of expansion, with enough training data, to rewrite the sampler (not included) to split on sentence, and capture entire books, or works of authors. This could possibly lead to being able to take a book from the author not included in training data and trying to predict parts of sentences in that book. This was what I originally wanted to do, but is not how the current version works.

Only books 1-10 in 1900's are included.

1. Set the working directory using `setwd('path_to_where_R_scripts_and_data_are_stored')`
2. Start the tokenization and modelling from scratch with `source('preword_runner.r')` each script ends by starting the next.
3. Command line interface starts when the preword_modeler finishes. You can jump straight to this part skipping the tokenization and modeling by using `source('preword_interface.r')` 
The command line interface explains itself while running.

## Below is unchanged from the original NWP_Shiny app to maintain the original authors explanation (better than mine) and still applies, it has not been updated to include any of my changes present in the source code except for removing the testing  and shiny app sections.

# Building the language model

A brief description of the main steps to obtain the language model from the initial corpus. The latter can consist of any set of text files. In the present case I used data from twitter, blog articles and news articles (about 1GB in total).

Extracting ngrams (word sequences) from the corpus is computationally intensive, especially for large corpora. For this reason the first step was done using an optimized C++ toolkit called MeTa.

### step 1. extracting ngrams

ngrams of order 1 to 4 were extracted from each source file using MeTa.
This can be done using the provided profile.cpp tool and customising the filter chain. I used lowercase and alpha filters, the latter removing all non-alphanumeric characters. For this I modified the freq() function as follows:

``` {C}
using namespace meta::analyzers;

std::unique_ptr<token_stream> stream
    = make_unique<tokenizers::icu_tokenizer>();
stream = make_unique<filters::lowercase_filter>(std::move(stream));
stream = make_unique<filters::alpha_filter>(std::move(stream));
ngram_word_analyzer ana{n, std::move(stream)};
```

Note that, by default, profile.cpp handles ngrams of order up to 3, but you can use higher orders by adding the following lines in main() (or eventually directly pass an argument to freq()):

```{C}
if (all || args.find("--freq-4gram") != args.end())
    freq(file, config, 4);
if (all || args.find("--freq-5gram") != args.end())
    freq(file, config, 5);
```

Note that MeTa by default outputs the ngrams and their count using underscore as a delimiter. I found it a bit unconvenient for further parsing of the files, especially when the ngrams potentially contain underscores. I switched the delimiter to white space in MeTa source code (and this is what the R code expect when reading MeTa's output). This is just a simple change in the source code: locate the ngram_word_analyzer.cpp file and change the following line:

```{C}
combined = tokens[i - j] + "_" + combined;
```
to:
```{C}
combined = tokens[i - j] + " " + combined;
```
Then re-compile MeTa.

### step 2. building the language model

Once ngram have been generated by MeTa and output as text files, we can read them into R and start building the model.
In order to deal with relatively large ngram tables, the data.table package is used and all processing steps are based on join/filter operations (looping through tables rows is by far too slow here).
The whole process takes typically less than a minute and the main steps are as follows:

1. **read the ngram files from MeTa** (4 tables for each source file)
2. **merge the tables**, resulting in 4 tables for ngrams of order 1 to 4
3. **filter all ngrams with non alphabetic characters or appearing less than 5 times**.
Although an alpha filter is used in MeTa, I had some issues with unicodes which seems specific to the R+windows combination.
This is the reason why I included an additional filtering step. Besides, filtering ngrams with count < 5 is to be able to handle tables in memory (i.e. there are a large amount of unique ngram in each table, the latter not contributing to prediction power)
4. **compute the conditional probabilities**. For example for the trigram table, this corresponds to the probability of the third word to appear given the first two words.
I used interpolated Kneyser-Ney smoothing as a language model. This requires computing several intermediate results including continuation counts and then chaining up the results since it is a recursive model.
(a nice way to compute it is to use a recursive function, but it forces you to loop through the tables which is very inefficient. So the actual code uses join operations)

From the above steps a interpolated KN model is built for each ngram order. The lowest order model (unigrams) simply corresponds to maximum likelihood estimation since no recursive call to lower order model is possible.
For higher order models (2-4), the estimated probability take into account information from lower order ngrams.
When doing prediction, we first look into the model of order 4, trying to predict the next words based on the first three words.
If no result is found (i.e. the sequence of three words was unseen in the training corpus), we simply back-off to a lower order KN model.

For more details about how to compute the Kneyser-Ney probabilities, I strongly recommend the [Thesis from Martin Körner](http://mkoerner.de/media/bachelor-thesis.pdf) and [this article](http://www.speech.sri.com/projects/srilm/manpages/pdfs/chen-goodman-tr-10-98.pdf).

### Step 3. Storing the model in a local database

Once KN models of order 1 to 4 are computed, they are stored in a local SQLite database.
I used the SQLite R package for this.
When predicting, one can simply connect to the database and retrieve specific word sequences using SQL queries.

### Testing (CHANGES MADE REFLECTED IN ABOVE TESTING SECTION)

### The shiny App (NO LONGER PRESENT, REPLACED WITH COMMAND LINE PREWORD_INTERFACE)

# References and notes

* [1] J. Klund and M. Novak, "If Word Prediction Can Help, Which Program Do You Choose?", available at http://trace.wisc.edu
* [2] A. Newell, J. Arnott, L. Booth, W. Beattie, B. Brophy and I. Ricketts. "Effect of the "pal" word prediction system on the quality and quantity of text generation.", Augmentative and Alternative Communication, 8(4), 304�311, 1992
