library(stringr)

# this extracts partial strings starting with an upper-case letter
# and ending with a digit, for all elements of the input vector..
# "." period, any single case letter, "*" the preceding item will
# be matched zero or more times, ".*" regex for a string
# comprised of any item being repeated arbitrarily often.
shopping_list <- c("bread & Apples x4", "flouR", "sugar", "milk x2")
str_extract(shopping_list, "[A-Z].*[1-9]")
# output:
[1] "Apples x4" NA   NA   NA

# this extracts partial strings with lowercase repetitions of 4, for all elements of the input vector..
str_extract(shopping_list, "[a-z]{1,4}")
# output:
[1] "brea" "flou" "suga" "milk"

# this extracts whole words with lowercase repetitions of 4, for all elements of the input vector..
str_extract(shopping_list, "\\b[a-z]{1,4}\\b")
#output:
[1] NA     NA     NA     "milk"


# this will find abbreviated names and remove the period:
# the uppercase letters followed by a period are matched by
# [A-Z][.]? = repeated at most once. the parentheses delineate a
# backreference, i.e. the uppercase letter, which will be
# replaced by \\1 which is the first backreference.
str <- c("i.e., George W. Bush", "Lyndon B. Johnson, etc.")
gsub("([A-Z])[.]?", "\\1", str)
# output:
[1] "George W Bush"    "Lyndon B Johnson"

# keeps the first word and removes the rest.
# matches and replaces the substring comprised of the first
# white space followed by any single character,
# designated by the period, repeated zero or more times, as
# given by the asterisk.
str <- c("George W. Bush", "Lyndon B. Johnson")
sub(" .*", "", str)
# output:
[1] "George" "Lyndon"

# removes the last word plus the preceding space in a string.
# looks for a space followed by any word which is the last one:
# the dollar sign $ is a meta-character that matches the
# beginning and end of a line.
sub("\\s\\w+$", "", str)
# output:
[1] "George W." "Lyndon B."

# keep only the last word of a string.
# looks for anything, repeated arbitraily often followed by a
# space ".*\\" and a word which is the last in line.
# for this word you put brackets for a back-reference, which is
# returned by "\\1", the 1st back-reference.
sub(".*\\s(\\w+$)", "\\1", str)
# output:
[1] "Bush"    "Johnson"

# split string on first occurrence of a given character:
x <- "I want to split here, though I don't want to split elsewhere, even here."
y <- c("Here's comma 1, and 2, see?", "Here's 2nd sting, like it, not a lot.")
XX <- "SoMeThInGrIdIcUlOuS"
strsplit(sub(",\\s*", XX, x), XX)
strsplit(sub(",\\s*", XX, y), XX)
# output:
> strsplit(sub(",\\s*", XX, x), XX)
[[1]]
[1] "I want to split here"                              
[2] "though I don't want to split elsewhere, even here."
> strsplit(sub(",\\s*", XX, y), XX)
[[1]]
[1] "Here's comma 1" "and 2, see?"   
[[2]]
[1] "Here's 2nd sting"    "like it, not a lot."

str <- c("&George W. Bush", "Lyndon B. Johnson?")
gsub("[^[:alnum:][:space:].]", "", str)
# keep alphanumeric signs AND full-stop, remove anything else,
# that is, all other punctuation. the caret determines what
# shouldn't be matched
# output:
[1] "George W. Bush"    "Lyndon B. Johnson"

# extracts dates with format dd.mm.yyyy..
str <- c("since december the 12th 2009", "this was the year 1911",
        "the date 30.07.1798", "I was born 14.06.1977")
dates <- str_extract(str, "[0-9]{2}[.][0-9]{2}[.][0-9]{4}")
dates[!is.na(dates)]
# output:
[1] "30.07.1798" "14.06.1977"

# extract text (alphanumeric signs only = [1-9, Aa-Zz]) between parantheses,
# paranthese in search string must be escaped with slashes and
# .*? stands for any sign, repeated arbitrarily often and does 
# not extract greedy - that is, it does not take text within first and 
# last match but from first matches of parantheses!
a <- "[question(37), question_pipe(\"Person10\")]"
b <- unlist(str_extract_all(a, pattern = "\\(.*?\\)"))
gsub("[[:punct:]]", "", b)
# output:
[1] "37"       "Person10"

# extract 4-digit numbers, at the beginning (^) followed by space (\\s),
# preceded and followed by space or preceded by space at end of line ($)
xx <- "4567 coihr 1234 &/()= 567899876 jngm 34 ljd 1238"
pat <- "(^|\\s)(\\d{4})($|\\s)"
as.numeric(regmatches(xx, gregexpr(pat, xx, perl=TRUE))[[1]])
# output:
[1] 4567 1234 1238

# trim leading zeroes
# caret assigns the beginning of a string, zero repeated at least once (+) followed by anything, 
# which is caught with a back-reference ("\\1")
sub("^[0]+(.)", "\\1", "000650")
# output:
"650"

# the last task can be achieved more conveniently with str_trim::stringr (thanks, eduard!)
# however, the input and output here are numbers, whereas in the above example we used strings 
library(stringr)
str_trim(000650)
# output:
650
