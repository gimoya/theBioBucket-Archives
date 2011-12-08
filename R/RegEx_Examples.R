library(stringr)

shopping_list <- c("bread & Apples x4", "flouR", "sugar", "milk x2")
str_extract(shopping_list, "[A-Z].*[1-9]")
# this extracts partial strings starting with an upper-case letter
# and ending with a digit, for all elements of the input vector..
# "." period, any single case letter, "*" the preceding item will
# be matched zero or more times, ".*" regex for a string
# comprised of any item being repeated arbitrarily often.

# output:
[1] "Apples x4" NA   NA   NA


str_extract(shopping_list, "[a-z]{1,4}")
# this extracts partial strings with lowercase repetitions of 4, for all elements of the input vector..

# output:
[1] "brea" "flou" "suga" "milk"


str_extract(shopping_list, "\\b[a-z]{1,4}\\b")
# this extracts whole words with lowercase repetitions of 4, for all elements of the input vector..

#output:
[1] NA     NA     NA     "milk"


str <- c("i.e., George W. Bush", "Lyndon B. Johnson, etc.")
gsub("([A-Z])[.]?", "\\1", str)
# this will find abbreviated names and remove the period:
# the uppercase letters followed by a period are matched by
# [A-Z][.]? = repeated at most once. the parentheses delineate a
# backreference, i.e. the uppercase letter, which will be
# replaced by \\1 which is the first backreference.

# output:
[1] "George W Bush"    "Lyndon B Johnson"


str <- c("George W. Bush", "Lyndon B. Johnson")
sub(" .*", "", str)
# keeps the first word and removes the rest.
# matches and replaces the substring comprised of the first
# white space followed by any single character,
# designated by the period, repeated zero or more times, as
# given by the asterisk.

# output:
[1] "George" "Lyndon"


sub("\\s\\w+$", "", str)
# removes the last word plus the preceding space in a string.
# looks for a space followed by any word which is the last one:
# the dollar sign $ is a meta-character that matches the
# beginning and end of a line.

# output:
[1] "George W." "Lyndon B."


sub(".*\\s(\\w+$)", "\\1", str)
# keep only the last word of a string.
# looks for anything, repeated arbitraily often followed by a
# space ".*\\" and a word which is the last in line.
# for this word you put brackets for a back-reference, which is
# returned by "\\1", the 1st back-reference.

# output:
[1] "Bush"    "Johnson"


str <- c("&George W. Bush", "Lyndon B. Johnson?")
gsub("[^[:alnum:][:space:].]", "", str)
# keep alphanumeric signs AND full-stop, remove anything else,
# that is, all other punctuation. the caret the caret delineates what
# shouldn't be matched

# output:
[1] "George W. Bush"    "Lyndon B. Johnson"


str <- c("since december the 12th 2009", "this was the year 1911",
        "the date 30.07.1798", "I was born 14.06.1977")
dates <- str_extract(str, "[0-9]{2}[.][0-9]{2}[.][0-9]{4}")
dates <- dates[!is.na(dates)]
# extracts dates with format dd.mm.yyyy

# output:
[1] "30.07.1798" "14.06.1977"


