#Running IndexMain and SearchMain

To build the index run:

$ sbt "run-main BuildMain [wikipedia tsv]"

Then run the following to search the index:

$ sbt "run-main SearchMain [query]"

All articles containing at least one of the terms in the query will be output, one per line, with the most relevant articles appearing first.

NOTE: JVM memory should be at least 2G


#Runtime Complexity

##BuildIndex

Time complexity:

for line in file
    add to title map (constant)
    for term in line
        add to token map (constant)
        check existence (constant)
        add to list (constant)
        add to size map (constant)

Space complexity:

Because I am storing all positions of every term in the corpus, the size of the index in memory is of the same order as the size of the corpus. However, since I convert all titles and terms (Strings) to ids and tokens (Ints), the in-memory size should be considerably smaller.

##Serialization & Deserialization

These are both linear in the size of the index. Files are read line by line and added to the relevant data structures in constant time.

##Search



#Scalability


#Ranking Limitations

My ranking algorithm is very naive and simply sums the tf-idf score for each term in the document. It does not take into consideration the adjacency of terms, or how near the beginning of an article a term is, or the order of the search terms.


#Improvements

##Search Titles
##Stemming terms
##Ranking by term adjacency
##Synonym model
##Reconstructing snippets
