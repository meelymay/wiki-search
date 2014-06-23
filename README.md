#Simple Wikipedia Search

##Running IndexMain and SearchMain

To build the index run:

~~~
$ sbt "run-main BuildMain [wikipedia tsv]"
~~~

Then run the following to search the index:

~~~
$ sbt "run-main SearchMain [query]"
~~~

All articles containing at least one of the terms in the query will be output, one per line, with the most relevant articles appearing first. You can include multi-word phrases by escaping the space character, eg. "Sound\ Cloud."

Tests can be run using:

~~~
$ sbt test
~~~

NOTE: JVM memory should be at least 2G


##Runtime Complexity

###BuildIndex

The time complexity of building the index is linear in the total number of terms (non-unique) in the wikipedia dump, as is the space complexity.

~~~
for line in file
    add to title map (constant)
    for term in line
        add to token map (constant)
        check existence (constant)
        add to list (constant)
        add to size map (constant)
~~~

Because I am storing all positions of every term in the corpus, the size of the index in memory is of the same order as the size of the corpus. However, since I convert all titles and terms (Strings) to ids and tokens (Ints), the in-memory size should be considerably smaller. The Strings are each only stored once in the title and token maps.

###Serialization & Deserialization

These are both linear in the size of the index. Files are read line by line and added to the relevant data structures in constant time.

###Search

The runtime of the ranking algorithm is parameterized by how many terms are in the query (t) and how many matching documents there are (d). For multi-word queries we must additionally consider how many words per term (w) and positions per document (p).

~~~
for each term (t)
    get matching documents (linear(d) or Multi-word Term Complexity)
    for each document for term
        score (constant)
sort scores (t*d log (t*d))
~~~

Multi-word Term Complexity: for multi-word terms with (w) words, finding the matching documents is also dependent on the number of matching positions in the documents (p).

~~~
get all intersecting docs: O(w*d)

for each intersecting docs (d)
    for each position in doc (p)
        for each word (w)
            check if the corresponding position exists in the doc (constant)
~~~

Worst case, for a query with (t) multi-word terms with (w) words and (p) positions per matching document, of which there are (d), the runtime would be:

~~~
O(t*w*d*p + t*d log (t*d))
~~~

##Scalability

On a machine with more memory, this indexer should be able to handle a much bigger index (as the time and space scale linearly). It is not entirely obvious from the current implementation of the index how we could distribute across multiple machines if one machine's memory were not large enough to hold it. It would probably make sense to store the index in a designated key-value store (like memcached) instead of deserializing each time we start up the searching program. Currently SearchMain reloads the index into memory each time it is run, but if we were actually serving searches we would only have to load the index when we restarted the server.

##Ranking Limitations

My ranking algorithm simply sums the tf-idf score for every (term, document) pair, where the terms are the terms in the query. The simple sum means that a document may still be ranked highly in the results even if it were missing several terms simply because one term's tf-idf was very high. My ranking does not take into consideration the adjacency of terms (other than for multi-word terms), or how near the beginning of an article a term is, or the order of the search terms (again, other than for multi-word terms).


##Improvements (in order of predicted improvement to result relevance)

###Search Titles
Currently titles are not indexed separately from the text, even though is probably the highest indicator of relevance when terms in the query match terms in the title.

###Stemming terms
Stemming the terms in both articles and queries would have several benefits. Firstly, it would increase the coverage of the index by allowing queries to match articles with all words with the same root as the term (eg. "run" matches "running," "dog" matches "dogs," "creator" matches "creation"). Secondly, it would decrease the number of keys in the index (though the number of positions stored would be the same). My approach for actually stemming the terms would probably involve lists of common prefixes and suffixes (eg. "-ing, "un-", "-s").

###Ranking by term proximity
Since the index includes position information for the terms, it might be helpful to consider how close terms are in a document (suggesting that they might be the related concepts that the searcher was looking for).

###Synonym model
In addition to stemming the terms, it might be helpful to come up with a synonym model to also include documents with the synonyms of the search terms. I would probably add some discounting to these documents and still rank the documents with the actual terms more highly. Building the model for these synonyms could be done when building the index, an additional map from tokens to their synonym's tokens, maybe with some weights. Actually determining which words are synonyms would be a challenging problem on its own; I imagine we could use some statistical models based on the word's surrounding context and comparing those.

###Spelling correction
It could be implemented similarly to the synonym model.

###Reconstructing snippets
Showing snippets along with the document title would be a shnazzy feature, but it (obviously) would not improve relevance.
