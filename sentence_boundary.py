def splitParagraphIntoSentences(paragraph):
    import re
    sentenceEnders = re.compile(r"""
        # Split sentences on whitespace between them.
        (?:               # Group for two positive lookbehinds.
          (?<=[.!?])      # Either an end of sentence punct,
        | (?<=[.!?]['"])  # or end of sentence punct and quote.
        )                 # End group of two positive lookbehinds.
        (?<!  Mr\.   )    # Don't end sentence on "Mr."
        (?<!  Mrs\.  )    # Don't end sentence on "Mrs."
        (?<!  Jr\.   )    # Don't end sentence on "Jr."
        (?<!  Dr\.   )    # Don't end sentence on "Dr."
        (?<!  Prof\. )    # Don't end sentence on "Prof."
        (?<!  Sr\.   )    # Don't end sentence on "Sr."
        \s+               # Split on whitespace between sentences.
        """, 
        re.IGNORECASE | re.VERBOSE)
    sentenceList = sentenceEnders.split(paragraph)
    return sentenceList

if __name__ == '__main__':
    f = open("sample.txt", 'r')
    text = f.read()
    mylist = []
    sentences = splitParagraphIntoSentences(text)
    for s in sentences:
        mylist.append(s.strip())
    for i in mylist:
        print i
