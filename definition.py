#!/usr/bin/python

# needs to run with mac system python
# as this uses DictionaryServices from the ObjC bindings
# Mac only!

# queries the built in mac dictionary app and returns the
# definition for the word passed as an argument

# the haskell game program just reads from this process's stdout

import DictionaryServices
import sys

if __name__ == "__main__":
    try:
        word = sys.argv[1].decode('utf-8')
        result = DictionaryServices.DCSCopyTextDefinition(None, word, (0, len(word)))
        print result.encode('utf-8')
    except:
        print 'No definition available'
