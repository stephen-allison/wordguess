#!/usr/bin/python

# needs to run with mac system python
# as this uses DictionaryServices from the ObjC bindings
# Mac only!

# queries the built in mac dictionary app and returns the
# definition for the word passed as an argument

# the haskell game program just reads from this process's stdout

import DictionaryServices
import sys

def get_definition(word):
    try:
        result = DictionaryServices.DCSCopyTextDefinition(None, word, (0, len(word)))
        return result.encode('utf-8')
    except:
        return 'No definition available'

if __name__ == "__main__":
    word = sys.argv[1].decode('utf-8')
    definition = get_definition(word)
    print definition
