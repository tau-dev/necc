# Tokenizer
Missing encoding for string prefixes and charcter constants, as well as
trigraphs.

# Preprocessor
Missing a lot of pragmas; need to unify `_Pragma("...")` and
`#pragma` spellings. https://en.cppreference.com/w/c/preprocessor/impl

# Parser
Does not yet support old-style declarations.
Need to review where trailing commas are and aren't allowed.

# Missing C23 features

* Some Unicode stuff
* `auto`
* VLAs initializers
* Digit separators
* Identical cvr-qualifications for array types and their element types
* `#embed`
* Attribute implementations
