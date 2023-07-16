# Tokenizer
Woefully incomplete. Missing encoding {pre,suf}fixes for strings and
charcter constants, trigraphs.

# Preprocessor
Missing a lot of pragmas. Those that affect the parser should be passed
along with special tokens; also need to unify `_Pragma("...")` and
`#pragma` spellings. https://en.cppreference.com/w/c/preprocessor/impl

# Parser
Need to review where trailing commas are and aren't allowed.

