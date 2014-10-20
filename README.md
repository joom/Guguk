Guguk [![Build Status](https://secure.travis-ci.org/joom/Guguk.svg)](http://travis-ci.org/joom/Guguk)
=====

Turkish NLP library for Haskell. (Name should be pronounced like *"goo gook"*.)

Note that this is a personal pet project, heavily influenced by the mighty [zemberek-nlp](http://github.com/ahmetaa/zemberek-nlp).

## Progress

- [x] Syllabification (in [Guguk.Syllabification](src/Guguk/Syllabification.hs)) ![Progress](http://progressed.io/bar/95)
    * Passes all the tests. The `syllabify` function can be refactored, the guard usage prevented better pattern matching.
- [x] Phonetics (in [Guguk.Phonetics](src/Guguk/Phonetics.hs)) ![Progress](http://progressed.io/bar/20)
    * More usable set of functions for the existing data and types is needed.
- [x] Turkish Alphabet (in [Guguk.TurkishAlphabet](src/Guguk/TurkishAlphabet.hs)) ![Progress](http://progressed.io/bar/10)
    * Case conversion, vowel check, ASCIIfying, deASCIIfying functions etc. needed.
- [ ] Phonology (in [Guguk.Morphology.Phonology](src/Guguk/Morphology/Phonology.hs)) ![Progress](http://progressed.io/bar/10)
    * More usable set of functions for Turkish phonology and morphotactical rules.
- [ ] Tokenization ([Guguk.Tokenization](src/Guguk/Tokenization)) ![Progress](http://progressed.io/bar/5)
    * [Sentence boundary detector](src/Guguk/Tokenization/SentenceBoundary.hs) started, basic functionality exists but some tests fail and there are certain things to do. (e.g. Turkish dictionary of abbreviations) Lexer needed.

## Contribution

I'm very open to any pull requests, issues or other kinds of suggestions. Feedback is especially important since I'm neither a Haskell nor Turkish NLP expert.

## License

[MIT License](http://joom.mit-license.org/)
