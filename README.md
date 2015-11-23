Guguk [![Build Status](https://secure.travis-ci.org/joom/Guguk.svg)](http://travis-ci.org/joom/Guguk)
=====

Turkish NLP library for Haskell. (pronounce: *"[goo gook](http://forvo.com/word/guguk/#tr)"*.)

Note that this is a personal pet project, heavily influenced by the mighty [zemberek-nlp](http://github.com/ahmetaa/zemberek-nlp).

## Progress

- [x] Syllabification (in [Guguk.Syllabification](src/Guguk/Syllabification.hs)) ![Progress](http://progressed.io/bar/100)
    * Passes all the tests.
- [x] Phonetics (in [Guguk.Phonetics](src/Guguk/Phonetics.hs)) ![Progress](http://progressed.io/bar/20)
    * More usable set of functions for the existing data and types is needed.
- [x] Turkish Alphabet (in [Guguk.TurkishAlphabet](src/Guguk/TurkishAlphabet.hs)) ![Progress](http://progressed.io/bar/10)
    * ASCIIfying, deASCIIfying functions etc. needed.
- [ ] Phonology (in [Guguk.Morphology.Phonology](src/Guguk/Morphology/Phonology.hs)) ![Progress](http://progressed.io/bar/10)
    * More usable set of functions for Turkish phonology and morphotactical rules.
- [ ] Tokenization ([Guguk.Tokenization](src/Guguk/Tokenization)) ![Progress](http://progressed.io/bar/7)
    * Basic functionality for [Sentence boundary detector](src/Guguk/Tokenization/SentenceBoundary.hs). (*TODO*: Handling ":" and "...", and changing from `String` to `Text`) This can be rewritten using Parsec.
    * Lexer needed.
- [ ] POS Tagger ([Guguk.Syntax.PosTagger](src/Guguk/Syntax/PosTagger.hs)) ![Progress](http://progressed.io/bar/0)

## Contribution

I'm very open to any pull requests, issues or other kinds of suggestions. Feedback is especially important since I'm neither a Haskell nor Turkish NLP expert.

## Projects that use Guguk

* [Divan.hs](http://github.com/joom/Divan.hs): Ottoman Divan poetry vezin checker
* [Hezarfen](http://github.com/joom/Hezarfen): Ottoman Turkish to Modern Turkish transcription. (in progress)

## License

[MIT License](http://joom.mit-license.org/)
