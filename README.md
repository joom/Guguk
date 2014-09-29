Guguk
=====

Turkish NLP library for Haskell. (Name should be pronounced like *"goo gook"*.)

Note that this is a personal pet project, heavily influenced by the mighty [zemberek-nlp](http://github.com/ahmetaa/zemberek-nlp).

## Progress

- [x] Syllabification (in [Guguk.Syllabification](src/Guguk/Syllabification.hs)) ![Progress](http://progressed.io/bar/90)
    * Syllabification fails on words like `elektrik` and `yeşilimtrak`.
- [ ] Phonetics (in [Guguk.Phonetics](src/Guguk/Phonetics.hs)) ![Progress](http://progressed.io/bar/20)
    * More usable set of functions for the existing data and types is needed.
- [ ] Turkish Alphabet (in [Guguk.TurkishAlphabet](src/Guguk/TurkishAlphabet.hs)) ![Progress](http://progressed.io/bar/10)
    * Case conversion, vowel check, ASCIIfying, deASCIIfying functions etc. needed.
- [ ] Phonology (in [Guguk.Morphology.Phonology](src/Guguk/Morphology/Phonology.hs)) ![Progress](http://progressed.io/bar/10)
    * More usable set of functions for Turkish phonology and morphotactical rules.
- [ ] Tokenization (planned to be in *Guguk.Tokenization*) ![Progress](http://progressed.io/bar/0)
    * Sentence boundary detector, and lexer are needed. Nothing has been done about this yet.

## Contribution

I'm very open to any pull requests, issues or other kinds of suggestions. Feedback is especially important since I'm neither a Haskell nor Turkish NLP expert.

## License

[MIT License](http://joom.mit-license.org/)
