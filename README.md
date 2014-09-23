Guguk
=====

Turkish NLP library for Haskell. (Name should be pronounced like *"goo gook"*.)

Note that this is a personal pet project, heavily influenced by the mighty [zemberek-nlp](http://github.com/ahmetaa/zemberek-nlp).

## Progress

- [x] Syllabification (in *Guguk.Syllabification*) ![Progress](http://progressed.io/bar/90)
    * Syllabification fails on words like `elektrik` and `yeşilimtrak`.
- [ ] Phonetics (in *Guguk.Morphology.Phonetics*) ![Progress](http://progressed.io/bar/10)
    * A usable set of functions for the existing data and types is needed.
- [ ] Tokenization (in *Guguk.Tokenization*) ![Progress](http://progressed.io/bar/0)
    * Sentence boundary detector, and lexer are needed. Nothing has been done about this yet.
- [ ] Turkish Alphabet (in *Guguk.TurkishAlphabet*) ![Progress](http://progressed.io/bar/1)
    * Case conversion, vowel check, ASCIIfying, deASCIIfying functions etc. needed.

## Contribution

I'm very open to any pull requests, issues or other kinds of suggestions. Feedback is especially important since I'm neither a Haskell nor Turkish NLP expert.

## License

[MIT License](http://joom.mit-license.org/)
