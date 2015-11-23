module Guguk.Syntax.PosTagger where

-- | Possible parts of speech as given in the
--   METU-Sabanci treebank paper.
data POS = Start
         | Noun
         | Adj
         | Adv
         | Verb
         | Pron
         | Conj
         | Det
         | Postp
         | Ques
         | Interj
         | Num
         | Dup
         | Punc
         | Unknown
         deriving (Eq, Show, Read, Ord, Enum)
