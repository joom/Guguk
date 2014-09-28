{-# LANGUAGE OverloadedStrings #-}

module Guguk.Phonetics
where

import qualified Data.Text as T
import qualified Data.List as L

type IPASymbol        = T.Text
type SurfaceForm      = T.Text

-- Vowel types

data VowelOpenness    = Close | NearClose | CloseMid |
                        Mid | OpenMid | NearOpen | Open
                        deriving (Show, Eq)

data VowelLocation    = Front | NearFront | Central | NearBack | Back
                        deriving (Show, Eq)

data VowelRoundedness = Rounded | Unrounded | Nonrounded
                        deriving (Show, Eq)

data VowelLength      = Long | NormalLength
                        deriving (Show, Eq)

data Vowel            = Vowel IPASymbol VowelOpenness
                          VowelLocation VowelRoundedness VowelLength
                        deriving (Show, Eq)

-- Consonant types

data ConsonantPlace   = Labial | Bilabial | Dental | Labiodental |
                        Alveolar | PostAlveolar | PalatoAlveolar |
                        Palatal | Velar | Glottal | AlveolarLateral
                        deriving (Show, Eq)

data ConsonantManner  = Nasal | Stop | Affricate | Fricative |
                        Approximant | Flap | Sibilant
                        deriving (Show, Eq)

data ConsonantVoice   = Voiced | Voiceless
                        deriving (Show, Eq)

data Consonant        = Consonant IPASymbol ConsonantVoice ConsonantPlace ConsonantManner
                        deriving (Show, Eq)

-- Phoneme types

data Phoneme          = VowelPhoneme     SurfaceForm Vowel |
                        ConsonantPhoneme SurfaceForm Consonant
                        deriving (Show, Eq)

turkishPhonemes :: [Phoneme]
turkishPhonemes = [
   -- Vowels
   VowelPhoneme "a" (Vowel "ɑ"  Open     Back  Unrounded NormalLength)
 , VowelPhoneme "a" (Vowel "a"  Open     Front Unrounded NormalLength)
 , VowelPhoneme "e" (Vowel "e"  CloseMid Front Unrounded NormalLength)
 , VowelPhoneme "e" (Vowel "ɛ"  OpenMid  Front Unrounded NormalLength)
 , VowelPhoneme "e" (Vowel "æ"  NearOpen Front Unrounded NormalLength)
 , VowelPhoneme "ı" (Vowel "ɯ"  Close    Back  Unrounded NormalLength)
 , VowelPhoneme "i" (Vowel "i"  Close    Front Unrounded NormalLength)
 , VowelPhoneme "o" (Vowel "o"  CloseMid Back  Rounded   NormalLength)
 , VowelPhoneme "ö" (Vowel "ø"  CloseMid Front Rounded   NormalLength)
 , VowelPhoneme "u" (Vowel "u"  Close    Back  Rounded   NormalLength)
 , VowelPhoneme "ü" (Vowel "y"  Close    Front Rounded   NormalLength)
   -- Long Vowels
 , VowelPhoneme "â" (Vowel "aː" Open     Back  Unrounded Long)
 , VowelPhoneme "î" (Vowel "iː" Close    Front Unrounded Long)
 , VowelPhoneme "û" (Vowel "uː" Close    Back  Rounded   Long)

   -- Consonants
 , ConsonantPhoneme "b" (Consonant "b"  Voiced    Bilabial        Stop)
 , ConsonantPhoneme "c" (Consonant "d͡ʒ" Voiced    PalatoAlveolar  Affricate)
 , ConsonantPhoneme "ç" (Consonant "t͡ʃ" Voiceless PalatoAlveolar  Affricate)
 , ConsonantPhoneme "d" (Consonant "d̪"  Voiced    Dental          Stop)
 , ConsonantPhoneme "f" (Consonant "f"  Voiceless Labiodental     Fricative)
 , ConsonantPhoneme "g" (Consonant "ɡ"  Voiced    Velar           Stop)
 , ConsonantPhoneme "ğ" (Consonant "ɣ"  Voiced    Velar           Fricative)
 , ConsonantPhoneme "h" (Consonant "h"  Voiceless Glottal         Fricative)
 , ConsonantPhoneme "j" (Consonant "ʒ"  Voiced    PalatoAlveolar  Sibilant)
 , ConsonantPhoneme "k" (Consonant "k"  Voiceless Velar           Stop)
 , ConsonantPhoneme "l" (Consonant "l"  Voiced    AlveolarLateral Approximant)
 , ConsonantPhoneme "m" (Consonant "m"  Voiced    Bilabial        Nasal)
 , ConsonantPhoneme "n" (Consonant "n"  Voiced    Alveolar        Nasal)
 , ConsonantPhoneme "p" (Consonant "p"  Voiceless Bilabial        Stop)
 , ConsonantPhoneme "r" (Consonant "ɾ"  Voiced    Alveolar        Flap)
 , ConsonantPhoneme "s" (Consonant "s"  Voiceless Alveolar        Fricative)
 , ConsonantPhoneme "ş" (Consonant "ʃ"  Voiceless PalatoAlveolar  Fricative)
 , ConsonantPhoneme "t" (Consonant "t̪"  Voiceless Dental          Stop)
 , ConsonantPhoneme "v" (Consonant "v"  Voiced    Labiodental     Fricative)
 , ConsonantPhoneme "y" (Consonant "j"  Voiced    Palatal         Approximant)
 , ConsonantPhoneme "z" (Consonant "z"  Voiced    Alveolar        Sibilant)
 ]

-- Phoneme general functions
getSurfaceForm :: Phoneme -> SurfaceForm
getSurfaceForm (VowelPhoneme     x _) = x
getSurfaceForm (ConsonantPhoneme x _) = x

getIPASymbol :: Phoneme -> IPASymbol
getIPASymbol (VowelPhoneme     _ (Vowel     x _ _ _ _)) = x
getIPASymbol (ConsonantPhoneme _ (Consonant x _ _ _  )) = x

getBySurfaceForm :: SurfaceForm -> [Phoneme]
getBySurfaceForm sf = L.filter ((== sf) . getSurfaceForm) turkishPhonemes

getByIPASymbol :: IPASymbol -> Maybe Phoneme
getByIPASymbol ipa = L.find ((== ipa) . getIPASymbol) turkishPhonemes

-- Vowel specific functions
vowelTypeError :: String
vowelTypeError = "Cannot get vowel-specific feature of a consonant"

isVowel :: Phoneme -> Bool
isVowel (VowelPhoneme _ _) = True
isVowel _                  = False

vowelOpenness :: Phoneme -> VowelOpenness
vowelOpenness (VowelPhoneme _ (Vowel _ x _ _ _)) = x
vowelOpenness _ = error vowelTypeError

vowelLocation :: Phoneme -> VowelLocation
vowelLocation (VowelPhoneme _ (Vowel _ _ x _ _)) = x
vowelLocation _ = error vowelTypeError

vowelRoundedness :: Phoneme -> VowelRoundedness
vowelRoundedness (VowelPhoneme _ (Vowel _ _ _ x _)) = x
vowelRoundedness _ = error vowelTypeError

vowelLength :: Phoneme -> VowelLength
vowelLength (VowelPhoneme _ (Vowel _ _ _ _ x)) = x
vowelLength _ = error vowelTypeError

-- Consonant specific functions
consonantTypeError :: String
consonantTypeError = "Cannot get consonant-specific feature of a vowel"

isConsonant :: Phoneme -> Bool
isConsonant (ConsonantPhoneme _ _) = True
isConsonant _                      = False

consonantVoice :: Phoneme -> ConsonantVoice
consonantVoice (ConsonantPhoneme _ (Consonant _ x _ _)) = x
consonantVoice _ = error consonantTypeError

consonantPlace :: Phoneme -> ConsonantPlace
consonantPlace (ConsonantPhoneme _ (Consonant _ _ x _)) = x
consonantPlace _ = error consonantTypeError

consonantManner :: Phoneme -> ConsonantManner
consonantManner (ConsonantPhoneme _ (Consonant _ _ _ x)) = x
consonantManner _ = error consonantTypeError
