module Tests.Tokenization.SentenceBoundary where

import Guguk.Tokenization.SentenceBoundary
import Test.HUnit

sentenceLists :: [[String]]
sentenceLists = [
  -- from github.com/ahmetaa/turkish-nlp-examples
  [ "Prof. Dr. Veli Davul açıklama yaptı.",
    "Kimse %6.5'lik enflasyon oranını beğenmemiş!",
    "Oysa maçta 2. olmuştuk...",
    "Değil mi?"],
  -- from Radikal newspaper
  [ "Saat 17.56 itibarıyla uluslararası piyasalarda altının onsu 1.244,38 dolardan işlem görüyor.",
    "Borsa İstanbul Altın Piyasası endeksi önceki kapanışa göre yüzde 0,61 değer kazandı.",
    "Altının kilogramı kapanışta 90 bin 150 lira oldu."],
  -- from various Wikipedia articles
  [ "Türkiye topraklarında gelişen edebiyatın ilk ürünleri Selçuklu devrine aittir.",
    "Ancak bu devirden günümüze ulaşabilenler XIII. yüzyıla aittir.",
    "Farsça mesnevisi ile tanınan Mevlana Celaleddin Rumi'nin (1207-1273) az sayıda Türkçe beyitleri vardır."],
  [ "37. Cumhuriyet Hükümeti, (26 Ocak 1974 - 17 Kasım 1974).",
    "Bülent Ecevit tarafından kurulan CHP ve MSP koalisyon hükümetidir.",
    "I. Ecevit Hükümeti olarak anılmaktadır."],
  [ "T.C. Adalet Bakanlığının görevleri, ... kanunun 2. maddesinde sayılmıştır.",
    "Buna göre Adalet Bakanlığı'nın bazı görevleri şunlardır:",
    "Kanunlarla verilen diğer görevleri yapmak."] ]

-- Combines each sentence list into a string
-- and then checks if the sentence boundary detector result
-- matches the ideal sentence list.
tests :: Test
tests = TestList $ map (TestCase . conv) sentenceLists
  where conv l = assertEqual "Sent. Boundary Test" l (segment . unwords $ l)
