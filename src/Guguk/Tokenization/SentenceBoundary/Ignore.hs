-- Based on https://hackage.haskell.org/package/fullstop-0.1.4
-- Copyright (C) 2009 Eric Kow <eric.kow@gmail.com> - MIT License
-- Modified to work on Turkish.

-- | Strings whose internal "." should not be treated as sentence boundaries

module Guguk.Tokenization.SentenceBoundary.Ignore where

-- | Abbreviations of all kinds.
-- The ones with full capital letters are not included,
-- they are abbreviations by default.
abbrs :: [String]
abbrs = [
  "age.", "agm.", "agy.", "Alb.", "Alm.", "anat.", "ant.", "Apt.", "Ar.",
  "AR-GE", "ark.", "Arş. Gör.", "As.", "As. İz.", "Asb.", "astr.", "astrol.",
  "Atğm.", "atm.", "Av.", "bağ.", "Bçvş.", "bit. b.", "biy.", "bk.", "bl.",
  "Bl.", "Bn.", "Bnb.", "bot.", "Böl.", "bs.", "Bşk.", "Bul.", "Bulg.",
  "Cad.", "cm", "coğ.", "cos", "Cum. Bşk.", "çev.", "Çvş.", "dal", "dam",
  "db.", "dg", "dil b.", "dk.", "dl", "dm", "Doç.", "doğ.", "Dr.", "drl.",
  "Dz. Kuv.", "Dz. Kuv. K.", "dzl.", "Ecz.", "ed.", "e. ", "ekon.", "Ens.",
  "Erm.", "EuGH", "f.", "Fak.", "Fak-Fuk Fon", "Far.", "fel.", "fil.", "fiz.",
  "fizy.", "fob", "Fr.", "g", "Gen.", "geom.", "gn.", "Gnkur.", "Gön.", "gr.",
  "Grt", "hay. b.", "haz.", "hek.", "hl", "hlk.", "hm", "Hs. Uzm.", "huk.",
  "Hv. Kuv.", "Hv. Kuv. K.", "Hz. öz.", "Hz.", "İbr.", "İng.", "is.", "İsp.",
  "işl.", "İt.", "Jap.", "jeol.", "kal", "kg", "KHz", "kim.", "km", "koor.",
  "Kor.", "Kora.", "Korg.", "kr.", "krş.", "Kur.", "Kur. Bşk.", "l",
  "Lat.", "Ltd.", "m", "Mac.", "Mah.", "man.", "mat.", "Md.", "mec.", "MHz",
  "mim.", "min.", "mm", "Müh.", "Mür.", "müz.", "No.", "Nö.", "Nö. Sb.",
  "Okt.", "Onb.", "Opr.", "Or.", "Ora.", "Ord.", "Org.", "Ort.", "Osm. T.",
  "öl.", "ör.", "öz.", "ped.", "Port.", "Prof.", "psikol.", "Ro-Ro", "Rum.",
  "Rus.", "s.", "sa.", "Sb.", "sf.", "Sl.", "Sn.", "snt.", "Sok.",
  "sos.", "sp.", "Srp.", "Şb.", "t", "tar.", "Tb.", "tek.", "tel.", "telg.",
  "Tğm.", "tic.", "tiy.", "tlks.", "tls.", "Top.", "Tug.", "Tuğa.",
  "Tuğg.", "Tüm.", "Tüma.", "Tümg.", "Uzm.", "Üçvş.", "ünl.", "Ütğm.",
  "vb.", "vd.", "Vet.", "vs.", "w", "Y. Mim.", "Y. Müh.", "Yay.", "Yb.",
  "Yd. Sb.", "YKr", "Yrd.", "Yrd. Doç.", "Yun.", "yy.", "Yzb.", "zf.", "zm.",
  "zool.", "San.", "Op.", "Tic.", "Şti.", "vek.", "St."]

initials :: [String]
initials = map (: ".") ['A'..'Z']
