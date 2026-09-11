(ns culture.facts
  "Country-level regional-culture catalog for Kazakhstan (KAZ) -- national
  dishes, protected products, beverages, crafts, festivals and heritage
  sites, per ADR-2607171400 addendum 2 (cloud-itonami-municipality-
  culture-catalog Wave 1, in com-junkawasaki/root). Sibling namespace to
  `marketentry.facts` / `statute.facts` (ADR-2607141700); city-level
  counterparts live in the cloud-itonami-municipality-* repos.

  Catalog is keyed by UPPERCASE ISO3 (mirrors `statute.facts`); entries
  carry no :culture/municipality (that attribute is city-level only).

  Every entry cites a source URL that was actually fetched and read on
  :culture/retrieved-at -- never fabricated. Summaries state only what the
  cited source confirms. An item not in this table has NO spec-basis, full
  stop; extend `catalog`, do not invent an id/url.")

(def catalog
  "iso3 -> vector of culture entries."
  {"KAZ"
   [{:culture/id "kaz.dish.beshbarmak"
     :culture/name "Beshbarmak"
     :culture/country "KAZ"
     :culture/kind :dish
     :culture/summary "Meat, noodles and onion broth dish of Central Asian cuisine, usually made from finely chopped boiled meat mixed with egg noodles and an onion sauce; shared across Central Asia including Kazakhstan."
     :culture/url "https://en.wikipedia.org/wiki/Beshbarmak"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "kaz.dish.baursak"
     :culture/name "Baursak"
     :culture/country "KAZ"
     :culture/kind :dish
     :culture/summary "Fried dough food found in the cuisines of Central Asia, Idel-Ural, Mongolia and the Middle East; part of Kazakh cuisine, where 856 kg of baursaks were cooked in one day in Almaty in 2014."
     :culture/url "https://en.wikipedia.org/wiki/Boortsog"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "kaz.dish.kuurdak"
     :culture/name "Kuurdak"
     :culture/name-local "қуырдақ"
     :culture/country "KAZ"
     :culture/kind :dish
     :culture/summary "Traditional Central Asian dish of fried meat and offal seasoned with onions and garlic, served since the era of the nomads; part of Kazakh cuisine."
     :culture/url "https://en.wikipedia.org/wiki/Kuurdak"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "kaz.beverage.kumis"
     :culture/name "Kumis"
     :culture/country "KAZ"
     :culture/kind :beverage
     :culture/summary "Fermented dairy drink made from mare's milk, important to the Turkic and Mongolic peoples of the Central and East Asian steppes, including the Kazakhs."
     :culture/url "https://en.wikipedia.org/wiki/Kumis"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "kaz.beverage.shubat"
     :culture/name "Shubat"
     :culture/country "KAZ"
     :culture/kind :beverage
     :culture/summary "Fermented camel-milk beverage of Central Asia (chal); known in Kazakhstan as shubat, where it is a staple summer food."
     :culture/url "https://en.wikipedia.org/wiki/Chal"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "kaz.craft.yurt"
     :culture/name "Yurt"
     :culture/country "KAZ"
     :culture/kind :craft
     :culture/summary "Portable round tent of Central Asian nomads; in Kazakh tradition the shangyrak crown is passed from father to son, and a stylized version of the crown is at the center of the coat of arms of Kazakhstan."
     :culture/url "https://en.wikipedia.org/wiki/Yurt"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "kaz.festival.nauryz"
     :culture/name "Nauryz (Nowruz)"
     :culture/country "KAZ"
     :culture/kind :festival
     :culture/summary "Spring-equinox New Year festival; a four-day public holiday in Kazakhstan, where the new year is started by cooking nauryz kozhe, and inscribed for Kazakhstan among other countries as UNESCO intangible cultural heritage."
     :culture/url "https://en.wikipedia.org/wiki/Nowruz"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "kaz.heritage.mausoleum-of-khoja-ahmed-yasawi"
     :culture/name "Mausoleum of Khoja Ahmed Yasawi"
     :culture/country "KAZ"
     :culture/kind :heritage
     :culture/summary "Timurid mausoleum in Turkestan, southern Kazakhstan, commissioned by Timur in 1389 and designated a UNESCO World Heritage Site in 2003."
     :culture/url "https://en.wikipedia.org/wiki/Mausoleum_of_Khoja_Ahmed_Yasawi"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "kaz.heritage.tanbaly"
     :culture/name "Tanbaly (Tamgaly)"
     :culture/country "KAZ"
     :culture/kind :heritage
     :culture/summary "Petroglyph site in the Zhetysu region of Kazakhstan with approximately 5,000 petroglyphs, inscribed as a UNESCO World Heritage Site in 2004."
     :culture/url "https://en.wikipedia.org/wiki/Tanbaly"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}]})

(defn spec-basis [iso3] (get catalog iso3))

(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s)
         missing (remove catalog iso3s)]
     {:requested (count iso3s)
      :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note (str "cloud-itonami-iso3166-kaz culture catalog "
                 "(ADR-2607171400 addendum 2, Wave 1): " (count (get catalog "KAZ"))
                 " KAZ entries, each with a fetched-and-read citation. "
                 "Extend `culture.facts/catalog`, never fabricate an id/url.")})))

(defn by-kind [iso3 kind]
  (filterv #(= (:culture/kind %) kind) (spec-basis iso3)))
