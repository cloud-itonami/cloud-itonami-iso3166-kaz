(ns marketentry.facts "Kazakhstan market-entry catalog.")
(def catalog
  {"KAZ" {:name "Kazakhstan"
          :owner-authority "goszakup.gov.kz / Ministry of Finance"
          :legal-basis "Law on Public Procurement"
          :national-spec "goszakup e-procurement + BIN/IIN"
          :provenance "https://www.goszakup.gov.kz/"
          :required-evidence ["BIN/IIN record" "goszakup registration record" "Business registry extract" "Authorized-representative record"]
          :rep-owner-authority "contracting authorities / MoF"
          :rep-legal-basis "Kazakhstani legal entity (BIN) typically required for goszakup awards"
          :rep-provenance "https://www.goszakup.gov.kz/"
          :corporate-number-owner-authority "Ministry of Justice / SRC"
          :corporate-number-legal-basis "BIN (business identification number)"
          :corporate-number-provenance "https://egov.kz/"}
   "USA" {:name "United States" :owner-authority "GSA/SAM.gov" :legal-basis "FAR" :national-spec "SAM.gov" :provenance "https://sam.gov/"
          :required-evidence ["EIN record" "SAM.gov registration record" "State business registration record" "SAM UEI verification record"]}
   "RUS" {:name "Russian Federation" :owner-authority "EIS" :legal-basis "44-FZ" :national-spec "EIS" :provenance "https://zakupki.gov.ru/"
          :required-evidence ["INN/OGRN record" "EIS registration" "EGRUL extract" "Authorized-representative record"]}
   "CHN" {:name "China" :owner-authority "CCGP" :legal-basis "GPL" :national-spec "CCGP" :provenance "http://www.ccgp.gov.cn/"
          :required-evidence ["USCC record" "CCGP registration" "Business license" "Authorized-representative record"]}})

(defn spec-basis [iso3] (get catalog iso3))
(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s) missing (remove catalog iso3s)]
     {:requested (count iso3s) :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note "R0 catalog seed"})))
(defn required-evidence-satisfied? [iso3 submitted]
  (when-let [{:keys [required-evidence]} (spec-basis iso3)]
    (= (count required-evidence) (count (filter (set submitted) required-evidence)))))
(defn evidence-checklist [iso3] (:required-evidence (spec-basis iso3) []))
(defn rep-spec-basis [iso3]
  (when-let [sb (spec-basis iso3)]
    (when (:rep-owner-authority sb)
      (select-keys sb [:rep-owner-authority :rep-legal-basis :rep-provenance]))))
(defn corporate-number-spec-basis [iso3]
  (when-let [sb (spec-basis iso3)]
    (when (:corporate-number-owner-authority sb)
      (select-keys sb [:corporate-number-owner-authority :corporate-number-legal-basis :corporate-number-provenance]))))
