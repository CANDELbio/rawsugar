(ns org.parkerici.rawsugar.cnavigate
  (:require [bidi.bidi :as bidi]))

(def routes
  ["/" {""           :home
        "project/"   {[:project] :project}
        "batch/"     {[:project "/" :batch] :batch}
        "files/"     {[:project "/" :batch] :files}
        "sheet/"     {[:project "/" :batch "/" :sheet] :sheet}
        }])

(def url-for (partial bidi/path-for routes))


(def logo
  "Replace with your logo")
