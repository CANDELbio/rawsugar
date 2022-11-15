(ns org.parkerici.rawsugar.excel
  (:require [dk.ative.docjure.spreadsheet :as docjure]
            [org.parkerici.multitool.core :as u]
            [me.raynes.fs :as fs]
            ))

(defn read-row
  [row]
  (map docjure/read-cell (docjure/cell-seq row)))

(defn row-size
  [row]
  (if row (count (filter identity (read-row row))) 0))

(defn filtered-rows
  "Filters sheet to only rows that are close to the typical size (hopefully eliminating typical garbage side-stuff)"
  [sheet]
  (let [rows (filter #(pos? (row-size %)) (docjure/row-seq sheet))
        row-sizes (map row-size rows)
        typical (and (not (empty? rows))
                     (first (u/max-by val (frequencies row-sizes))))]
    (filter #(> (row-size %) (* typical 0.8)) rows)))

(defn read-sheet
  [sheet]
  (let [rows (filtered-rows sheet)
        ;; Assuming a header row.
        cols (and (not (empty? rows))
                  (filter identity (read-row (first rows))))]
    {:columns cols
     :data 
     (map (fn [row]
            (zipmap cols (read-row row)))
          (filter identity (rest rows)))}))

(defn read-excel-file
  "Read an excel file. Returns a map of sheet names to sheet data.
  Sheet names are the base filename plus '.<sheetname>' if there are >1 sheets"
  [file & [filename]]
  (let [sheets (docjure/sheet-seq (docjure/load-workbook file))
        base-filename (fs/base-name (or filename file) true)]
    (into {}
          (remove (comp empty? second)
                  (zipmap (map #(if (= 1 (count sheets))
                                  base-filename
                                  (str base-filename "." (docjure/sheet-name %)))
                               sheets)
                          (map read-sheet sheets))))))
