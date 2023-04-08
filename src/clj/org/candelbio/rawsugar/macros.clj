(ns org.candelbio.rawsugar.macros
  (:require [environ.core :as env]))

;;; This is to pass the value into the cljs build. Maybe there's an easier way.
(defmacro ag-grid-license []
  (env/env :ag-grid-license))
