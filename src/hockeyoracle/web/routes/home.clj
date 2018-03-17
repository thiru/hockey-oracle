;; ## Summary
;;
;; The home page.
;; 
(ns hockeyoracle.web.routes.home
  (:require
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]

            [thiru.logging :refer :all]
            [thiru.reporting :refer :all]
            [thiru.utils :refer :all]

            [hockeyoracle.app :as app]
            [hockeyoracle.web.routes.template :refer :all]))

(defn get-home-page
  "Home page."
  [req]
  (template-page
    req
    "Home"
    [:div
      [:h1 "TODO: Home Page"]
      [:div#content]]))
