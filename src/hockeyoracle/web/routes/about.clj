;; ## Summary
;;
;; The about page.
;; 
(ns hockeyoracle.web.routes.about
  (:require
            [clojure.string :as string]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [java-time :as jt]

            [thiru.logging :refer :all]
            [thiru.reporting :refer :all]
            [thiru.utils :refer :all]

            [hockeyoracle.app :as app]
            [hockeyoracle.core.db :as db]
            [hockeyoracle.web.routes.template :refer :all]))

(defn get-about-page
  "About page."
  [req]
  ;; Otherwise show list of leagues (if any)
  (template-page
    req
    "About"
    [:div
      [:p "The Hockey Oracle is a website intended to ease management of "
          "amateur hockey leagues."]

      [:p "Current features:"]
      [:ul
        [:li "Generate random teams from a pool of " [:em "active"] " players"]
        [:li "Manage a schedule of games including"]
        [:ul
          [:li "Sending out email reminders of upcoming games"]
          [:li "Keeping track of player status per game, e.g.:"
            [:ul
              [:li "confirmed to play"]
              [:li "unable to play"]
              [:li "undecided"]]]]]

      [:p
        [:span "Please note that this is an "]
        [:strong "alpha "]
        [:span "version of the website with limited functionality."]]

      [:table.brief-table
        [:tr
          [:td "Version"]
          [:td (:version @app/config)]]
        [:tr
          [:td "Last Updated"]
          [:td.utc-time (:updated @app/config)]]
        [:tr
          [:td "Source Code"]
          [:td [:a {:href "https://github.com/thiru/hockey-oracle"}
                 "Hosted on Github"]]]
        [:tr
          [:td "License"]
          [:td
            [:a {:href "https://www.gnu.org/licenses/gpl-3.0.html"}
              "GPLv3"]]]
        [:tr
          [:td "Copyright"]
          [:td "2014-"
               (.toString (jt/year (jt/local-date)))
               " Thirushanth Thirunavukarasu"]]]]))
