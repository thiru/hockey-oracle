;; ## Summary
;;
;; The game schedule page.
;; 
(ns hockeyoracle.web.routes.schedule
  (:require
            [clojure.string :as string]
            [compojure.route :as route]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]

            [thiru.logging :refer :all]
            [thiru.reporting :refer :all]
            [thiru.utils :refer :all]

            [hockeyoracle.app :as app]
            [hockeyoracle.core.db :as db]
            [hockeyoracle.web.routes.error-403 :refer :all]
            [hockeyoracle.web.routes.error-404 :refer :all]
            [hockeyoracle.web.routes.template :refer :all]))

(defn build-schedule-page
  [req user league games]
  (template-page
    req
    "Schedule"
    [:div

      ;; New Game button
      (if (:commissioner? user) ;TODO
        [:section#edit-controls
          [:a.button
            {:href (str "/"
                        (string/lower-case (:tricode league))
                        "/new-game")}
            "New Game"]])

      (if (empty? games)

        ;; No games scheduled!
        [:h2.no-games "No games scheduled"]

        ;; List of games
        [:div
          [:header#schedule.blue-heading
            [:h2 "Schedule"]
            [:span (str " (" (count games) ")")]]
          [:ul#schedule-list.data-list
            (for [game games]
              [:li.game-item
                [:span
                  [:a.game-time
                    {:href (str "/"
                                (string/lower-case (:tricode league))
                                "/games/"
                                (:id game))}
                    [:span.game-time (:game-time game)]]
                  [:span.clear-fix]]])]])]

    :league league))

(defn get-schedule-page
  "Game schedule page."
  [req league-tricode]
  (let [league (db/get-league {:tricode league-tricode})
        user (-> req :session :user)]

    (cond (empty? league)
          ;; League not found!
          (route/not-found get-error-404-page)

          ;; User doesn't belong to league!
          (not-any? #(= % (:id league)) (:league_ids user))
          (route/not-found get-error-403-page)

          ;; Render schedule page
          :else
          (build-schedule-page
            req
            user
            league
            (db/get-games (:id league) :progress ["new" "underway"])))))
  
