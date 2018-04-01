;; ## Summary
;;
;; The home page.
;; 
(ns hockeyoracle.web.routes.home
  (:require
            [clojure.string :as string]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.http-response :as hr]

            [thiru.logging :refer :all]
            [thiru.reporting :refer :all]
            [thiru.utils :refer :all]

            [hockeyoracle.app :as app]
            [hockeyoracle.core.db :as db]
            [hockeyoracle.web.routes.template :refer :all]))

(defn get-home-page
  "Home page.
  
  List available leagues if the logged in user belongs to more than one,
  otherwise redirect to the league's schedule page."
  [req]
  (let [user (-> req :session :user)]
    ;; Redirect to the league's schedule page if user only belongs to one
    ;; league
    (if (= 1 (count (:league_ids user)))

      (let [league (db/get-league {:id (first (:league_ids user))})]
        (if (empty? league)
          ;; But what if the league isn't found in the database
          (template-page
            req
            "Home"
            [:div
              [:h2 "We couldn't find the league you belong to."]
              [:h2 "Please contact your league's manager."]])
          ;; Redirect
          (hr/temporary-redirect (str "/" 
                                      (string/lower-case (:tricode league))
                                      "/games/schedule"))))

      ;; Otherwise show list of leagues (if any)
      (template-page
        req
        "Home"
        (if (zero? (count (:league_ids user)))

          ;; Player doesn't belong to any leagues
          [:div
            [:h2 "Looks like you don't belong to any leagues!"]
            [:h2 "Please contact your league's manager."]]

          ;; List of leagues
          [:div
            [:h2 "Choose your league"]
            [:ul.simple-list
              (for [league (db/get-leagues (:league_ids user))]
                [:li
                  [:a.button.wide-button
                    {:href (str "/"
                                (string/lower-case (:tricode league))
                                "/games/schedule")
                     :title (:tricode league)}
                    (:name league)]])]])))))
