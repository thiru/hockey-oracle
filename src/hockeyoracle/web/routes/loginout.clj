;; ## Summary
;;
;; The login and logout resources.
;; 
(ns hockeyoracle.web.routes.loginout
  (:require
            [clojure.string :as str]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [prone.debug :refer [debug]]
            [ring.util.http-response :as hr]

            [thiru.logging :refer :all]
            [thiru.reporting :refer :all]
            [thiru.utils :refer :all]

            [hockeyoracle.app :as app]
            [hockeyoracle.web.routes.template :refer :all]))

(defn get-login-page
  "The login page."
  [req & {:keys [failed-attempt? username password]}]
  (template-page
    req
    "Login"
    (if-let [user (-> req :session :user)]
      [:div
        [:h2 (str "Hey " user ",")]
        [:p "It looks like you're already logged in."]
        [:p "Would you like to "
          [:a {:href "/logout"} "log out"]
          "?"]]
      [:div
        [:h1 "Please log in to continue"]
        [:form {:action (if (-> req :params :go-back-to)
                          (str "/login?go-back-to="
                               (-> req :params :go-back-to))
                          "/login")
                :method "post"}
          [:p
            [:input#email.full-width
              {:name "username"
               :placeholder "Username or email address"
               :title "Username or email address"
               :type "text"
               :value username}]]
          [:p
            [:input#password.full-width
              {:name "password"
               :placeholder "Password"
               :title "Password"
               :type "password"
               :value password}]]
          (if failed-attempt?
            [:p.error "Invalid username or password"])
          [:p
            [:button.button.full-width "Login"]]]])
    :script-files ["/js/pages/loginout.js"]
    :css-files ["/css/pages/loginout.css"]))

(defn get-users
  "Get all registered users."
  []
  (let [file-path "users.edn"
        users (load-config file-path {})]
    (if (:empty? users)
      (log :error (str "No users configured in " file-path)))
    (:data users)))

(defn post-login-page
  "Handle user login attempt."
  [req]
  (let [go-back-to-url (-> req :params :go-back-to)
        username (get-in req [:form-params "username"])
        password (get-in req [:form-params "password"])
        users (get-users)
        valid? (some-> users
                       (get (keyword username))
                       (get :password)
                       (= password))]
    (if valid?
      (do
        (log :info (str "User '" username "' successfully logged in"))
        (-> (template-page
              req
              "Login"
              [:div#login-successful {:data-go-back-to-url go-back-to-url}
                [:h1 (str "Welcome, " username "!")]
                [:p 
                 [:i.fas.fa-cog.fa-spin]
                 " We're logging you in now..."]]
              :script-files ["/js/pages/loginout.js"]
              :css-files ["/css/pages/loginout.css"])
            (hr/ok)
            (hr/content-type "text/html")
            (assoc :session {:user username})))
      (do
        (log :warning (str "User '" username "' failed login"))
        (-> (get-login-page
              req
              :failed-attempt? true
              :username username
              :password password)
            (hr/unauthorized)
            (hr/content-type "text/html"))))))

(defn get-logout-page
  [req]
  (if-let [user (-> req :session :user)]
    (log :info (str "User '" user "' logged out")))
  (-> (template-page
        req
        "Logged Out"
        [:div
          [:h2 "You've been successfully logged out"]
          [:a.button {:href "/"} "Go back to the home page"]])
      (hr/ok)
      (hr/content-type "text/html")
      (assoc :session nil)))
