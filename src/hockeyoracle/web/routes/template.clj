;; ## Summary
;;
;; Common template page for all pages.
;; 
(ns hockeyoracle.web.routes.template
  (:require
            [clojure.string :as string]
            [clojure.pprint :refer [cl-format]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]

            [thiru.utils :refer :all]
            [thiru.logging :refer :all]
            [thiru.reporting :refer :all]

            [hockeyoracle.app :as app]))

(defn gen-main-id
  "Generate a safe string to be used as the `main` HTML element id.
  
  The main intention being to use it to target page-specific CSS styles."
  [page-title]
  (-> page-title
      (string/replace #"[^\w\d\s']+" "-")
      (string/lower-case)
      (str "-page")))

(defn template-page
  "Common template used by all pages on the site.
  
  * `req`
    * The ring request map
  * `title`
    * The title of the page
  * `content`
    * Hiccup structure containing the body of the page
  * `league`
    * The current league (if any)
  * `css-files`
    * An optional list of CSS files to include
  * `script-files`
    * An optional list of Javascript files to include"
  [req title content & {:keys [league css-files script-files]}]
  (let [user (-> req :session :user)]
    (html5
      {:lang "en"
       :id "root"
       :data-user (if (non-empty? user) (:id user))
       :data-league (if (non-empty? league) (:name league))}
      [:head
   
       ;; Meta
       [:meta {:charset "utf-8"}]
       [:meta {:http-equiv "X-UA-Compatible"
               :content "IE=edge"}]
       [:meta {:name "robots" :content "noindex, nofollow"}]
       [:meta {:name "theme-color" :content "#0F83BC"}]
       [:meta {:name "viewport"
               :content "width=device-width, initial-scale=1"}]
  
       ;; Title
       [:title (str title
                    " - "
                    (if (non-empty? league)
                      (str (:name league) " - "))
                    (:name @app/config))]
  
       ;; Manifest (for smart phone icon)
       [:link {:href "/manifest.json" :rel "manifest"}]

       ;; Fav Icon
       [:link {:href "/images/favicon.ico" :rel "icon"}]
  
       ;; CSS
       [:link {:href (str "/css/main.css?v=" (:version @app/config))
               :rel "stylesheet"}]
       [:link {:href "https://code.cdn.mozilla.net/fonts/fira.css"
               :rel "stylesheet"}]
       (if (non-empty? css-files)
         (for [cf css-files]
           [:link {:href (str cf "?v=" (:version @app/config))
                   :rel "stylesheet"}]))
       
       ;; Scripts (dependencies)
       [:script
         {:defer ""
          :src "/deps/font-awesome/svg-with-js/js/fontawesome-all.min.js"}]
       [:script {:src "/deps/jquery/jquery-2.1.3.min.js"}]
       [:script {:src "/deps/lodash/lodash.min.js"}]
       [:script {:src "/deps/momentjs/moment.min.js"}]
       [:script {:src "/deps/rxjs/rx.all.min.js"}]

       ;; Scripts (domain)
       [:script {:src (str "/js/utils.js?v=" (:version @app/config))}]
       [:script {:src (str "/js/utils/ui.js?v=" (:version @app/config))}]
       [:script {:src (str "/js/main.js?v=" (:version @app/config))}]
       (if (non-empty? script-files)
         (for [sf script-files]
           [:script {:src (str sf "?v=" (:version @app/config))}]))]

      ;; HTML Body
      [:body
        [:div#overlay "&nbsp;"]
        [:div#top-shade]

        ;; Header
        [:header#top-heading
          [:div#top-right-heading
            (when (non-empty? league)
              [:a {:href (cl-format "/~(~A~)" (:tricode league))
                   :title (str (:name league))}
                  (str (:name league))]
              [:span " - "])
            (when (non-empty? user)
              [:a {:href (if (non-empty? league)
                            (cl-format "/~(~A~)/users/me"
                                       (:tricode league))
                            "/users/me")}
                  (str (:name user))]
              [:a {:href "/logout"
                   :title "Log out"}
                  [:i.fa.fa-sign-out-alt]])
            (when (empty? user)
              [:a {:href "javascript:void(0)"
                   :onclick "page.showLogin()"}
                "Log in"])]
          [:a {:href "/"}
            [:img {:alt "logo"
                   :class "logo"
                   :src "/images/banner.jpg"}]
            [:span {:class "title"}
              "Hockey Oracle"]]]

        ;[:header#site-header
          ;[:div#site-name
            ;[:a {:href "/" :title "Go home"} "Hockey Oracle"]]
          ;[:div#account-links
            ;(if (and user (not= "/logout" (-> req :uri)))
              ;[:span
                ;[:a#account-link
                  ;{:href "/todo" :title "View account details"}
                  ;user
                ;"&nbsp;"
                ;[:a {:href "/logout" :title "Logout"}
                  ;[:i.fas.fa-sign-out-alt]]
            ;(if (and (nil? user) (not= "/login" (-> req :uri)))
              ;[:span
                ;[:a#login-link {:href "/login"}
                  ;[:i.fas.fa-sign-in-alt]
                  ;"&nbsp;Login"]]]
        [:nav]
        [:main {:id (gen-main-id title)} content]])))
