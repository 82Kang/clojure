(ns guestbook.routes.home
  (:require
   [guestbook.layout :as layout]               ;; TODO:
   [clojure.java.io :as io]                    ;; java io standard lib
   [guestbook.db.core :as db]                  ;; exposes the database functions
   [guestbook.middleware :as middleware]       ;; TODO:
   [ring.util.response]                        ;; TODO:
   [struct.core :as st]                        ;; for validating the user input
   [ring.util.http-response :as response]))    ;; TODO:

(def message-schema
  [[:name
    st/required
    st/string]
   [:message
    st/required
    st/string
    {:message "message must contain at least 10 characters"
     :validate (fn [msg] (>= (count msg) 10))}]])

(defn validate-message [params]
  (first (st/validate params message-schema)))

(defn home-page [{:keys [flash] :as request}]
  (layout/render request "home.html" 
                 (merge {:messages (db/get-messages)}
                        (select-keys flash [:name :message :errors]))))

(defn about-page [request]
  (layout/render request "about.html"))

(defn save-message! [{:keys [params]}]
  ;; Fetch the params from the HTTP request
  ;; Original request is passed as a map, where params key holds the user data 
  (if-let [errors (validate-message params)]
    ;; TODO: Order of the function call of two functions below.
    (-> (response/found "/")
        ;; Create a flash session, to pass the error to the UI
        (assoc :flash (assoc params :errors errors)))
    (do
      (db/save-message! params)
      (response/found "/"))))

(defn home-routes []
  [ "" 
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["/" {:get home-page}]
   ["/about" {:get about-page}]
   ["/message" {:post save-message!}]])
