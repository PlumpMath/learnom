(ns ^:figwheel-always om-tut.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [clojure.string :as string]))

(enable-console-print!)

(println "This text is printed from src/om-tut/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"
                          :list ["Lion" "Zebra" "Buffalo" "Antelope"]
                          :contacts
                          [{:first "Ben" :last "Bitdiddle" :email "benb@mit.edu"}
                           {:first "Alyssa" :middle-initial "P" :last "Hacker" :email "aphacker@mit.edu"}
                           {:first "Eva" :middle "Lu" :last "Ator" :email "eval@mit.edu"}
                           {:first "Louis" :last "Reasoner" :email "prolog@mit.edu"}
                           {:first "Cy" :middle-initial "D" :last "Effect" :email "bugs@mit.edu"}
                           {:first "Lem" :middle-initial "E" :last "Tweakit" :email "morebugs@mit.edu"}]}))

;; Using reify
(om/root
  (fn [data owner]
    (reify
      om/IRender
      (render [_]
        (dom/div nil
                 (dom/h1 nil (:text data))
                 (dom/h3 nil "Edit this and watch it change!")
                 (apply dom/ul nil
                        (map (fn [text] (dom/li nil text)) (:list data)))))))
  app-state
  {:target (. js/document (getElementById "app0"))})

;; Using om/component instead
(om/root
  (fn [data owner]
    (om/component
      (dom/div nil
               (dom/h1 nil (:text data))
               (dom/h3 nil "Edit this and watch it change!")
               (apply dom/ul #js {:className "animals"}
                      (map (fn [text] (dom/li nil text)) (:list data))))))
  app-state
  {:target (. js/document (getElementById "app1"))})

;; First Component
(defn middle-name [{:keys [middle middle-initial]}]
  (cond
    middle (str " " middle)
    middle-initial (str " " middle-initial ".")))

(defn display-name [{:keys [first last] :as contact}]
  (str last ", " first (middle-name contact)))

;(defn contact-view [contact owner]
;  (reify
;    om/IRender
;    (render [this]
;      (dom/li nil
;              (dom/span nil (display-name contact))
;              (dom/button nil "Delete")))))

;; Adding contacts
(defn parse-contact [contact-str]
  (let [[first middle last :as parts] (string/split contact-str #"\s+")
        [first last middle] (if (nil? last) [first middle] [first last middle])
        middle (when middle (string/replace middle "." ""))
        c (if middle (count middle) 0)]
    (when (>= (count parts) 2)
      (cond-> {:first first :last last}
              (== c 1) (assoc :middle-initial middle)
              (>= c 2) (assoc :middle middle)))))

(defn add-contact [data owner]
  (let [new-contact (-> (om/get-node owner "new-contact")
                        .-value
                        parse-contact)]
    (when new-contact
      (om/transact! data :contacts #(conj % new-contact)))))

;; This is a error stuff
(defn contact-view [contact owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [delete]}]
      (dom/li nil
              (dom/span nil (display-name contact))
              (dom/button #js {:onClick (fn [e] (put! delete @contact))} "Delete")))))


;Reminder: Notice that we're using vec to transform the result of remove (a lazy sequence)
;back into a vector, consistent with our aforementioned rule that state should only consist
;of associative data structures like maps and vectors.

(defn contacts-view [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:delete (chan)})
    om/IWillMount
    (will-mount [_]
      (let [delete (om/get-state owner :delete)]
        (go (loop []
              (let [contact (<! delete)]
                (om/transact! data :contacts
                              (fn [xs] (vec (remove #(= contact %) xs))))
                (recur))))))
    om/IRenderState
    (render-state [this {:keys [delete]}]
      (dom/div nil
               (dom/h2 nil "Contact list")
               (apply dom/ul nil
                      (om/build-all contact-view (:contacts data)
                                    {:init-state {:delete delete}}))
               (dom/div nil
                        (dom/input #js {:type "text" :ref "new-contact"})
                        (dom/button #js {:onClick #(add-contact data owner)} "Add contact"))))))

;(om/root
;  (fn [data owner]
;    (reify
;      om/IRender
;      (render [this]
;        (dom/div nil
;                 (dom/h2 nil "Contact list")
;                 (apply dom/ul nil
;                        (om/build-all contact-view (:contacts data)))))))
;  app-state
;  {:target (. js/document (getElementById "contacts"))})

;; The main om-root
(om/root
  (fn [data owner]
    (reify
      om/IRender
      (render [this]
        (dom/div nil
                 (om/build contacts-view data)))))
  app-state
  {:target (. js/document (getElementById "contacts"))})


(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)

