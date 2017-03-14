(ns ^:figwheel-always om-tut.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [clojure.string :as string]))

(enable-console-print!)

(println "This text is printed from src/om-tut/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text ["Hello world!"]
                          :list ["Lion" "Zebra" "Buffalo" "Antelope"]
                          :contacts
                          [{:first "Ben" :last "Bitdiddle" :email "benb@mit.edu"}
                           {:first "Alyssa" :middle-initial "P" :last "Hacker" :email "aphacker@mit.edu"}
                           {:first "Eva" :middle "Lu" :last "Ator" :email "eval@mit.edu"}
                           {:first "Louis" :last "Reasoner" :email "prolog@mit.edu"}
                           {:first "Cy" :middle-initial "D" :last "Effect" :email "bugs@mit.edu"}
                           {:first "Lem" :middle-initial "E" :last "Tweakit" :email "morebugs@mit.edu"}]
                          :people
                          [{:type :student :first "Ben" :last "Bitdiddle" :email "benb@mit.edu"}
                           {:type :student :first "Alyssa" :middle-initial "P" :last "Hacker"
                             :email "aphacker@mit.edu"}
                           {:type :professor :first "Gerald" :middle "Jay" :last "Sussman"
                             :email "metacirc@mit.edu" :classes [:6001 :6946]}
                           {:type :student :first "Eva" :middle "Lu" :last "Ator" :email "eval@mit.edu"}
                           {:type :student :first "Louis" :last "Reasoner" :email "prolog@mit.edu"}
                           {:type :professor :first "Hal" :last "Abelson" :email "evalapply@mit.edu"
                             :classes [:6001]}]
                          :classes
                          {:6001 ["The Structure and Interpretation of Computer Programs"]
                           :6946 ["The Structure and Interpretation of Classical Mechanics"]
                           :1806 ["Linear Algebra"]}}))

;; Using reify
(om/root
  (fn [data owner]
    (reify
      om/IRender
      (render [_]
        (dom/div nil
                 (dom/h1 nil (get (:text data) 0))
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
               (dom/h1 nil (get (:text data) 0))
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

;(defn add-contact [data owner]
;  (let [new-contact (-> (om/get-node owner "new-contact")
;                        .-value
;                        parse-contact)]
;    (when new-contact
;      (om/transact! data :contacts #(conj % new-contact)))))

(defn add-contact [data owner]
  (let [input (om/get-node owner "new-contact")
        new-contact (-> input .-value parse-contact)]
    (when new-contact
      (om/transact! data :contacts #(conj % new-contact))
      (om/set-state! owner :text ""))))

(defn contact-view [contact owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [delete]}]
      (dom/li nil
              (dom/span nil (display-name contact))
              (dom/button #js {:onClick (fn [e] (put! delete @contact))} "Delete")))))


(defn handle-change [e owner {:keys [text]}]
  (let [value (.. e -target -value)]
    (if-not (re-find #"[0-9]" value)
      (om/set-state! owner :text value)
      (om/set-state! owner :text text))))

;Reminder: Notice that we're using vec to transform the result of remove (a lazy sequence)
;back into a vector, consistent with our aforementioned rule that state should only consist
;of associative data structures like maps and vectors.
(defn contacts-view [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:delete (chan)
       :text ""})
    om/IWillMount
    (will-mount [_]
      (let [delete (om/get-state owner :delete)]
        (go (loop []
              (let [contact (<! delete)]
                (om/transact! data :contacts
                              (fn [xs] (vec (remove #(= contact %) xs))))
                (recur))))))
    om/IRenderState
    (render-state [this state]
      (dom/div nil
               (dom/h2 nil "Contact list")
               (apply dom/ul nil
                      (om/build-all contact-view (:contacts data)
                                    {:init-state state}))
               (dom/div nil
                        (dom/input #js {:type "text" :ref "new-contact" :value (:text state)
                                        :onChange #(handle-change % owner state)})
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


;; A test
(defn student-view [student owner]
  (reify
    om/IRender
    (render [_]
      (dom/li nil (display-name student)))))

(defn professor-view [professor owner]
  (reify
    om/IRender
    (render [_]
      (dom/li nil
              (dom/div nil (display-name professor))
              (dom/label nil "Classes")
              (apply dom/ul nil
                     (map #(dom/li nil (str %)) (:classes professor)))))))

(defmulti entry-view (fn [person _] (:type person)))

(defmethod entry-view :student
  [person owner] (student-view person owner))

(defmethod entry-view :professor
  [person owner] (professor-view person owner))

(defn people [data]
  (->> data
       :people
       (mapv (fn [x]
               (if (:classes x)
                 (update-in x [:classes] (fn [y] (mapv #(get ((:classes data) %) 0) y)))
                 x)))))

(defn registry-view [data owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (dom/div nil
               (dom/h2 nil "Registry")
               (apply dom/ul nil
                      (om/build-all entry-view (people data)))))))

(om/root registry-view app-state
         {:target (. js/document (getElementById "registry"))})

(defn classes-view [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:id "classes"}
               (dom/h2 nil "Classes")
               (apply dom/ul nil
                      (map #(dom/li nil %) (flatten (vals (:classes data)))))))))

(om/root classes-view app-state
         {:target (. js/document (getElementById "classes"))})

;; Editable text
(defn texteditable [text owner]
  (reify
    om/IInitState
    (init-state [_]
      {:editing false})
    om/IRenderState
    (render-state [_ {:keys [editing]}]
      (dom/li nil
              (dom/span #js {:style (if (not editing)
                                      #js {}
                                      #js {:display "none"})} (get text 0))
              (dom/input
                #js {:style (if editing
                              #js {}
                              #js {:display "none"})
                     :value (get text 0)
                     :onKeyDown #(when (= (.-key %) "Enter")
                                    (om/set-state! owner :editing false))
                     :onChange #(om/update! text [0] (.. % -target -value))})
              (dom/button
                #js {:style (if (not editing)
                              #js {}
                              #js {:display "none"})
                     :onClick #(om/set-state! owner :editing true)}
                "Edit")))))

(defn text-view [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:id "classes"}
               (dom/h2 nil "Text")
               (om/build texteditable (:text data))))))

(om/root text-view app-state
         {:target (. js/document (getElementById "editabletxt"))})

; Editable classview
(defn editable [text owner]
  (reify
    om/IInitState
    (init-state [_]
      {:editing false})
    om/IRenderState
    (render-state [_ {:keys [editing]}]
      (dom/li nil
              (dom/span #js {:style (if (not editing)
                                      #js {}
                                      #js {:display "none"})} (get text 0))
              (dom/input
                #js {:style (if editing
                              #js {}
                              #js {:display "none"})
                     :value (get text 0)
                     :onKeyDown #(when (= (.-key %) "Enter")
                                    (om/set-state! owner :editing false))
                     :onChange #(om/update! text [0] (.. % -target -value))})
              (dom/button
                #js {:style (if (not editing)
                              #js {}
                              #js {:display "none"})
                     :onClick #(om/set-state! owner :editing true)}
                "Edit")))))

(defn classes-view1 [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:id "classes"}
               (dom/h2 nil "Classes")
               (apply dom/ul nil
                      (om/build-all editable (vals (:classes data))))))))

(om/root classes-view1 app-state
         {:target (. js/document (getElementById "classes1"))})


(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)

