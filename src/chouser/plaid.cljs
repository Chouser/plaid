(ns chouser.plaid
  (:require-macros [chouser.plaid-macros :refer [rect h hc v vc r rc defdraw]])
  (:require [goog.dom :as dom]
            [clojure.string :as str]))

(enable-console-print!)

(println "This text is printed from src/chouser/plaid.cljs.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:code-meta nil, :code nil}))

(def plaid (dom/getElement "plaid"))

(defn svg-node [tag-name]
  (js/document.createElementNS "http://www.w3.org/2000/svg" tag-name))

(defn set-attr [obj attr x]
  (.setAttribute obj (name attr) x)
  obj)

(defn args [n & xs]
  (str (name n) "(" (str/join "," xs) ") "))

(defn split
  ([coll base] (list coll))
  ([coll base n & ns]
   (lazy-seq
    (cons
     (take (- n base) coll)
     (apply split (drop (- n base) coll) (max n base) ns)))))

(defn showcode [event onoff codeptr]
  (let [div (dom/getElement "code")]
    (when (= onoff :on)
      (dom/removeChildren div)
      (let [lines (:code @app-state)
            line (- (:line codeptr) (:line (:code-meta @app-state)))
            end-line (- (:end-line codeptr) (:line (:code-meta @app-state)))
            [b1 b2 b3 b4 b5] (map #(apply str %)
                                  (split lines 0
                                         line
                                         (inc line)
                                         end-line
                                         (inc end-line)))]
        (prn b2 b3 b4)
        (if (seq b3)
          (dom/append div nil
                      b1
                      (subs b2 0 (dec (:column codeptr)))
                      (dom/createDom "span" "hilite"
                                     (subs b2 (dec (:column codeptr)))
                                     b3
                                     (subs b4 0 (dec (:end-column codeptr))))
                      (subs b4 (dec (:end-column codeptr)))
                      b5)
          ;; Special case for single-line hilites
          (let [len (- (:end-column codeptr) (:column codeptr) -2)]
            (dom/append div nil
                        b1
                        (subs b2 0 (dec (:column codeptr)))
                        (dom/createDom "span" "hilite"
                                       (subs b2 (dec (:column codeptr)) len))
                        (subs b2 len)
                        b5)))))))

(defn rect* [parent-node codeptr x y w h f & [fill-opacity]]
  (let [rect (svg-node "rect")]
    (.appendChild parent-node rect)
    (doto rect
      (.addEventListener "mouseover" #(showcode % :on codeptr))
      (.addEventListener "mouseout" #(showcode % :off codeptr))
      (set-attr :x x)
      (set-attr :y y)
      (set-attr :width w)
      (set-attr :height h)
      (set-attr :fill f)
      (set-attr :fill-opacity (or fill-opacity 1)))))

(defn g [parent-node transform]
  (let [g (svg-node "g")]
    (.appendChild parent-node g)
    (set-attr g :transform transform)))

(def repeat-right
  (for [r (range 3)]
    (args :translate (* 100 r) 0)))

(def repeat-down
  (for [r (range 3)]
    (args :translate 0 (* 100 r))))

(defn v* [parent-node codeptr x width fill & [fill-opacity]]
  (doseq [t repeat-right]
    (set-attr (rect* parent-node codeptr x 0 width 300 fill fill-opacity)
              :transform t)))

(defn vc* [parent-node codeptr x width fill & [fill-opacity]]
  (v* parent-node codeptr (- x (/ width 2)) width fill fill-opacity))

(defn h* [parent-node codeptr y height fill & [fill-opacity]]
  (doseq [t repeat-down]
    (set-attr (rect* parent-node codeptr 0 y 300 height fill fill-opacity)
              :transform t)))

(defn hc* [parent-node codeptr y height fill & [fill-opacity]]
  (h* parent-node codeptr (- y (/ height 2)) height fill fill-opacity))

(defn r* [parent-node codeptr x y width height fill & [fill-opacity]]
  (doseq [t1 repeat-down
          t2 repeat-right]
    (set-attr (rect* parent-node codeptr x y width height fill fill-opacity)
              :transform (str t1 t2))))

(defn rc* [parent-node codeptr x y width height fill & [fill-opacity]]
  (r* parent-node codeptr (- x (/ width 2)) (- y (/ height 2)) width height fill fill-opacity))



(defdraw [p]
  (rect p 0 0 300 300 "#eebb66") ;; base color

  (vc p 50
      20


      "#0000ff") ;; extra at the end
  (hc p 50 20 (args :rgb 255 0 0) 0.5)
  (doseq [x (range 20 81 6)
          :when (not (< 40 x 60))]
    (vc p x 2 "#ffffff" 0.3))
  (rc p 50 50 10 10 "#ff55ff"))



(defn redraw [parent-node]
  (dom/removeChildren parent-node)
  (doto (dom/getElement "code")
    dom/removeChildren
    (.appendChild (dom/createTextNode (str/join "" (:code @app-state)))))
  (let [p (g parent-node (args :scale 3 3))]
    (draw p)))

(defonce init (redraw plaid))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  (redraw plaid)
)
