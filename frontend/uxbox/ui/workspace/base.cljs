(ns uxbox.ui.workspace.base
  (:require [beicon.core :as rx]
            [cats.labs.lens :as l]
            [uxbox.rstore :as rs]
            [uxbox.state :as s]
            [uxbox.data.projects :as dp]
            [uxbox.data.workspace :as dw]
            [uxbox.util.lens :as ul]
            [uxbox.ui.util :as util]
            [goog.events :as events])
  (:import goog.events.EventType))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lenses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:static project-state
  (as-> (ul/dep-in [:projects-by-id] [:workspace :project]) $
    (l/focus-atom $ s/state)))

(def ^:static page-state
  (as-> (ul/dep-in [:pages-by-id] [:workspace :page]) $
    (l/focus-atom $ s/state)))

(def ^:static pages-state
  (as-> (ul/getter #(let [pid (get-in % [:workspace :project])]
                        (dp/project-pages % pid))) $
    (l/focus-atom $ s/state)))

(def ^:static shapes-state
  (as-> (ul/getter (fn [state]
                     (let [pid (get-in state [:workspace :page])
                           shapes (->> (vals (:shapes-by-id state))
                                       (filter #(= (:page %) pid)))]
                       (into [] shapes)))) $
    (l/focus-atom $ s/state)))

(def ^:static workspace-state
  (as-> (l/in [:workspace]) $
    (l/focus-atom $ s/state)))

(def ^:static selected-state
  (as-> (l/in [:workspace :selected]) $
    (l/focus-atom $ s/state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scroll Stream
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce ^:private scroll-bus (rx/bus))
(defonce scroll-s (rx/dedupe scroll-bus))

(defonce top-scroll-s
  (->> scroll-bus
       (rx/map :top)
       (rx/dedupe)))

(defonce left-scroll-s
  (->> scroll-bus
       (rx/map :left)
       (rx/dedupe)))

(defonce top-scroll (rx/to-atom top-scroll-s))
(defonce left-scroll (rx/to-atom left-scroll-s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse Position Stream
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce shapes-dragging? (atom false))

(defonce selrect-dragging? (atom false))
(defonce selrect-pos (atom nil))

(defonce mouse-b (rx/bus))
(defonce mouse-s (rx/dedupe mouse-b))

;; Deltas

(defn- coords-delta
  [[old new]]
  (let [[oldx oldy] old
        [newx newy] new]
    [(- newx oldx)
     (- newy oldy)]))

(defonce mouse-delta-s
  (->> mouse-s
       (rx/sample 10)
       (rx/buffer 2 1)
       (rx/map coords-delta)))

(defonce $$drag-subscription$$
  (as-> mouse-delta-s $
    (rx/filter #(deref shapes-dragging?) $)
    (rx/on-value $ (fn [delta]
                     (doseq [sid @selected-state]
                       (rs/emit! (dw/move-shape sid delta)))))))

(defn selrect->rect
  [data]
  (let [start (:start data)
        current (:current data )
        start-x (min (first start) (first current))
        start-y (min (second start) (second current))
        current-x (max (first start) (first current))
        current-y (max (second start) (second current))
        width (- current-x start-x)
        height (- current-y start-y)]
    {:x start-x
     :y start-y
     :width (- current-x start-x)
     :height (- current-y start-y)}))

(defonce $$selrect-subscription-0$$
  (let [ss (as-> (rx/from-atom selrect-dragging?) $
             (rx/dedupe $)
             (rx/merge $ (rx/of false))
             (rx/buffer 2 1 $)
             (rx/share $))]
    (as-> ss $
      (rx/filter #(= (vec %) [false true]) $)
      (rx/with-latest-from vector mouse-s $)
      (rx/on-value $ (fn [[_ pos]]
                       (swap! selrect-pos assoc
                              :start pos
                              :current pos))))
    (as-> ss $
      (rx/filter #(= (vec %) [true false]) $)
      (rx/on-value $ (fn []
                       (let [selrect (selrect->rect @selrect-pos)]
                         (rs/emit! (dw/select-shapes selrect))
                         (reset! selrect-pos nil)))))))

(defonce $$selrect-subscription-1$$
  (as-> mouse-s $
    (rx/filter #(deref selrect-dragging?) $)
    (rx/on-value $ (fn [pos]
                     (swap! selrect-pos assoc :current pos)))))

;; Materialized views

(defonce mouse-position (rx/to-atom (rx/sample 50 mouse-s)))

(defn- mouse-mixin-did-mount
  [own]
  (letfn [(on-mousemove [event]
            (let [canvas (util/get-ref-dom own "canvas")
                  brect (.getBoundingClientRect canvas)
                  offset-x (.-left brect)
                  offset-y (.-top brect)
                  x (.-clientX event)
                  y (.-clientY event)]
              (rx/push! mouse-b [(- x offset-x)
                                 (- y offset-y)])))]
    (let [key (events/listen js/document EventType.MOUSEMOVE on-mousemove)]
      (js/console.log "mouse-mixin-did-mount" key)
      (assoc own ::eventkey key))))

(defn- mouse-mixin-will-unmount
  [own]
  (let [key (::eventkey own)]
    (events/unlistenByKey key)
    (dissoc own ::eventkey)))

(defn- mouse-mixin-transfer-state
  [old-own own]
  (let [key (::eventkey old-own)]
    (assoc own ::eventkey key)))

(def ^:static mouse-mixin
  {:did-mount mouse-mixin-did-mount
   :will-unmount mouse-mixin-will-unmount
   :transfer-state mouse-mixin-transfer-state})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def viewport-height  2048)
(def viewport-width 2048)

(def document-start-x 50)
(def document-start-y 50)

