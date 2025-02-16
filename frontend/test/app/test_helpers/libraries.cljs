(ns app.test-helpers.libraries
  (:require
   [cljs.test :as t :include-macros true]
   [cljs.pprint :refer [pprint]]
   [beicon.core :as rx]
   [potok.core :as ptk]
   [app.common.uuid :as uuid]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.common.pages.helpers :as cph]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.libraries-helpers :as dwlh]
   [app.test-helpers.pages :as thp]))

;; ---- Helpers to manage libraries and synchronization

(defn is-instance-root
  [shape]
  (t/is (some? (:shape-ref shape)))
  (t/is (some? (:component-id shape)))
  (t/is (= (:component-root? shape) true)))

(defn is-instance-subroot
  [shape]
  (t/is (some? (:shape-ref shape)))
  (t/is (some? (:component-id shape)))
  (t/is (nil? (:component-root? shape))))

(defn is-instance-head
  [shape]
  (t/is (some? (:shape-ref shape)))
  (t/is (some? (:component-id shape))))

(defn is-instance-child
  [shape]
  (t/is (some? (:shape-ref shape)))
  (t/is (nil? (:component-id shape)))
  (t/is (nil? (:component-file shape)))
  (t/is (nil? (:component-root? shape))))

(defn is-noninstance
  [shape]
  (t/is (nil? (:shape-ref shape)))
  (t/is (nil? (:component-id shape)))
  (t/is (nil? (:component-file shape)))
  (t/is (nil? (:component-root? shape)))
  (t/is (nil? (:remote-synced? shape)))
  (t/is (nil? (:touched shape))))

(defn is-from-file
  [shape file]
  (t/is (= (:component-file shape)
           (:id file))))

(defn resolve-instance
  [state root-inst-id]
  (let [page        (thp/current-page state)
        root-inst   (cph/get-shape page root-inst-id)
        shapes-inst (cph/get-children-with-self (:objects page)
                                                root-inst-id)]
    ;; Validate that the instance tree is well constructed
    (is-instance-root (first shapes-inst))
    (run! is-instance-child (rest shapes-inst))

    shapes-inst))

(defn resolve-noninstance
  [state root-inst-id]
  (let [page        (thp/current-page state)
        root-inst   (cph/get-shape page root-inst-id)
        shapes-inst (cph/get-children-with-self (:objects page)
                                                root-inst-id)]
    ;; Validate that the tree is not an instance
    (run! is-noninstance shapes-inst)

    shapes-inst))

(defn resolve-instance-and-main
  [state root-inst-id]
  (let [page          (thp/current-page state)
        root-inst     (cph/get-shape page root-inst-id)

        libs          (dwlh/get-libraries state)
        component     (cph/get-component libs (:component-id root-inst))

        shapes-inst   (cph/get-children-with-self (:objects page) root-inst-id)
        shapes-main   (cph/get-children-with-self (:objects component) (:shape-ref root-inst))

        unique-refs   (into #{} (map :shape-ref) shapes-inst)

        main-exists?  (fn [shape]
                        (t/is (some #(= (:id %) (:shape-ref shape))
                                    shapes-main)))]

    ;; Validate that the instance tree is well constructed
    (is-instance-root (first shapes-inst))
    (run! is-instance-child (rest shapes-inst))
    (run! is-noninstance shapes-main)
    (t/is (= (count shapes-inst)
             (count shapes-main)
             (count unique-refs)))
    (run! main-exists? shapes-inst)

    [shapes-inst shapes-main component]))

(defn resolve-component
  [state component-id]
  (let [page        (thp/current-page state)
        libs        (dwlh/get-libraries state)
        component   (cph/get-component libs component-id)
        root-main   (cph/get-component-root component)
        shapes-main (cph/get-children-with-self (:objects component) (:id root-main))]

    ;; Validate that the component tree is well constructed
    (run! is-noninstance shapes-main)

    [shapes-main component]))

