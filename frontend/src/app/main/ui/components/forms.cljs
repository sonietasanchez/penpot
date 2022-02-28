;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.components.forms
  (:require
   [app.common.data :as d]
   [app.main.ui.icons :as i]
   [app.util.dom :as dom]
   [app.util.forms :as fm]
   [app.util.i18n :as i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [app.util.object :as obj]   
   [clojure.string]
   [cuerdas.core :as str]
   [rumext.alpha :as mf]))

(def form-ctx (mf/create-context nil))
(def use-form fm/use-form)

(mf/defc input
  [{:keys [label help-icon disabled form hint trim children data-test] :as props}]
  (let [input-type   (get props :type "text")
        input-name   (get props :name)
        more-classes (get props :class)
        auto-focus?  (get props :auto-focus? false)

        form         (or form (mf/use-ctx form-ctx))

        type'        (mf/use-state input-type)
        focus?       (mf/use-state false)

        is-checkbox? (= @type' "checkbox")
        is-radio?    (= @type' "radio")
        is-text?     (or (= @type' "password")
                         (= @type' "text")
                         (= @type' "email"))

        touched?     (get-in @form [:touched input-name])
        error        (get-in @form [:errors input-name])

        value        (get-in @form [:data input-name] "")

        help-icon'   (cond
                       (and (= input-type "password")
                            (= @type' "password"))
                       i/eye

                       (and (= input-type "password")
                            (= @type' "text"))
                       i/eye-closed

                       :else
                       help-icon)

        klass (str more-classes " "
                   (dom/classnames
                     :focus          @focus?
                     :valid          (and touched? (not error))
                     :invalid        (and touched? error)
                     :disabled       disabled
                     :empty          (and is-text? (str/empty? value))
                     :with-icon      (not (nil? help-icon'))
                     :custom-input   is-text?
                     :input-radio    is-radio?
                     :input-checkbox is-checkbox?))

        swap-text-password
        (fn []
          (swap! type' (fn [input-type]
                         (if (= "password" input-type)
                           "text"
                           "password"))))

        on-focus  #(reset! focus? true)
        on-change (fn [event]
                    (let [target (dom/get-target event)
                          value  (if (or (= (.-type target) "checkbox")
                                         (= (.-type target) "radio"))
                                   (.-checked target)
                                   (dom/get-value target))]
                      (fm/on-input-change form input-name value trim)))

        on-blur
        (fn [_]
          (reset! focus? false)
          (when-not (get-in @form [:touched input-name])
            (swap! form assoc-in [:touched input-name] true)))

        props (-> props
                  (dissoc :help-icon :form :trim :children)
                  (assoc :id (name input-name)
                         :value value
                         :auto-focus auto-focus?
                         :on-focus on-focus
                         :on-blur on-blur
                         :placeholder label
                         :on-change on-change
                         :type @type')
                  (obj/clj->props))]

    [:div
     {:class klass}
     [:*
      [:> :input props]
      (cond
        (some? label)
        [:label {:for (name input-name)} label]

        (some? children)
        [:label {:for (name input-name)} children])

      (when help-icon'
        [:div.help-icon
         {:style {:cursor "pointer"}
          :on-click (when (= "password" input-type)
                      swap-text-password)}
         help-icon'])
      (cond
        (and touched? (:message error))
        [:span.error {:data-test (clojure.string/join [data-test "-error"]) }(tr (:message error))]

        (string? hint)
        [:span.hint hint])]]))


(mf/defc textarea
  [{:keys [label disabled form hint trim] :as props}]
  (let [input-name (get props :name)

        form     (or form (mf/use-ctx form-ctx))

        focus?   (mf/use-state false)

        touched? (get-in @form [:touched input-name])
        error    (get-in @form [:errors input-name])

        value    (get-in @form [:data input-name] "")

        klass    (dom/classnames
                  :focus     @focus?
                  :valid     (and touched? (not error))
                  :invalid   (and touched? error)
                  :disabled  disabled
                  ;; :empty     (str/empty? value)
                  )

        on-focus  #(reset! focus? true)
        on-change (fn [event]
                    (let [target (dom/get-target event)
                          value  (dom/get-value target)]
                      (fm/on-input-change form input-name value trim)))

        on-blur
        (fn [_]
          (reset! focus? false)
          (when-not (get-in @form [:touched input-name])
            (swap! form assoc-in [:touched input-name] true)))

        props (-> props
                  (dissoc :help-icon :form :trim)
                  (assoc :value value
                         :on-focus on-focus
                         :on-blur on-blur
                         ;; :placeholder label
                         :on-change on-change)
                  (obj/clj->props))]

    [:div.custom-input
     {:class klass}
     [:*
      [:label label]
      [:> :textarea props]
      (cond
        (and touched? (:message error))
        [:span.error (tr (:message error))]

        (string? hint)
        [:span.hint hint])]]))

(mf/defc select
  [{:keys [options label form default data-test] :as props
    :or {default ""}}]
  (let [input-name (get props :name)

        form      (or form (mf/use-ctx form-ctx))
        value     (or (get-in @form [:data input-name]) default)
        cvalue    (d/seek #(= value (:value %)) options)
        on-change (fn [event]
                    (let [target (dom/get-target event)
                          value  (dom/get-value target)]
                      (fm/on-input-change form input-name value)))]

    [:div.custom-select
     [:select {:value value
               :on-change on-change
               :data-test data-test}
      (for [item options]
        [:option {:key (:value item) :value (:value item)} (:label item)])]

     [:div.input-container
      [:div.main-content
       [:label label]
       [:span.value (:label cvalue "")]]

      [:div.icon
       i/arrow-slide]]]))

(mf/defc submit-button
  [{:keys [label form on-click disabled data-test] :as props}]
  (let [form (or form (mf/use-ctx form-ctx))]
    [:input.btn-primary.btn-large
     {:name "submit"
      :class (when-not (:valid @form) "btn-disabled")
      :disabled (or (not (:valid @form)) (true? disabled))
      :on-click on-click
      :value label
      :data-test data-test
      :type "submit"}]))

(mf/defc form
  [{:keys [on-submit form children class] :as props}]
  (let [on-submit (or on-submit (constantly nil))]
    [:& (mf/provider form-ctx) {:value form}
     [:form {:class class
             :on-submit (fn [event]
                          (dom/prevent-default event)
                          (on-submit form event))}
      children]]))



(mf/defc multi-input-row         
  [{:keys [item, remove-item!]}]
  (let [valid (val item)
        text (key item)]        
    [:div
     [:div.invite-member-email-text {:class (if (not valid) "invalid" "")}
      [:div.email text]
      [:div.icon {:on-click #(remove-item! (key item))}i/cross]]]))

(mf/defc multi-input
  [{:keys [label help-icon disabled form hint trim children data-test] :as props}]
  (let [input-type   (get props :type "text")
        multi-input-name   (get props :name)
        input-name         (keyword (str "single-" (name multi-input-name)))
        more-classes (get props :class)
        auto-focus?  (get props :auto-focus? false)

        form         (or form (mf/use-ctx form-ctx))

        input-ref    (mf/use-ref)

        focus?       (mf/use-state false)

        touched?     (get-in @form [:touched input-name])
        error        (get-in @form [:errors input-name])

        value        (get-in @form [:data input-name] "")

        items (mf/use-state  {})

        all-items (if (= "" value)
                    (str/join "," (keys @items))
                    (str/join "," (conj (keys @items) value)))

        klass (str more-classes " "
                   (dom/classnames
                    :focus          @focus?
                    :valid          (and touched? (not error))
                    :invalid        (and touched? error)
                    :disabled       disabled
                    :empty          (str/empty? value)))

        on-focus  #(reset! focus? true)
        on-change (fn [event]
                    (let [target (dom/get-target event)
                          value  (dom/get-value target)]
                      (fm/on-input-change form input-name value trim)
                      (fm/on-input-change form multi-input-name value trim)
                      ))
        remove-item!
        (fn [item]
          (swap! items dissoc item))

        add-item!
        (fn [item valid]
          (swap! items assoc item valid))

        on-blur
        (fn [_]
          (reset! focus? false)
          (when-not (get-in @form [:touched input-name])
            (swap! form assoc-in [:touched input-name] true)))

        input-key-down (fn [event]
                         (let [target (dom/event->target event)
                               value (dom/get-value target)
                               valid (and (not (= value "")) (dom/valid? target))]
                           (when (kbd/comma? event)
                             (dom/prevent-default event)
                             (add-item! value valid)
                             (fm/on-input-change form input-name ""))))


        input-props (-> props
                        (assoc :id (name input-name)
                               :value value
                               :auto-focus auto-focus?
                               :on-focus on-focus
                               :on-blur on-blur
                               :placeholder label
                               :on-change on-change
                               :type input-type
                               :ref input-ref
                               :on-key-down input-key-down)
                        (obj/clj->props))

        #_multi-input-props #_(-> props
                              (assoc :id (name multi-input-name)
                                     :name (name multi-input-name)
                                     :value all-items
                                     :on-change on-change)
                              (obj/clj->props))]

    [:div
     {:class klass}
     [:*
      (for [item @items]
        [:& multi-input-row {:item item :remove-item! remove-item!}])      
      [:> :input input-props]
      (cond
        (some? label)
        [:label {:for (name input-name)} label])

      (cond
        (and touched? (:message error))
        [:span.error (tr (:message error))]

        (string? hint)
        [:span.hint hint])
      
      
      [:& :input {:name (name multi-input-name)
                  :id (name multi-input-name)}]]]))



(mf/defc multi-email
  [{:keys [label help-icon disabled form hint trim children data-test] :as props}]
  (let [multi-input-name   (get props :name)
        single-input-name         (keyword (str "single-" (name multi-input-name)))
        form         (or form (mf/use-ctx form-ctx))
        value        (get-in @form [:data multi-input-name] "")
        items (mf/use-state  {})
        remove-item!
        (fn [item]
          (swap! items dissoc item))

        add-item!
        (fn [item valid]
          (swap! items assoc item valid))

        single-mail-value        (get-in @form [:data single-input-name] "")

        all-items (if (= "" single-mail-value)
                    (str/join "," (keys @items))
                    (str/join "," (conj (keys @items) single-mail-value)))

        input-key-down (fn [event]
                         (let [target (dom/event->target event)
                               value (dom/get-value target)
                               valid (and (not (= value "")) (dom/valid? target))]
                           (print props)
                           (when (kbd/comma? event)
                             (dom/prevent-default event)
                             (add-item! value valid)
                             (fm/on-input-change form single-input-name ""))))

        input-key-up #(fm/on-input-change form multi-input-name all-items trim)

        single-props (-> props
                         (assoc :name single-input-name)
                         (assoc :on-key-down input-key-down)
                         (assoc :on-key-up input-key-up))]

    [:div
     (for [item @items]
       [:& multi-input-row {:item item :remove-item! remove-item!}])
     [:& input single-props]
     [:input {:id (name multi-input-name)
              :read-only true
              :type "hidden"
              :value value}]]))