;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.tokens.management.create.form-input-token
  (:require
   [app.common.data :as d]
   [app.common.types.tokens-lib :as ctob]
   [app.main.data.style-dictionary :as sd]
   [app.main.ui.ds.controls.input :refer [input*]]
   [app.main.ui.forms :as fc]
   [app.util.dom :as dom]
   [app.util.forms :as fm]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [clojure.core :as c]
   [rumext.v2 :as mf]))

(defn- resolve-value
  [tokens prev-token value]
  (let [token
        {:value value
         :name "__PENPOT__TOKEN__NAME__PLACEHOLDER__"}

        tokens
        (-> tokens
            ;; Remove previous token when renaming a token
            (dissoc (:name prev-token))
            (update (:name token) #(ctob/make-token (merge % prev-token token))))]

    (->> tokens
         (sd/resolve-tokens-interactive)
         (rx/mapcat
          (fn [resolved-tokens]
            (let [{:keys [errors resolved-value] :as resolved-token} (get resolved-tokens (:name token))]
              (if resolved-value
                (rx/of {:value resolved-value})
                (rx/of {:error (first errors)}))))))))

(mf/defc form-input-token*
  [{:keys [name tokens token] :rest props}]

  (let [form       (mf/use-ctx fc/context)
        input-name name

        resolved-input-name
        (mf/with-memo [input-name]
          (keyword (str "resolved-" (c/name input-name))))

        touched?
        (and (contains? (:data @form) input-name)
             (get-in @form [:touched input-name]))

        error
        (get-in @form [:errors input-name])

        value
        (get-in @form [:data input-name] "")

        ;; TODO: MOdificar este stream para que si llega un token de
        ;; composite mire si el name es diferente a value
        resolve-stream
        (mf/with-memo [token]
          (if-let [value (:value token)]
            (rx/behavior-subject value)
            (rx/subject)))

        hint*
        (mf/use-state {})

        hint
        (deref hint*)

        on-change
        (mf/use-fn
         (mf/deps resolve-stream input-name)
         (fn [event]
           (let [value (-> event dom/get-target dom/get-input-value)]
             (fm/on-input-change form input-name value true)
             (rx/push! resolve-stream value))))

        props
        (mf/spread-props  props  {:on-change on-change
                                  :default-value value
                                  :hint-message (:message hint)
                                  :hint-type (:type hint)})
        props
        (if (and error touched?)
          (mf/spread-props props {:hint-type "error"
                                  :hint-message (:message error)})
          props)]

    (mf/with-effect [resolve-stream tokens token input-name touched?]
      (let [subs (->> resolve-stream
                      (rx/debounce 300)
                      (rx/mapcat (partial resolve-value tokens token))
                      (rx/map (fn [result]
                                (d/update-when result :error
                                               (fn [error]
                                                 ((:error/fn error) (:error/value error))))))
                      (rx/subs! (fn [{:keys [error value]}]
                                  (when touched?
                                    (if error
                                      (do
                                        (swap! form assoc-in [:errors input-name] {:message error})
                                        (swap! form assoc-in [:errors resolved-input-name] {:message error})
                                        (swap! form update :data dissoc resolved-input-name)
                                        (reset! hint* {:message error :type "error"}))
                                      (let [message (tr "workspace.tokens.resolved-value" value)]
                                        (swap! form update :errors dissoc input-name resolved-input-name)
                                        (swap! form update :data assoc resolved-input-name value)
                                        (reset! hint* {:message message :type "hint"})))))))]

        (fn []
          (rx/dispose! subs))))

    [:> input* props]))

(defn on-input-change
  ([form field value]
   (on-input-change form field value false))
  ([form field value trim?]
   (swap! form (fn [state]
                 (-> state
                     (assoc-in [:touched :value field] true)
                     (assoc-in [:data :value field] (if trim? (str/trim value) value))
                     #_(update-in [:errors :value] dissoc field))))))


(mf/defc token-composite-value-input*
  [{:keys [name tokens token] :rest props}]

  (let [form       (mf/use-ctx fc/context)
        input-name name

        touched? false
        ;; ;; FIXME
        ;; (and (contains? (get-in @form [:data :value])
        ;;                 input-name)
        ;;      (get-in @form [:touched :value input-name]))

        error
        (get-in @form [:errors :value input-name])

        value
        (get-in @form [:data :value input-name] "")

        ;; TODO: MOdificar este stream para que si llega un token de
        ;; composite mire si el name es diferente a value
        resolve-stream
        (mf/with-memo [token]
          (if-let [value (get-in token [:value input-name])]
            (rx/behavior-subject value)
            (rx/subject)))

        hint*
        (mf/use-state {})

        hint
        (deref hint*)

        on-change
        (mf/use-fn
         (mf/deps resolve-stream input-name)
         (fn [event]
           (let [value (-> event dom/get-target dom/get-input-value)]
             (on-input-change form input-name value true)
             (rx/push! resolve-stream value))))

        props
        (mf/spread-props props {:on-change on-change
                                :default-value value
                                :hint-message (:message hint)
                                :hint-type (:type hint)})
        props
        (if (and error)
          (mf/spread-props props {:hint-type "error"
                                  :hint-message (:message error)})
          props)]

    (mf/with-effect [resolve-stream tokens token input-name touched?]
      (let [subs (->> resolve-stream
                      (rx/debounce 300)
                      (rx/mapcat (partial resolve-value tokens token))
                      (rx/map (fn [result]
                                (d/update-when result :error
                                               (fn [error]
                                                 (assoc error :message ((:error/fn error) (:error/value error)))))))

                      (rx/subs!
                       (fn [{:keys [error value]}]
                         (cond
                           (and error (str/empty? (:error/value error)))
                           (do
                             (prn "AAAA" error)
                             (swap! form update-in [:errors :value] dissoc input-name)
                             (swap! form update-in [:data :resolved-value] dissoc input-name)
                             (reset! hint* {}))


                           (some? error)
                           (let [error' (:message error)]
                             (prn "EEEE" error)
                             (swap! form assoc-in  [:errors :value input-name] {:message error'})
                             (swap! form assoc-in  [:errors :resolved-value input-name] {:message error'})
                             ;; (swap! form update-in [:data :resolved-value] dissoc input-name)
                             (reset! hint* {:message error' :type "error"}))

                           :else
                           (let [message (tr "workspace.tokens.resolved-value" value)]
                             (swap! form update :errors dissoc :value)
                             (swap! form update-in [:data :resolved-value] assoc input-name value)
                             (reset! hint* {:message message :type "hint"}))))))]
        (fn []
          (rx/dispose! subs))))

    [:> input* props]))
