(ns simplecalculator.core
   (:require [reagent.core :as r :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]

    ))

(enable-console-print!)

;;pension age calculator
(defonce pension-age (r/atom {:dob nil :pensiondate 0 :age 0}))

(def today (js/Date.))

(defn date? [x]
  (= (type x) js/Date))


(defn get-date! [date]
    (if (date? date)
       (.toLocaleDateString date "en" "%d-%b-%Y")
      "unselected"))

(defn calc-pensionage []
  (let [{:keys [dob pensiondate age]} @pension-age ]
    (if(and (<(js/Date. "1950-04-06") dob) (> (js/Date. "1953-04-05") dob))
      ;;age to get state pension is 63
      ;;add 63 to dob
      ((swap! pension-age assoc :age 63)
      (swap! pension-age assoc :pensiondate  (js/Date. (+ (.getFullYear dob) 63) (.getMonth dob) (.getDate dob)))))
    (if(and (<(js/Date. "1953-04-06") dob ) (>(js/Date. "1953-12-05") dob))
      ;;age to get state pension is 65
      ;;add 65 to dob
      ((swap! pension-age assoc :age 65)
      (swap! pension-age assoc :pensiondate  (js/Date. (+ (.getFullYear dob) 65) (.getMonth dob) (.getDate dob)))))
    (if(and (<(js/Date. "1953-12-06") dob ) (>(js/Date. "1960-04-05") dob))
      ;;age to get state pension is 66
      ;;add 66 to dob
      ((swap! pension-age assoc :age 66)
      (swap! pension-age assoc :pensiondate  (js/Date. (+ (.getFullYear dob) 66) (.getMonth dob) (.getDate dob))))
      )
    (if(and (<(js/Date. "1960-04-06") dob ) (>(js/Date. "1977-04-05") dob))
      ;;age to get state pension is 67
      ;;add 67 to dob
      ((swap! pension-age assoc :age 67)
      (swap! pension-age assoc :pensiondate  (js/Date. (+ (.getFullYear dob) 67) (.getMonth dob) (.getDate dob))))
      )
    (if(<(js/Date. "1977-04-06") dob )
      ;;age to get state pension is 68
      ;;add 68 to dob
      ((swap! pension-age assoc :age 68)
      (swap! pension-age assoc :pensiondate  (js/Date. (+ (.getFullYear dob) 68) (.getMonth dob) (.getDate dob))))
      )
   )
)


;;calculator component
(defonce result-data (r/atom {:number 0 :result nil :operator nil}))

(defn calc-result []
  (let [{:keys [number result operator]} @result-data]
    (if (= operator "+") (swap! result-data assoc :result (+ (* 1 result) (* 1 number))))
    (if (= operator "-") (swap! result-data assoc :result (- result number )))
    (if (= operator "*") (swap! result-data assoc :result (* result number )))
    (if (= operator "/") (swap! result-data assoc :result (/ result number )))
    (swap! result-data assoc :number 0)
    ))

;;BMI Calculator components
(defonce bmi-data (r/atom {:height 180 :weight 80}))

(defn calc-bmi []
  (let [{:keys [height weight bmi] :as data} @bmi-data
        h (/ height 100)]
    (if (nil? bmi)
      (assoc data :bmi (/ weight (* h h)))
      (assoc data :weight (* bmi h h)))))

(defn slider [param value min max]
  [:input {:type "range" :value value :min min :max max
           :style {:width "100%"}
           :on-change (fn [e]
                        (swap! bmi-data assoc param (.-target.value e))
                        (when (not= param :bmi)
                          (swap! bmi-data assoc :bmi nil)))}])

;; -------------------------
;; Views

;;navigation view
;;navigation-view will be shared by three main components
(defn navigation-view []
  [:div
  [:a {:href "/"} "Maths "]
  [:a {:href "/BMI"} " BMI"]
  [:a {:href "/Pensionage"} " PensionAge"]
]
)


(defn home-page []
  (let [{:keys [number result operator]} @result-data]
  [:div
   [navigation-view]
   [:h4 "Maths Calculator"]
   [:input {:type "number"
               :value  number
               :on-change (fn [e]
                            (swap! result-data assoc :number (.-target.value e))
                           )
                        }]
   [:div
      [:input {:type "button"
               :value  "+"
               :on-click (fn [e]
                        (swap! result-data assoc :operator (.-target.value e))
                        (if(nil? result)
                           ((swap! result-data assoc :result number)
                            (swap! result-data assoc :number 0)))
                    )
            }]
      [:input {:type "button"
               :value  "-"
               :on-click (fn [e]
                        (swap! result-data assoc :operator (.-target.value e))
                       (if(nil? result)
                           ((swap! result-data assoc :result number)
                            (swap! result-data assoc :number 0)))
                    )
            }]
      [:input {:type "button"
               :value  "/"
               :on-click (fn [e]
                        (swap! result-data assoc :operator (.-target.value e))
                        (if(nil? result)
                           ((swap! result-data assoc :result number)
                            (swap! result-data assoc :number 0)))
                    )
            }]
    [:input {:type "button"
               :value  "*"
               :on-click (fn [e]
                        (swap! result-data assoc :operator (.-target.value e))
                        (if(nil? result)
                           ((swap! result-data assoc :result number)
                            (swap! result-data assoc :number 0)))
                    )
            }]
     [:input {:type "button"
               :value  "="
               :on-click calc-result
            }]
    ]
    [:p result]

   ]))

(defn bmi-page []
  (let [{:keys [weight height bmi]} (calc-bmi)
        [color diagnose] (cond
                          (< bmi 18.5) ["orange" "underweight"]
                          (< bmi 25) ["inherit" "normal"]
                          (< bmi 30) ["orange" "overweight"]
                          :else ["red" "obese"])]
    [:div
     [navigation-view]
     [:h4 "BMI calculator"]
     [:div
      "Height: " (int height) "cm"
      [slider :height height 100 220]]
     [:div
      "Weight: " (int weight) "kg"
      [slider :weight weight 30 150]]
     [:div
      "BMI: " (int bmi) " "
      [:span {:style {:color color}} diagnose]
      [slider :bmi bmi 10 50]]]))

(defn pension-page []
  (let [{:keys [dob pensiondate age]} @pension-age]
  [:div
  [navigation-view]
   [:div [:h4 "State Pension Age Calculator"]
    [:p "Enter Date Of Birth"]
    [:input {:type "date"
             :on-change (fn [e] (swap! pension-age assoc :dob (js/Date. (.-target.value e))))
             }]
     [:input {:type "button"
               :value  "Pension Age"
               :on-click calc-pensionage
            }]
    [:p  "You will reach pension age at " age " as from " (get-date! pensiondate) ]
   ]
   ]
  ))

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

(secretary/defroute "/BMI" []
  (session/put! :current-page #'bmi-page))

(secretary/defroute "/Pensionage" []
  (session/put! :current-page #'pension-page))
;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [current-page] (.getElementById js/document "app")))


  (accountant/configure-navigation!)
  (accountant/dispatch-current!)
  (mount-root)
