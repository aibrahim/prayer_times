#!/usr/bin/env bb

(ns prayer-times
  (:require [babashka.http-client :as http]
            [babashka.json :as json]
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]
            [clojure.string :as str]
            [clojure.java.shell :refer [sh]]))

(defn get-current-hour-mins
  "Return current hour and mins as a tuple."
  []
  (let [now (java.time.LocalTime/now)]
    [(.getHour now)
     (.getMinute now)]))

(defn parse-hour-mins
  "Take `h-mm-str` as hour-min string with PM and AM at the end, and
  Return tuple of hour and mins.
  Ex: 
  \"01:30 PM\""
  [h-mm-str]
  (let [formatter (java.time.format.DateTimeFormatter/ofPattern "h:mm a")
        parsed-time (java.time.LocalTime/parse h-mm-str formatter)]
    [(.getHour parsed-time)
     (.getMinute parsed-time)]))

(defn get-prayers-times
  [town year month day]
  (let [now (java.time.ZonedDateTime/now)
        year (.getYear now)
        month (.getMonthValue now)
        day (.getDayOfMonth now)
        url (format "https://as-m247-prod.azurewebsites.net/api/timetable/getbytown/%s/%s/%s/%s?includeFullDetails=true" town year month day)]
    (-> (http/get url)
      :body
      json/read-str
      first
      (select-keys [:begFajr :jamFajr
                    :begDhuhr :jamDhuhr
                    :begAsar :jamAsar
                    :begMaghrib :jamMaghrib
                    :begEsha :jamEsha]))))

(defn get-current-prayers-times
  "Return current prayers times."
  [town]
  (let [now (java.time.ZonedDateTime/now)
        year (.getYear now)
        month (.getMonthValue now)
        day (.getDayOfMonth now)]
    (get-prayers-times town year month day)))

(defn get-tomorrow-prayers-times
  [town]
  (let [tomorrow (.plusDays (java.time.ZonedDateTime/now) 1)
        year (.getYear tomorrow)
        month (.getMonthValue tomorrow)
        day (.getDayOfMonth tomorrow)]
    (get-prayers-times town year month day)))

(defn parse-all-prayers-times
  [prayers-times]
  (reduce-kv
    (fn [res k v]
      ;; only Fajr is at AM
      (let [meridian (if (#{:begFajr :jamFajr} k) "AM" "PM")]
        (assoc res k (parse-hour-mins (str v " " meridian)))))
    {} prayers-times))

(defn time-diff
  "Calculate the time difference in hours and minutes between the current time and a target time."
  [[target-hour target-min] [current-hour current-min]]
  [(- target-hour current-hour) (- target-min current-min)])

(defn get-next-prayer-same-day
  "Return the next prayer time at the same day by comparing current time with parsed prayer times."
  [parsed-prayers]
  (let [current-time (get-current-hour-mins)]
    (->> parsed-prayers
      (map (fn [[prayer-time parsed-time]]
             [prayer-time (time-diff parsed-time current-time)]))
      (filter (fn [[_ [hour diff]]]
                (or (> hour 0) (and (= hour 0) (> diff 0)))))
      (sort-by (comp first second)) ;; Sort by the nearest positive time difference
      first)))

(defn get-next-prayer
  [town]
  (let [current-prayers (get-current-prayers-times town)
        [next-prayer remaining-time] (-> current-prayers
                                       (parse-all-prayers-times)
                                       (get-next-prayer-same-day))]
    (if next-prayer
      (let [[beg-or-jam prayer] (-> next-prayer
                                  csk/->kebab-case-string
                                  (str/split #"-"))]
        {:prayer-name    prayer
         :beg-or-jam     beg-or-jam
         :prayer-time    (get current-prayers next-prayer)
         :remaining-time remaining-time})
      (let [tomorrow-prayers (-> (get-tomorrow-prayers-times town)
                               (select-keys [:begFajr]))]
        {:prayer-name    :fajr
         :beg-or-jam     :beg
         :prayer-time    (get tomorrow-prayers :begFajr)}))))

(defn notify-message
  [title body]
  (let [message (format "display notification \"%s\" with title \"%s\""
                  body title)]
    (sh "osascript" "-e" message)))

(defn notify-next-prayer
  [town]
  (let [{:keys [prayer-name prayer-time]} (get-next-prayer "cambridge")]
    (notify-message "🕌 Next Prayer" (str prayer-name " at " prayer-time))))


(comment


  (notify-next-prayer "cambridge")


  
  
  ,)
