#!/usr/bin/env bb

(ns prayer-times
  (:require [babashka.http-client :as http]
            [babashka.json :as json]))

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

(defn get-diffs
  [parsed-prayers-times]
  (let [[current-hour current-mins] (get-current-hour-mins)]
    (->> parsed-prayers-times
      (map (fn [[prayer [hour mins]]]
             [prayer [(- hour current-hour)
                      (- mins current-mins)]])))))

(defn get-next-prayer
  [diffs]
  (let [prayer (->> diffs
                 (filter (comp (partial < 0) first last))
                 (sort-by (comp first last))
                 first)]
    (if (nil? prayer)
      (-> (get-tomorrow-prayers-times "cambridge")
        (parse-all-prayers-times)
        (select-keys [:begFajr])))))



(comment

  (-> (get-current-prayers-times "cambridge")
    (parse-all-prayers-times)
    (get-diffs)
    (get-next-prayer)
    )
  
  ,)
