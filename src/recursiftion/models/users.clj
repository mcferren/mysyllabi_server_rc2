(ns recursiftion.models.users
  (:require [buddy.hashers :as hashers]
            [clojure.set :refer [map-invert]]
            [recursiftion.dao_dictionary :as dao]
            [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.conversion :refer [from-db-object]]))

; (def mongo-uri "mongodb://mcferren:Broadway$19@candidate.36.mongolayer.com:11032,candidate.33.mongolayer.com:11038/app40992485")
(def mongo-uri "mongodb://mcferren:Broadway$19@candidate.55.mongolayer.com:10588,candidate.54.mongolayer.com:10796/mysyllabi-data-store?replicaSet=set-56526ee0a5b023028800061a")


(def mongoconnection (let [{:keys [conn db]} (mg/connect-via-uri mongo-uri)] db))

(def user-levels
  {"user" ::user
   "admin" ::admin})
(derive ::admin ::user)


(defn find-user-by-id [userid]
  (let [_userid userid
         qcoll "users"]
(println "RATATATAT" _userid)
          
        (dissoc (mc/find-one-as-map mongoconnection qcoll {:_id _userid}) :password)
    )
)

(defn create-new-instance [instanceobj]
  (let [_instanceobj instanceobj
        tcoll "tabs"
        ncoll "nodes"]


        (recursiftion.dao_dictionary/create-node-tab-batch {
          :batchobj {
              :nodes (get-in _instanceobj [:nodes])
              :tabs (get-in _instanceobj [:tabs])
          }
        })

  ))


(defn insert-user [userobject]
  (let [_uobject userobject
        ucoll "users"]

  		(if (mc/any? mongoconnection ucoll {:_id (get-in _uobject [:userid])})
            (mc/find-one-as-map mongoconnection ucoll {:_id (get-in _uobject [:userid])})
            (mc/insert-and-return mongoconnection ucoll {
            								:_id 				               (get-in _uobject [:userid])
                            :email 					           (get-in _uobject [:email])
                            :password (hashers/encrypt (get-in _uobject [:password]))
                            :level 					           (get-in _uobject [:level])
            }))
    )
)


(defn get-user [userid]
  (let [_userid userid
        ucoll "users"]

  		(mc/find-one-as-map mongoconnection ucoll {:_id _userid})
    )
)

(defn password-matches?
  "Check to see if the password given matches the digest of the user's saved password"
  [userid password]

  (hashers/check password (get-in (get-user userid) [:password]))
)


