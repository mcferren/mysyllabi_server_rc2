(ns recursiftion.models.users
  (:require [buddy.hashers :as hashers]
            [clojure.set :refer [map-invert]]
            [clojurewerkz.neocons.rest        :as nr]
            [clojurewerkz.neocons.rest.cypher :as cy]))

(def conn (nr/connect "http://app40992485:F6bNfeA6f35v0RijWJDE@app40992485.sb02.stations.graphenedb.com:24789/db/data/"))

(def user-levels
  {"user" ::user
   "admin" ::admin})
(derive ::admin ::user)

(def find-user-by-id-query "MATCH (u:User)
							WHERE u.id = {_userid}
                        	RETURN u as userobject;")

(defn find-user-by-id [userid]
  (let [_userid userid]
        
        (dissoc
        	(get-in 
        		(first (cy/tquery conn find-user-by-id-query {:_userid _userid } )
        		) ["userobject" :data] )
        	:password)
    )
)


(def insert-user-query "CREATE (u:User {
	                           email   			: {_email},
	                           id 			    : {_userid},
	                           level   			: {_level},
	                           password         : {_password}
				        })
                        RETURN u as userobject;")


(defn insert-user [userpayload]
  (let [_userpayload userpayload]
        
        (dissoc
        	(get-in 
        		(first (cy/tquery conn insert-user-query {:_userid                    (get-in _userpayload [:userid])
        											      :_email                     (get-in _userpayload [:email])
  													      :_level	                  (get-in _userpayload [:level])
        											      :_password (hashers/encrypt (get-in _userpayload [:password]))}
        											     )) ["userobject" :data] )
        	:password)
    )
)

(def user-query "MATCH (a:User)
                 WHERE a.id = {_userid}
                 RETURN a AS userobject;")

(defn get-user-password [userid]
  (let [_userid userid]
        (get-in (first (cy/tquery conn user-query {:_userid _userid})) ["userobject" :data] )
    )
)

(defn password-matches?
  "Check to see if the password given matches the digest of the user's saved password"
  [userid password]

  (hashers/check password (get-in (get-user-password userid) [:password]))
)


