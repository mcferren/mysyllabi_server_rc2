;; the top section includes all of the libraries
;; injected so that we can use their namespace

(ns recursiftion.dao_dictionary
  (:use joda-time)
  (:refer-clojure :exclude [sort find])
  (:require [clojure.string]
            [clojure.data]
            [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.conversion :refer [from-db-object]]
            [monger.query :refer :all]
            [clojure.pprint :refer [pprint]]
            [joda-time.accessors :as ja]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [clj-time.local :as l])
  (:import [com.mongodb MongoOptions ServerAddress]
           [org.bson.types.ObjectId])
  )



;; connect using connection URI stored in an env variable, in this case, MONGOHQ_URL
; (let [uri               (System/genenv "MONGOHQ_URL")
; (def mongo-uri "mongodb://mcferren:Broadway$19@linus.mongohq.com:10028/app30824309")
; (def mongo-uri "mongodb://mcferren:Broadway$19@candidate.36.mongolayer.com:11032,candidate.33.mongolayer.com:11038/app40992485")
(def mongo-uri "mongodb://mcferren:Broadway$19@candidate.55.mongolayer.com:10588,candidate.54.mongolayer.com:10796/mysyllabi-data-store?replicaSet=set-56526ee0a5b023028800061a")


(def mongoconnection (let [{:keys [conn db]} (mg/connect-via-uri mongo-uri)] db))

; (let [uri               "mongodb://mcferren:Broadway$19@candidate.55.mongolayer.com:10588,candidate.54.mongolayer.com:10796/mysyllabi-data-store?replicaSet=set-56526ee0a5b023028800061a"
;       {:keys [conn db]} (mg/connect-via-uri uri)])

; (def mongo-uri "mongodb://mcferren:Broadway$19@linus.mongohq.com:10028/app30824309?maxPoolSize=128&waitQueueMultiple=5;waitQueueTimeoutMS=150;socketTimeoutMS=5500&autoConnectRetry=true;safe=false&w=1;wtimeout=2500;fsync=true")


(defn get-node [nodeid]
  (let [_nodeid nodeid
        ncoll "nodes"]

    (mc/find-maps mongoconnection ncoll {:_id _nodeid})  
))



(defn get-node-batch [nodeidarray]
  (let [_nodeidarray nodeidarray
        ncoll "nodes"]
        (println "()()()()))()()()()()()" _nodeidarray)

      (mc/find-maps mongoconnection ncoll {:_id { "$in" _nodeidarray } } )
))

(defn String->Number [stringarg]
  (let [n stringarg]

       (println "****** string->number")
       (println "stringarg" stringarg)
       (println "n" n)

        (cond
            (nil? n) nil
            (number? (read-string n)) n
            :else nil)
))


; (defn fetchObjFromArray [fetch_id inputarray]

;     (reduce (fn [reduced_obj element_obj]

;                 (if (= fetch_id (get-in element_obj [:id]))
;                     (assoc reduced_obj (keyword fetch_id) element_obj) ;; or (merge-with concat reduced_obj {(keyword fetch_id [element_obj])})
;                     reduced_obj
;                 )    
;             )
;             {}
;             inputarray
;     )
; )



(defn createReportFromObjArray [inputarray]

    (reduce (fn [returnobj elementobj]

                (let [_id (get-in elementobj [:_id])
                      keyword (keyword _id)]

                    (assoc returnobj keyword elementobj)
                )
            )
            {}
            inputarray)
)

(defn ALTcreateReportFromObjArray [inputarray]

    (reduce (fn [returnobj elementobj]

                (let [_id (get-in elementobj [:tailparent])
                      keyword (keyword _id)]

                    (merge-with concat returnobj {keyword [elementobj]})
                )
            )
            {}
            inputarray)
)

; (defn process-node-components [nodeid depth]
;   (let [_instanceid instanceid
;         uri mongo-uri
;             {:keys [conn db]} (mg/connect-via-uri uri)
;         ncoll "nodes"
;         tcoll "tabs"
;         _tabs (mc/find-maps mongoconnection tcoll {:_id { "$in" _roottabidsarray } } )
;         _nodes (mc/find-maps mongoconnection ncoll {:_id { "$in" _roottabidsarray } } )]

;           (if (> _depth 1)
;               (process-node-components )
;               {
;                 :nodes 
;                 :tabs
;               }
;           )
; ))

(defn get-refined-nodeids-and-tabids [tabid]
  (let [_tabid tabid
         tcoll "tabs"
         ncoll "nodes"
         _inventory (get-in (first (mc/find-maps mongoconnection tcoll {:_id _tabid } )) [:inventory])
        ; _targettab (str nodeid "_ALLRESOURCES")
        ; _tabarray (first (mc/find-maps mongoconnection tcoll {:_id _tabid} ))
        ; _targetnodes (get-in _tabarray [:inventory])
        _targetpointers (filter (fn [element] (not (= "all" (get-in element [:currenttab]))))
                                _inventory)
        _pointeroperation (set (map (fn [element] 
                                                {
                                                    :refine (get-in element [:currenttab])
                                                    :tabid (str (get-in element [:_id]) "_ALLRESOURCES")
                                                }
                                             ) 
                                             _targetpointers))
        _refinetabs (group-by :tabid _pointeroperation)
        ]
        ; (println "BARKER BIRDDDDY" (take 2 _targetnodes))
        ; (println "GOODS" (map (fn [element] (with-collection mongoconnection tcoll (find {:_id { "$in" _roottabidsarray } }) (paginate :page 1 :per-page 2))) 

(println "$$$ _pointeroperation" _pointeroperation)

        _refinetabs

))



(defn leaf? [pointerobj]

    (let [leaftabtypearray [ "vacant" "stub" "edit" "tags" "menu" "coin" "headline" "button" "destination" "modal" "calendar" "basic" ] ]

        (if (= (.indexOf leaftabtypearray (get-in pointerobj [:currenttab])) -1) true false)
    )
)

(defn decipherTabidFromPointer [ pointerobj ]

    ; (let [leaftabtypearray [ "vacant" "stub" "edit" "tags" "menu" "coin" "headline" "button" "destination" "modal" "calendar" "basic" ] ]
          
        (if (= (get-in pointerobj [:currenttab]) "day")
            (str (get-in pointerobj [:_id]) "_" (get-in pointerobj [:currentdate]) "_" (get-in pointerobj [:instance]) )
            ; (if (leaf? pointerobj)
                (str (get-in pointerobj [:_id]) "_ALLRESOURCES_" (get-in pointerobj [:instance]) )
                ; (get-in pointerobj [:currenttab])
                ; "mook"
            ; )
        )
    ; ) 
)



(defn interpretTabObjArray [tab_obj_array] 

    (reduce (fn [outside_reducedobj tab_obj_element]

                (let [parent_tab_id         (get-in tab_obj_element [:tabobj :_id])
                      parent_tab_inventory  (get-in tab_obj_element [:tabobj :inventory])
                      parent_node_id        (get-in tab_obj_element [:parent_node])]

                    (reduce (fn [inside_reducedobj pointerobj_element]

                                (let [inner_iterator               (get-in inside_reducedobj           [:iterator])
                                      nodeid                       (get-in pointerobj_element          [:_id])
                                      nodekey                      (keyword                            (last (clojure.string/split nodeid #"_")))
                                      tabid                        (decipherTabidFromPointer           pointerobj_element)
                                      old_node_id_array            (get-in inside_reducedobj           [:node_id_array])
                                      old_tab_id_array             (get-in inside_reducedobj           [:tab_id_array])
                                      old_bubbble_obj              (get-in inside_reducedobj           [:bubble_obj])
                                      old_tab_listener_datastore   (get-in inside_reducedobj           [:tab_listener_datastore])
                                      old_cmpref_datastore         (get-in inside_reducedobj           [:cmpref_datastore])
                                      listener?                    (true? (get-in pointerobj_element   [:urlnodelistener]))
                                      bubblepath                   (into [{ 
                                                                            :ancestor_tabid     parent_tab_id 
                                                                            :pointer_index      inner_iterator 
                                                                            :ancestor_nodeid    parent_node_id
                                                                         }] 
                                                                         (get-in tab_obj_element [:bubblepath]))
                                      element_info_packet          {
                                                                        :target_node_id  nodeid
                                                                        :target_tab_id   tabid
                                                                        :bubblepath      bubblepath
                                                                   }]

                                    {
                                        :iterator                   (inc inner_iterator)
                                        :node_id_array              (conj old_node_id_array nodeid)
                                        :tab_id_array               (if (leaf? pointerobj_element) (conj  old_tab_id_array  tabid) old_tab_id_array)
                                        :bubble_obj                 (conj  old_bubbble_obj element_info_packet)
                                        :tab_listener_datastore     (if (and listener? leaf?)
                                                                        (assoc old_tab_listener_datastore nodekey [element_info_packet]) ;; !! TEMP FIX - - - - LISTENERS SHOULD REFERENCE CMP_REFs INSTEAD
                                                                        ; (assoc old_tab_listener_datastore nodekey (conj (get-in old_tab_listener_datastore [nodekey]) element_info_packet)) ;; LISTENERS SHOULD REFERENCE CMP_REFs INSTEAD
                                                                        old_tab_listener_datastore
                                                                    )
                                        :cmpref_datastore           (if (leaf? pointerobj_element)
                                                                        (merge-with concat old_cmpref_datastore {(keyword nodeid) [ element_info_packet ]})
                                                                        old_cmpref_datastore
                                                                    )
                                    }  
                                )        
                            )
                            (assoc outside_reducedobj :iterator 0)
                            parent_tab_inventory
                    )       
                )
            )
            {}
            tab_obj_array
    )
)



(defn IMPOSTERTraverseTree [tab_obj_array traversedepth]

    (let [traversal_obj                         (interpretTabObjArray tab_obj_array)

          application_node_id_array             (into [] (get-in traversal_obj [:node_id_array]))           ;; convert PersistentList to array
          application_tab_id_array              (into [] (get-in traversal_obj [:tab_id_array]))            ;; convert PersistentList to array
          application_tab_bubble_id_array       (into [] (get-in traversal_obj [:bubble_obj]))              ;; convert PersistentList to array
          application_tab_listener_datastore    (into {} (get-in traversal_obj [:tab_listener_datastore]))  
          application_cmpref_datastore          (into {} (get-in traversal_obj [:cmpref_datastore])) 

          application_node_obj_array            (mc/find-maps mongoconnection "nodes" {:_id { "$in" application_node_id_array }})
          application_tab_obj_array             (mc/find-maps mongoconnection "tabs"  {:_id { "$in" application_tab_id_array }})

          application_tab_datastore             (createReportFromObjArray application_tab_obj_array)
          application_tab_bubble_obj_array      (map (fn [element] 
                                                          {
                                                              :tabobj         (get-in application_tab_datastore [(keyword (get-in element [:target_tab_id]))])
                                                              :bubblepath     (get-in element [:bubblepath])
                                                              :parent_node    (get-in element [:target_node_id])
                                                          }
                                                     ) 
                                                     application_tab_bubble_id_array
                                                )]


        (if (< 0 traversedepth)

            (let [returnobj         (IMPOSTERTraverseTree application_tab_bubble_obj_array (- traversedepth 1))]

                {
                    :nodeids        (distinct (flatten (conj application_node_id_array      (get-in returnobj [:nodeids]))))
                    :nodeobjs       (distinct (flatten (conj application_node_obj_array     (get-in returnobj [:nodeobjs]))))
                    :tabids         (distinct (flatten (conj application_tab_id_array       (get-in returnobj [:tabids]))))
                    :tabobjs        (distinct (flatten (conj application_tab_obj_array      (get-in returnobj [:tabobjs]))))
                    :tablisteners   (conj application_tab_listener_datastore (get-in returnobj [:tablisteners]))
                    :cmprefs        (merge-with concat application_cmpref_datastore (get-in returnobj [:cmprefs]) )
                }
            )
            {
                :nodeids        application_node_id_array
                :nodeobjs       application_node_obj_array
                :tabids         application_tab_id_array    
                :tabobjs        application_tab_obj_array
                :tablisteners   application_tab_listener_datastore
                :cmprefs        application_cmpref_datastore
            }
        )
    )
)



(defn assembleTailString [ pointerobj ]

    (let [leaftabtypearray [ "vacant" "stub" "edit" "tags" "menu" "coin" "headline" "button" "destination" "modal" "calendar" "basic" ] ]
          
        (if (= (get-in pointerobj [:currenttab]) "day")
            (str (get-in pointerobj [:nodeid]) "_" (get-in pointerobj [:currentdate]) "_" (get-in pointerobj [:instance]))
            (if (= (.indexOf leaftabtypearray (get-in pointerobj [:currenttab])) -1)
                (str (get-in pointerobj [:nodeid]) "_ALLRESOURCES_" (get-in pointerobj [:instance]))
                ; (get-in pointerobj [:currenttab])
                ; (get-in pointerobj [:instance])
            )
        )
    ) 
)


(defn updateOrderAmount [new_listener_datastore orderamount tailparent_key]

    (let [old_element_info_packet (get-in new_listener_datastore [tailparent_key])
          old_order               (get-in old_element_info_packet [:order])
          new_order               (if (or (nil? old_order) (< orderamount old_order)) orderamount old_order)
          new_element_info_packet (assoc old_element_info_packet :order new_order)]

        
          ; (do (println "3mooseeggs" new_element_info_packet)
          ;     (println "4mouseeggs" (keys new_listener_datastore))
              (assoc new_listener_datastore tailparent_key new_element_info_packet)
    )
)


(defn SECRETinterpretTabObjArray [pointer_obj_array master_datastore] 

    (reduce (fn [reduced_obj pointerobj_element]

         (let [nodeid                       (get-in pointerobj_element          [:nodeid])
               nodekey                      (keyword                            (last (clojure.string/split nodeid #"_")))
               tailparent                   (assembleTailString                 pointerobj_element)
               ALTtailkey                   (keyword                            tailparent)
               grandparent                  (get-in pointerobj_element          [:tailparent])
               grandparent_order            (get-in pointerobj_element          [:order])
               old_node_id_array            (get-in reduced_obj                 [:node_id_array])
               old_tailparent_array         (get-in reduced_obj                 [:tail_array])
               old_listener_datastore       (get-in reduced_obj                 [:listener_datastore])
               old_ALTlistener_datastore    (get-in reduced_obj                 [:ALTlistenerStore])
               listener?                    (true? (get-in pointerobj_element   [:urlnodelistener]))
               element_info_packet          {
                                                 :target_node_id                nodeid
                                                 :target_tail                   tailparent
                                                 :target_grandparent            grandparent
                                                 :target_grandparent_order      grandparent_order
                                            }
               new_listener_datastore       (if listener? (assoc old_listener_datastore 
                                                                 nodekey (conj (get-in old_listener_datastore [nodekey]) element_info_packet))
                                                          old_listener_datastore
                                            )
               new_ALTlistenerStore         (if listener? (assoc old_ALTlistener_datastore ALTtailkey element_info_packet)
                                                          old_ALTlistener_datastore
                                            )
               return_listener_datastore    (if-not (nil? (get-in old_ALTlistener_datastore [(keyword (get-in pointerobj_element [:tailparent]))]))
                                                    
                                                    ; new_ALTlistenerStore
                                                    ; (do (println "1mooseeggs" (get-in pointerobj_element [:tailparent]))
                                                        ; (println "2mouseeggs" (keys old_ALTlistener_datastore))
                                                        (updateOrderAmount new_ALTlistenerStore 
                                                                       (get-in pointerobj_element [:order])
                                                                       (keyword (get-in pointerobj_element [:tailparent]))
                                                    )
                                                    new_ALTlistenerStore
                                            )]

            {
                :node_id_array              (conj  old_node_id_array      nodeid)
                :tail_array                 (conj  old_tailparent_array   tailparent)
                :listener_datastore         new_listener_datastore
                :ALTlistenerStore           return_listener_datastore
                ;; if exists key for tail of pointerobj then check if its the lowest - we're trying to get first index
            }  
         )        
     )
     { 
        :ALTlistenerStore (get-in master_datastore [:ALTlistenerStore]) 
        :listener_datastore (get-in master_datastore [:listener_datastore]) 
     }
     pointer_obj_array)
)


(defn SECRETTraverseTree [pointer_obj_array traversedepth master_datastore]

    (let [traversal_obj                         (SECRETinterpretTabObjArray pointer_obj_array master_datastore)

          application_node_id_array             (into [] (get-in traversal_obj [:node_id_array]))           ;; convert PersistentList to array
          application_tail_array                (into [] (get-in traversal_obj [:tail_array]))              ;; convert PersistentList to array
          application_listener_datastore        (into {} (get-in traversal_obj [:listener_datastore]))
          application_ALTlistener_datastore     (into {} (get-in traversal_obj [:ALTlistenerStore]))

          application_node_obj_array            (mc/find-maps mongoconnection "nodes" {:_id { "$in" application_node_id_array }})
          application_tail_obj_array            (mc/find-maps mongoconnection "pointers" {:tailparent { "$in" application_tail_array }})]
          ; tail_fetch_query_regex                (str "^" (clojure.string/join "|^" application_tail_array))
          ; application_tail_obj_array            (mc/find-maps mongoconnection "pointers" {:tailparent {$regex tail_fetch_query_regex }})]


; (with-collection mongoconnection ncoll
; (find {:_id { "$in" application_input_node_id_array }})
; ; (fields [:_id])
; ;; it is VERY IMPORTANT to use array maps with sort
; (sort (array-map :_id 1))
; ; (limit 10)

          
        (if (< 0 traversedepth)

            (let [returnobj         (SECRETTraverseTree application_tail_obj_array (- traversedepth 1) traversal_obj)]

                    returnobj
                {
                    :nodeids        (distinct (flatten (conj application_node_id_array      (get-in returnobj [:nodeids]))))
                    :nodeobjs       (distinct (flatten (conj application_node_obj_array     (get-in returnobj [:nodeobjs]))))
                    :pointerids     (distinct (flatten (conj application_tail_array         (get-in returnobj [:pointerids]))))
                    :pointerobjs    (distinct (flatten (conj application_tail_obj_array     (get-in returnobj [:pointerobjs]))))
                    :listeners      (conj application_listener_datastore (get-in returnobj [:listeners]))
                    :ALTlisteners   (conj application_ALTlistener_datastore (get-in returnobj [:ALTlisteners]))
                }
            )
            {
                :nodeids        application_node_id_array
                :nodeobjs       application_node_obj_array
                :pointerids     application_tail_array
                :pointerobjs    application_tail_obj_array
                :listeners      application_listener_datastore
                :ALTlisteners   application_ALTlistener_datastore
            }
        )
    )
)



(defn ALTTraverseTree [pointer_obj_array traversedepth]         ;; THIS NEEDS TO ROLL AN ARRAY OF TABOBJ'S INSTEAD OF AN ARRAY OF POINTER OBJS

    (let [traversal_obj  (reduce (fn [returnobj pointerobj_element]

                             (let [nodeid                       (get-in pointerobj_element          [:_id])
                                   nodekey                      (keyword                            (last (clojure.string/split nodeid #"_")))
                                   tabid                        (decipherTabidFromPointer           pointerobj_element)
                                   old_node_id_array            (get-in returnobj                   [:node_id_array])
                                   old_tab_id_array             (get-in returnobj                   [:tab_id_array])
                                   old_tab_listener_datastore   (get-in returnobj                   [:tab_listener_datastore])
                                   listener?                    (true? (get-in pointerobj_element   [:urlnodelistener]))]
 
                                {
                                    :node_id_array              (conj  old_node_id_array  nodeid)
                                    :tab_id_array               (conj  old_tab_id_array   tabid)
                                    :tab_listener_datastore     (if listener? (assoc old_tab_listener_datastore 
                                                                                     nodekey (conj (get-in old_tab_listener_datastore [nodekey]) tabid))  ;; object {...} instead ?
                                                                              old_tab_listener_datastore
                                                                )
                                }  
                             )        
                         )
                         {}
                         pointer_obj_array)

          application_node_id_array           (into [] (get-in traversal_obj [:node_id_array]))           ;; convert PersistentList to array
          application_tab_id_array            (into [] (get-in traversal_obj [:tab_id_array]))            ;; convert PersistentList to array
          application_tab_listener_datastore  (into {} (get-in traversal_obj [:tab_listener_datastore]))  

          application_node_obj_array          (mc/find-maps mongoconnection "nodes" {:_id { "$in" application_node_id_array }})
          application_tab_obj_array           (mc/find-maps mongoconnection "tabs"  {:_id { "$in" application_tab_id_array }})
          application_pointer_obj_array       (flatten (map (fn [element] (get-in element [:inventory]) ) application_tab_obj_array))] ;; DON"T FLATTEN


        (if (< 0 traversedepth)

            (let [returnobj         (ALTTraverseTree application_pointer_obj_array (- traversedepth 1))]

                {
                    :nodeids        (distinct (flatten (conj application_node_id_array      (get-in returnobj [:nodeids]))))
                    :nodeobjs       (distinct (flatten (conj application_node_obj_array     (get-in returnobj [:nodeobjs]))))
                    :tabids         (distinct (flatten (conj application_tab_id_array       (get-in returnobj [:tabids]))))
                    :tabobjs        (distinct (flatten (conj application_tab_obj_array      (get-in returnobj [:tabobjs]))))
                    :tablisteners   (conj application_tab_listener_datastore (get-in returnobj [:tablisteners]))
                }
            )
            {
                :nodeids        application_node_id_array
                :nodeobjs       application_node_obj_array
                :tabids         application_tab_id_array
                :tabobjs        application_tab_obj_array
                :tablisteners   application_tab_listener_datastore
            }
        )
    )
)



; application_tab_obj_array         (with-collection mongoconnection tcoll
;                                     (find {:_id { "$in" application_tab_id_array }})
;                                     ; (fields [:_id])
;                                     ;; it is VERY IMPORTANT to use array maps with sort
;                                     (sort (array-map :_id 1))
;                                     ; (limit 10)
;                                 )


(defn traversetree [pointer_obj_array traversedepth]

    (let [ncoll                             "nodes"
          tcoll                             "tabs"
          
          application_input_node_id_array   (distinct (flatten (map (fn [element] (get-in element [:_id]) ) pointer_obj_array)))
          application_node_obj_array        (with-collection mongoconnection ncoll
                                                (find {:_id { "$in" application_input_node_id_array }})
                                                ; (fields [:_id])
                                                ;; it is VERY IMPORTANT to use array maps with sort
                                                (sort (array-map :_id 1))
                                                ; (limit 10)
                                            )
          application_output_node_id_array  (map (fn [element] (get-in element [:_id]) ) application_node_obj_array)
          application_tab_id_array          (map (fn [element] (decipherTabidFromPointer element)
                                                    ; {
                                                    ;     :tabid (decipherTabidFromPointer element)
                                                    ;     :listener (true? (get-in pointerobj [:urlnodelistener]) )
                                                    ; }
                                                 ) 
                                                  pointer_obj_array
                                            )
          application_tab_obj_array         (with-collection mongoconnection tcoll
                                                (find {:_id { "$in" application_tab_id_array }})
                                                ; (fields [:_id])
                                                ;; it is VERY IMPORTANT to use array maps with sort
                                                (sort (array-map :_id 1))
                                                ; (limit 10)
                                            )
          application_pointer_array         (flatten (map (fn [element] (get-in element [:inventory]) ) application_tab_obj_array))]

          (if (< 0 traversedepth)

              (let [_returnobj (traversetree application_pointer_array (- traversedepth 1))]

                  {
                      :nodeids (distinct (flatten (conj application_output_node_id_array (get-in _returnobj [:nodeids]))))
                      :nodeobjs (distinct (flatten (conj application_node_obj_array (get-in _returnobj [:nodeobjs]))))
                      :tabids (distinct (flatten (conj application_tab_id_array (get-in _returnobj [:tabids]))))
                      :tabobjs (distinct (flatten (conj application_tab_obj_array (get-in _returnobj [:tabobjs]))))
                  }
              )
              {
                  :nodeids application_output_node_id_array
                  :nodeobjs application_node_obj_array
                  :tabids application_tab_id_array
                  :tabobjs application_tab_obj_array
              }
          )
    )
)







(def monthset [
  "January"
  "February"
  "March"
  "April"
  "May"
  "June"
  "July"
  "August"
  "September"
  "October"
  "November"
  "December"
])


(def dayset [
  "Monday"
  "Tuesday"
  "Wednesday"
  "Thursday"
  "Friday"
  "Saturday"
  "Sunday"
])


(defn dateToString[dateobj]
  (let [_dateobj dateobj
        _dayofmonth (value (property _dateobj :dayOfMonth))
        _dayofweek (get-in dayset [(- (value (property _dateobj :dayOfWeek)) 1)])
        _month (get-in monthset [(- (value (property _dateobj :monthOfYear)) 1)])
        _year (value (property _dateobj :year))]

        (str _dayofweek " " _month " " _dayofmonth ", " _year)

))


(defn createDefaultCalendarVacant [ dateutc iterator ] 

    {
      :_id           (str "HHHHHHHH-" dateutc iterator)
      :name          ""
      :type          "vacant"
      :tags          {}
      :color         "blue"
      :background    ""
      :custom        ""
      :subscribers   []
      :auxtabs       []
    }
)


(defn createDefaultCalendarStub [ dateutc iterator ]

    {
      :_id           (str "GGGGGGGGHOOLA-" dateutc iterator)
      :name          (dateToString (date-time (read-string (str dateutc))))
      :type          "stub"
      :tags          ()
      :color         "blue"
      :background    ""
      :custom        ""
      :subscribers   []
      :auxtabs       []
    }
)



(defn createDefaultCalendarTab [tabid vacantid stubid] 

  {
    :_id                       tabid 
    :heightstate               "dooper"
    :percentageheight          100
    :inventory                 [
        {
            :_id                       vacantid
            :isFavorite                false
            :trueindex                 0
            :currenttab                "vacant"
            :currentdate               nil
            :style                     {
                                           :width    "250px"
                                           :height   "271px"
                                           :padding  "30px"
                                       }
        } {
            :_id                       stubid
            :isFavorite                false
            :trueindex                 1
            :currenttab                "stub"
            :currentdate               nil
            :style                     {
                                           :width    "250px"
                                           :height   "271px"
                                           :padding  "30px"
                                       }
        }
    ] 
  }
)


(defn createGenericTab [tabid] 
  
    {
      :_id                       tabid
      :heightstate               "dooper"
      :percentageheight          100
      :inventory                 [] 
    }
)


(defn createNewTabObj [tabid type iterator]

    (if (= type "calendar")
        (let [dateutc       (c/to-long (t/today-at 12 00))
             _vacantobj     (createDefaultCalendarVacant dateutc iterator)
             _stubobj       (createDefaultCalendarStub dateutc iterator)]

            (createDefaultCalendarTab tabid 
                                      (get-in _vacantobj [:_id]) 
                                      (get-in _stubobj [:_id]))
        )
        (createGenericTab tabid)
    )
)




(defn fetchNewKeystring [nodeobj]

    (if (= (get-in nodeobj [:type]) "calendar")
        (c/to-long (t/today-at 12 00))
        "ALLRESOURCES"
    )
)




(defn fetchCurretTab [nodeobj]

    (if (= "calendar" (get-in nodeobj [:type]))
        "calendar"
        "basic"
    )
)


; (defn fetchMasterTabidFromNodeObj [nodeobj leaf]

;     (str (get-in element [:_id]) "_" (fetchCurretTab element) "_master")
; )


(defn fetchTabidFromPointerObj [pointerobj]

    (str (get-in pointerobj [:_id]) "_"
         ())
)



(defn fetchExistingKeystring [nodeobj pointerobj]

    (if (= (get-in nodeobj [:type]) "calendar")
        (get-in pointerobj [:currentdate])
        "ALLRESOURCES"
    )
)





(defn ALTcloneTabObj [tabobj instancestamp iterator]

    (let [oldtabid            (get-in tabobj [:_id])
          newsuffix           (str "_" instancestamp iterator)
          newtabid            (clojure.string/replace oldtabid #"_master" newsuffix)
          returntabobj        (assoc tabobj :_id newtabid)]

          returntabobj
    )
)


; 1. ITERATES OVER EACH INVENTORY POINTER AND CHANGES THE INSTANCE PROPERTY VALUE
; 2. DOES NOT!!! CREATES A NEW TABOBJ FOR EACH NEW INSTANCE 
(defn instantiateChildrenFromTheirMaster [ tabobj ] 

    (let [inventory_pointer_array   (get-in tabobj [:inventory])
          randomprefix              (c/to-long (t/today-at 12 00))  
          ncoll                     "nodes"
          tcoll                     "tabs"
          inventory_node_ids        (map (fn [element] (get-in element [:_id])) inventory_pointer_array)
          inventory_node_datastore  (createReportFromObjArray (mc/find-maps mongoconnection ncoll {:_id { "$in" inventory_node_ids }}))
          inventory_tab_ids         (map (fn [element] 

                                            (let [nodeid    (get-in element [:_id])
                                                  nodeobj   (get-in inventory_node_datastore [(keyword nodeid)])
                                                  instance  (get-in element [:instance])]

                                                (str nodeid "_" (fetchExistingKeystring nodeobj element) "_master")
                                                ; (str nodeid "_" (fetchExistingKeystring nodeobj element) "_" instance)
                                            )
                                          )
                                          inventory_pointer_array
                                    )
          inventory_tab_datastore   (createReportFromObjArray (mc/find-maps mongoconnection tcoll {:_id { "$in" inventory_tab_ids }}))
          revised_inventory_array   (map (fn [element] 

                                            (let [nodeid            (get-in element [:_id])
                                                  nodeobj           (get-in inventory_node_datastore [(keyword nodeid)])
                                                  tabid             (str nodeid "_" (fetchExistingKeystring nodeobj element) "_master")
                                                  tabobj            (get-in inventory_tab_datastore [(keyword tabid)])
                                                  randomtimestamp   (c/to-long (l/local-now))
                                                  iterator          (str "_" (.indexOf inventory_pointer_array element) "_")
                                                  new_tab_obj       (if (nil? tabobj)
                                                                        (createNewTabObj tabid 
                                                                                         (get-in nodeobj [:type])
                                                                                         (.indexOf inventory_pointer_array element)) 
                                                                        (ALTcloneTabObj tabobj 
                                                                                        randomtimestamp
                                                                                        (.indexOf inventory_pointer_array element)) 
                                                                     )] 

                                                  ; new_tab_obj
                                                        (assoc element :instance (str randomtimestamp iterator) 
                                                                       :currenttab (fetchCurretTab nodeobj))
                                            )
                                         ) 
                                         inventory_pointer_array
                                    )]

; inventory_pointer_array
(assoc tabobj :inventory inventory_pointer_array)


          ; inventory_tab_ids         (map (fn [element] (fetchTabidFromPointerObj element)) inventory_pointer_array)
          ; inventory_node_objs       (mc/find-maps mongoconnection ncoll {:_id { "$in" inventory_node_ids }})
          ; report                    (group-by #(:_id %) inventory_node_objs)
          ; inventory_tab_ids         (map (fn [element] (fetchMasterTabidFromNodeObj element)) inventory_node_objs)
          ; tab_fetch_query_regex     (str "^" (clojure.string/join "|^" master_tabid_class_input_array))
          ; inventory_tab_objs        (mc/find-maps mongoconnection tcoll {:_id {$regex tab_fetch_query_regex }})
          ; new_node_id_batch         (map (fn [element] (clojure.string/join "_" (butlast (butlast (clojure.string/split (get-in element [:_id]) #"_"))))) 
          ;                                 new_node_obj_batch)





          ; new_node_obj_batch        (map (fn [element] (assoc element :_id (str (get-in element [:_id]) "_"
          ;                                                                       (fetchNewKeystring element) "_"
          ;                                                                       randomprefix (.indexOf inventory_node_objs element)))) 
          ;                                 inventory_node_objs)
          ; new_node_id_batch         (map (fn [element] (clojure.string/join "_" (butlast (butlast (clojure.string/split (get-in element [:_id]) #"_"))))) 
          ;                                 new_node_obj_batch)
          ; new_node_obj_datastore    (zipmap new_node_id_batch inventory_node_objs)

          ; new_sideeffect_nodeobjs   (if (mc/insert-batch mongoconnection tcoll new_node_obj_batch)
          ;                               true
          ;                               false)]

;; NOW NEED TO MAKE THIS RECURSIVRE AND GO ANOTHER LAYER DEEP FOR APPLICATION > CATEGORY > LANGUAGE-ARTS_ALLRESOURCES

;; ALSO NEED TO ACCOUNT FOR LAST ARGUMENT

          ; (assoc tabobj :inventory revised_inventory_array)
          ; inventory_node_datastore
    )
)



(defn cloneTabObj [tabobj instancestamp iterator]

    (let [oldtabid            (get-in tabobj [:_id])
          newsuffix           (str "_" instancestamp iterator)
          newtabid            (clojure.string/replace oldtabid #"_master" newsuffix)
          returntabobj        (assoc tabobj :_id newtabid)]

          (instantiateChildrenFromTheirMaster returntabobj)
    )
)






(defn processUrlArgs [argarray]

    (let [ncoll                                   "nodes"
          tcoll                                   "tabs"
         _argarray                                argarray
         _lastarg                                 (last argarray)
         _islastargnode?                          (if (< 0 (count (get-node _lastarg))) true false)
          args_traverse_depth                     2
          argument_node_objs                      (mc/find-maps mongoconnection ncoll {:_id { "$in" _argarray }})
          argument_node_instantiation_objs_raw    (if (< 0 (count (pop argarray)))
                                                      (flatten (map (fn [element] (get-in element [:instantiations]) ) argument_node_objs)))
          argument_node_input_instantiation_ids   (distinct (flatten (map (fn [element] (vals element) ) argument_node_instantiation_objs_raw)))
          argument_node_instantiation_objs_final  (with-collection mongoconnection ncoll
                                                        (find {:_id { "$in" argument_node_input_instantiation_ids }})
                                                        ; (fields [:_id])
                                                        ;; it is VERY IMPORTANT to use array maps with sort
                                                        (sort (array-map :_id 1))
                                                        ; (limit 10)
                                                  )
          argument_node_output_instantiation_ids  (map (fn [element] (get-in element [:_id]) ) argument_node_instantiation_objs_final)
          tab_fetch_query_regex                   (str "^" (clojure.string/join "|^" argument_node_output_instantiation_ids))
          all_possible_related_tab_instances      (mc/find-maps mongoconnection tcoll {:_id {$regex tab_fetch_query_regex }})
          argument_node_hashmap                   (zipmap argument_node_output_instantiation_ids argument_node_instantiation_objs_final)
          instantiations_pointer_obj_array        (map (fn [element] {
                                                              :_id         element
                                                              :instance    (to-millis-from-epoch (date-time))
                                                              :currenttab  (if (= "calendar" (get-in (get-in argument_node_hashmap [element]) [:type])) "day" "all")
                                                              :currentdate (if (and (not _islastargnode?) (not (nil? (String->Number _lastarg)))) _lastarg nil)
                                                       }) argument_node_input_instantiation_ids)]


; (println "GOOFIEst" (map (fn [element] (str element ".*")) argument_node_output_instantiation_ids))
; (println "GOOFIEst" (mc/find-maps mongoconnection ncoll {:_id {$regex { "$in" (map (fn [element] (str element ".*")) argument_node_output_instantiation_ids) }}}))
; (println "GOOFIER" (mc/find-maps mongoconnection ncoll {:_id {$regex "^mysyllabi_hist|^categ"  }}))
; (println "GOOFIER" all_possible_related_tab_instances)


          (traversetree instantiations_pointer_obj_array args_traverse_depth)
    )
)






(defn experimenting [argarray]

    (let [ncoll                                   "nodes"
          tcoll                                   "tabs"
         _argarray                                argarray
         _lastarg                                 (last argarray)
         _islastargnode?                          (if (< 0 (count (get-node _lastarg))) true false)
          args_traverse_depth                     2
          urlargs_node_objs                       (mc/find-maps mongoconnection ncoll {:_id { "$in" _argarray }})
          instantiation_objs                      (if (< 0 (count urlargs_node_objs))
                                                      (flatten (map (fn [element] (get-in element [:instantiations]) ) urlargs_node_objs)) ())
          argument_node_instantiation_objs_raw    (if (< 0 (count urlargs_node_objs))
                                                      (flatten (map (fn [element] (get-in element [:instantiations]) ) urlargs_node_objs)) ())
          argument_node_input_instantiation_ids   (distinct (flatten (map (fn [element] (vals element) ) argument_node_instantiation_objs_raw)))
          argument_node_instantiation_objs_final  (with-collection mongoconnection ncoll
                                                        (find {:_id { "$in" argument_node_input_instantiation_ids }})
                                                        ; (fields [:_id])
                                                        ;; it is VERY IMPORTANT to use array maps with sort
                                                        (sort (array-map :_id 1))
                                                        ; (limit 10)
                                                  )
          argument_node_hashmap                   (zipmap argument_node_input_instantiation_ids argument_node_instantiation_objs_final)



          ; argument_node_output_instantiation_ids  (map (fn [element] (get-in element [:_id]) ) argument_node_instantiation_objs_final)
          master_tabid_class_input_array          (map (fn [element] (str (get-in element [:_id]) "_" (fetchNewKeystring element) "_master")) 
                                                        argument_node_instantiation_objs_final)
          reflection_tabid_to_nodeobj             (zipmap master_tabid_class_input_array argument_node_instantiation_objs_final)
          tab_fetch_query_regex                   (str "^" (clojure.string/join "|^" master_tabid_class_input_array))
          fetched_master_tabobj                   (mc/find-maps mongoconnection tcoll {:_id {$regex tab_fetch_query_regex }})
          master_tabid_class_store_obj            (zipmap (map (fn [element] (get-in element [:_id])) fetched_master_tabobj) fetched_master_tabobj)
          url_tab_objs                            (map (fn [element] (if (nil? (get-in master_tabid_class_store_obj [element]))
                                                                         (createNewTabObj element 
                                                                                          (get-in (get-in reflection_tabid_to_nodeobj [element]) [:type])
                                                                                          (.indexOf master_tabid_class_input_array element)) 
                                                                         (cloneTabObj (get-in master_tabid_class_store_obj [element]) 
                                                                                      (c/to-long (l/local-now)) 
                                                                                      (.indexOf master_tabid_class_input_array element)) 
                                                                     )
                                                        )
                                                        master_tabid_class_input_array)

          instantiations_pointer_obj_array        (map (fn [element] {
                                                              :_id         element
                                                              :instance    (to-millis-from-epoch (date-time))
                                                              :currenttab  (if (= "calendar" (get-in (get-in argument_node_hashmap [element]) [:type])) "day" "all")
                                                              :currentdate (if (and (not _islastargnode?) (not (nil? (String->Number _lastarg)))) _lastarg nil)
                                                       }) argument_node_input_instantiation_ids)]

;1.clone master obj
;2.change tabid of obj with incrementing (to-millis-from-epoch (date-time))
;3.iterate through each child and set new incrementing (to-millis-from-epoch (date-time)) instance value
;4.post new object to database
;5.store tabid's of new objs in array
;6.feed array to traversetree function 



; (println "GOOFIEst" (map (fn [element] (str element ".*")) argument_node_output_instantiation_ids))
; (println "GOOFIEst" (mc/find-maps mongoconnection ncoll {:_id {$regex { "$in" (map (fn [element] (str element ".*")) argument_node_output_instantiation_ids) }}}))
; (println "GOOFIER" (mc/find-maps mongoconnection ncoll {:_id {$regex "^mysyllabi_hist|^categ"  }}))
; (println "GOOFIER" all_possible_related_tab_instances)


          ; (map (fn [element] (if (nil? (get-in master_tabid_class_store_obj [element]))
          ;                         )) master_tabid_class_input_array)
        instantiation_objs
    )
)

;; ## 1. Traverse and Fetch tabid's and nodeod's from MYSYLLABI
;; ## 2. Build key/val object datastore for each nodeid listener (key) and its corresponding tabobj (val)

;; ## 3. Interpret url arguments
;; ## 4. Fetch corresponding nodeobjs and interpret their instatiations value
;; ## 5. Fetch nodeobj from instantiation values and make a data store: key -> destination, value -> array of nodeobjs
;; ## 6. For each key, check the Listener Data store and if element is present there, then:
            ;; (a) get-in element from listener data store and clone the nodeobj and tabobj with new GUEST and _instance markers
            ;; (b) push nodeobj onto inventory of newly cloned objects


; {
;     :nodeids        application_node_id_array
;     :nodeobjs       application_node_obj_array
;     :tabids         application_tab_id_array
;     :tabobjs        application_tab_obj_array
;     :tablisteners   application_tab_listener_datastore
; }



(defn flattenInstantiationObject [ urlarg_instantiation_datastore ]

    (reduce-kv (fn [returnarray destination_key nodeids_array_value] (concat returnarray nodeids_array_value)) 
                [] 
                urlarg_instantiation_datastore)

)


(defn fetchInstantiationIds [argarray]

    (let [args_node_objs    (mc/find-maps mongoconnection "nodes" {:_id { "$in" argarray }})]

        (reduce (fn [returnobj pointerobj_element]
 
                    {
                        :instantiation_hash         (merge-with concat (get-in returnobj [:instantiation_hash]) 
                                                                       (get-in pointerobj_element [:instantiations]))

                        :instantiation_id_tally     (concat (get-in returnobj [:instantiation_id_tally]) 
                                                            (flattenInstantiationObject (get-in pointerobj_element [:instantiations])))
                    }      
                )
                {
                    :instantiation_hash         {}
                    :instantiation_id_tally     [] 
                }
                args_node_objs
        )
    )
)



(defn createNewPointerObj [candidate_nodeid unique_string]

    {
        :_id                       candidate_nodeid
        :urlnodelistener           true 
        :instance                  (str (c/to-long (l/local-now)) "-" unique_string)
        :isFavorite                false
        :isOpen                    true
        :currenttab                "all"
        :currentdate               (c/to-long (t/today-at 12 00))
        :layoutbehavior            "row"
    }
)


(defn checkUsernameOfTabid [username tabid]

    (let [splitarray (clojure.string/split tabid #"_")]

        (if (= username (first splitarray))
            tabid
            (if (< (count splitarray) 4)
                (clojure.string/join "_" (cons username splitarray))
                (clojure.string/join "_" (cons username (rest splitarray)))
            )
        )
    )
)


(defn checkUsernameOfNodeid [username nodeid]

    (let [splitarray    (clojure.string/split nodeid #"_")]

        (if (= username (first splitarray))
            nodeid
            (if (< (count splitarray) 2)
                (clojure.string/join "_" (cons username splitarray))
                (clojure.string/join "_" (cons username (rest splitarray)))
            )
        )
    )
)


(defn fetchTargetTabObj [ tabid instance_datastore]

    (let [tab_key       (keyword tabid)
          candidate     (get-in instance_datastore [:old_tabs tab_key])]

        (if (string? candidate)
            (get-in instance_datastore [ :new_tabs (keyword candidate) ])
            candidate
        )
    )
)


(defn fetchTargetNodeObj [ nodeid instance_datastore]

    (let [node_key      (keyword nodeid)
          candidate     (get-in instance_datastore [:old_nodes node_key])]

        (if (string? candidate)
            (get-in instance_datastore [ :new_nodes (keyword candidate) ])
            candidate
        )
    )
)


(defn bindPointersToInventory [candidates_array target_tab_obj wedgeindex outerindex iterator]

    (get-in (reduce (fn [returnobj nodeid]

                        (let [inner_iterator        (get-in returnobj [:innerindex])
                              new_pointer_obj       (createNewPointerObj nodeid (str inner_iterator "-" wedgeindex "-" outerindex "-" iterator))
                              reduced_obj           (get-in returnobj [:tabobj])
                              new_inventory_array   (into [new_pointer_obj] (get-in reduced_obj [:inventory]))] ; IN FUTURE, ADD VALIDATION HERE

                            {                   
                                :tabobj         (assoc reduced_obj :inventory new_inventory_array) 
                                :innerindex     (inc inner_iterator)
                            }
                        )   
                    )
                    {
                         :tabobj     target_tab_obj
                         :innerindex 0
                    }
                    candidates_array
            )
            [:tabobj]
    )
)



(defn updateDatastore [instance_datastore new_addition_packet]

    (let [old_tabid_key                     (keyword (get-in new_addition_packet [:old_tabid]))
          old_nodeid_key                    (keyword (get-in new_addition_packet [:old_nodeid]))
          new_tabobj                        (get-in new_addition_packet [:new_tabobj])
          new_nodeobj                       (get-in new_addition_packet [:new_nodeobj])
          new_tabid                         (get-in new_tabobj [:_id])
          new_nodeid                        (get-in new_nodeobj [:_id])
          new_tabid_key                     (keyword new_tabid)
          new_nodeid_key                    (keyword new_nodeid)

          nil_check_target_new_tabs_store   (if (nil? (get-in instance_datastore [:new_tabs])) 
                                                (get-in instance_datastore [:old_tabs]) 
                                                (get-in instance_datastore [:new_tabs]))
          nil_check_target_new_nodes_store  (if (nil? (get-in instance_datastore [:new_nodes])) 
                                                (get-in instance_datastore [:old_nodes]) 
                                                (get-in instance_datastore [:new_nodes]))

          old_tab_datastore                 (assoc (get-in instance_datastore [:old_tabs]) old_tabid_key new_tabid)
          old_node_datastore                (assoc (get-in instance_datastore [:old_nodes]) old_nodeid_key new_nodeid)
          new_tab_datastore                 (assoc (dissoc nil_check_target_new_tabs_store old_tabid_key) new_tabid_key new_tabobj )
          new_node_datastore                (assoc (dissoc nil_check_target_new_nodes_store old_nodeid_key) new_nodeid_key new_nodeobj )

          comandeered_hash                  (assoc (get-in instance_datastore [:comandeered_hash]) old_nodeid_key new_nodeid)]


                (assoc instance_datastore :new_tabs             new_tab_datastore
                                          :old_tabs             old_tab_datastore
                                          :new_nodes            new_node_datastore
                                          :old_nodes            old_node_datastore
                                          :comandeered_hash     comandeered_hash
                )
    )
)



; (defn processBubblepath [child_nodeid bubblepath_array instance_datastore username] ;; cmpref_hash: when is this updated? comandeer?

;     (cond (nil? (get-in (first bubblepath_array) [:ancestor_nodeid])) ;; root ... this not needed -> (empty? bubblepath_array)

;                 {
;                     :instance_datastore  instance_datastore
;                     :bubblepath          bubblepath_array    ;; root or packet was already revised
;                 }

;           (checkOwnershipOfNodeid username (get-in (first bubblepath_array) [:ancestor_nodeid]) instance_datastore)

;                 (let [target_bubble_packet      (first bubblepath_array)
;                       old_nodeid                (get-in target_bubble_packet [:ancestor_tabid])
;                       target_node_obj           (fetchTargetTabObj old_nodeid instance_datastore)
;                       target_nodeid             (get-in target_node_obj [:_id])
;                       target_pointer_index      (get-in target_bubble_packet [:pointer_index])
;                       old_tabid                 (get-in target_bubble_packet [:ancestor_nodeid])
;                       target_tab_obj            (fetchTargetNodeObj old_tabid instance_datastore)
;                       target_tabid              (get-in target_tab_obj [:_id])]
;                       ; target_bubblepath         (get-in instance_datastore [:cmp_refs (keyword target_nodeid)])] 

; (println "wooz" target_nodeid)
; (println "wooz" bubblepath_array)

;                     {
;                         :instance_datastore  instance_datastore
;                         :bubblepath          (into [{
;                                                 :ancestor_tabid     target_nodeid
;                                                 :pointer_index      target_pointer_index
;                                                 :ancestor_nodeid    target_tabid
;                                              }]
;                                              (rest bubblepath_array))
;                     }
;                 )

;                 ; {
;                 ;     :instance_datastore  instance_datastore
;                 ;     :bubblepath          bubblepath_array    ;; root or packet was already revised
;                 ; }

;           :else

;                 (let [target_parent                 (first bubblepath_array)
;                       target_parent_nodeid          (get-in target_parent [:ancestor_nodeid])
;                       target_parent_nodeobj         (fetchTargetNodeObj target_parent_nodeid instance_datastore)
;                       new_nodeid                    (checkUsernameOfNodeid username target_parent_nodeid)
;                       new_nodeobj                   (assoc target_parent_nodeobj :_id new_nodeid)
;                       target_parent_pointerindex    (get-in target_parent [:pointer_index])
;                       similar_cmprefs               (get-in instance_datastore [:cmp_refs (keyword target_parent_nodeid)])
;                       processed_cmprefs             (reduce (fn [reduced_obj cmpref_packet]

;                                                                 (let [cmpref_tabid          (get-in cmpref_packet [:target_tab_id])
;                                                                       cmpref_bubblepath     (get-in cmpref_packet [:bubblepath])
;                                                                       cmpref_tab_obj        (fetchTargetTabObj cmpref_tabid reduced_obj)
;                                                                       new_tabid             (checkUsernameOfTabid username cmpref_tabid)
;                                                                       new_tabobj            (relabelPointerInsideInventoryByIndex cmpref_tab_obj
;                                                                                                                                   target_parent_pointerindex
;                                                                                                                                   child_nodeid
;                                                                                                                                   new_tabid
;                                                                                                                                   username)
;                                                                       processed_obj         (updateDatastore reduced_obj {
;                                                                                                                                 :old_tabid      cmpref_tabid
;                                                                                                                                 :old_nodeid     target_parent_nodeid
;                                                                                                                                 :new_tabobj     new_tabobj
;                                                                                                                                 :new_nodeobj    new_nodeobj
;                                                                                                                                 :bubblepath     cmpref_bubblepath
;                                                                                                                          })
;                                                                       in_processed_bubble   (processBubblepath target_parent_nodeid 
;                                                                                                                cmpref_bubblepath 
;                                                                                                                processed_obj 
;                                                                                                                username)]

;                                                                         ; (println "wooz" cmpref_tabid "-"
;                                                                         ;                   (checkOwnershipOfNodeid username (get-in (first bubblepath_array) [:ancestor_nodeid]) instance_datastore) "-"
;                                                                         ;                   target_parent_nodeid)

;                                                                         {
;                                                                             :instance_datastore (get-in in_processed_bubble [:instance_datastore])
;                                                                             :bubblepath         (into [{
;                                                                                                         :ancestor_tabid     new_tabid 
;                                                                                                         :pointer_index      target_parent_pointerindex 
;                                                                                                         :ancestor_nodeid    new_nodeid    
;                                                                                                       }]
;                                                                                                       (get-in in_processed_bubble [:bubblepath])
;                                                                                                 )
;                                                                         }
;                                                                 )
;                                                             )
;                                                             instance_datastore
;                                                             similar_cmprefs
;                                                     )
;                       out_processed_bubble          (processBubblepath target_parent_nodeid 
;                                                                        (rest bubblepath_array) 
;                                                                        (get-in processed_cmprefs [:instance_datastore]) 
;                                                                        username)]

;                             { 
;                                 :instance_datastore     (get-in out_processed_bubble [:instance_datastore])
;                                 :bubblepath             (into []
;                                                               (get-in out_processed_bubble [:bubblepath])
;                                                         )
;                             }
;                 )
;     )
; )







;; true is owner      
;; false is not owner
(defn checkOwnershipOfTabid [username tabid instance_datastore]

    (let [tabobj        (fetchTargetTabObj tabid instance_datastore)
          targettabid   (get-in tabobj [:_id])
          splitarray    (clojure.string/split targettabid #"_")]

        (if (= username (first splitarray)) true false)
    )
)

;; true is owner      
;; false is not owner
(defn checkOwnershipOfNodeid [username nodeid instance_datastore]

    (let [nodeobj       (fetchTargetNodeObj nodeid instance_datastore)
          targetnodeid  (get-in nodeobj [:_id])
          splitarray    (clojure.string/split targetnodeid #"_")]

        (if (= username (first splitarray)) true false)
    )
)



(defn relabelPointerInsideInventoryByIndex [tabobj targetindex child_nodeid new_tabid username]

    (assoc (assoc-in tabobj [:inventory targetindex] {:_id child_nodeid}) :_id new_tabid)
)





;; TO DO
;; 2. cmpref_hash should be edited in bubble function
;; 5. taxonomy tab should be replace instead of prepend/unshift
;; 6. Consider calendar scenarios


; :cmpref_datastore           (if (leaf? pointerobj_element)
;                                 (merge-with concat old_cmpref_datastore {(keyword nodeid) [ element_info_packet ]})
;                                 old_cmpref_datastore
;                             )

(defn fetchBubblePathFromCmpRef [nodeid]

)





;; @@!!@@!! CHANGE IT SO THAT EVERYTHING CRAWLS UP EXCEPT root AND THE PIPE CONDICTIONAL TO DETERMINE WHETHER TO CREATE NEW TAB AND MUTATE DATASTORE


(defn processBubblepath [child_nodeid bubblepath_array instance_datastore username] ;; cmpref_hash: when is this updated? comandeer?

    (cond (nil? (get-in (first bubblepath_array) [:ancestor_nodeid])) ;; root ... this not needed -> (empty? bubblepath_array)

                {
                    :instance_datastore  instance_datastore
                    :bubblepath          bubblepath_array    ;; root or packet was already revised
                }

          (checkOwnershipOfNodeid username (get-in (first bubblepath_array) [:ancestor_nodeid]) instance_datastore)

                (let [target_bubble_packet      (first bubblepath_array)
                      old_nodeid                (get-in target_bubble_packet [:ancestor_tabid])
                      target_node_obj           (fetchTargetTabObj old_nodeid instance_datastore)
                      target_nodeid             (get-in target_node_obj [:_id])
                      target_pointer_index      (get-in target_bubble_packet [:pointer_index])
                      old_tabid                 (get-in target_bubble_packet [:ancestor_nodeid])
                      target_tab_obj            (fetchTargetNodeObj old_tabid instance_datastore)
                      target_tabid              (get-in target_tab_obj [:_id])]
                      ; target_bubblepath         (get-in instance_datastore [:cmp_refs (keyword target_nodeid)])] 

(println "wooz" target_nodeid)
(println "wooz" bubblepath_array)

                    {
                        :instance_datastore  instance_datastore
                        :bubblepath          (into [{
                                                :ancestor_tabid     target_tabid
                                                :pointer_index      target_pointer_index
                                                :ancestor_nodeid    target_nodeid
                                             }]
                                             (rest bubblepath_array))
                    }
                )

                ; {
                ;     :instance_datastore  instance_datastore
                ;     :bubblepath          bubblepath_array    ;; root or packet was already revised
                ; }

          :else

                (let [target_parent                 (first bubblepath_array)
                      target_parent_nodeid          (get-in target_parent [:ancestor_nodeid])
                      target_parent_nodeobj         (fetchTargetNodeObj target_parent_nodeid instance_datastore)
                      new_nodeid                    (checkUsernameOfNodeid username target_parent_nodeid)
                      new_nodeobj                   (assoc target_parent_nodeobj :_id new_nodeid)
                      target_parent_pointerindex    (get-in target_parent [:pointer_index])
                      similar_cmprefs               (get-in instance_datastore [:cmp_refs (keyword target_parent_nodeid)])
                      processed_cmprefs             (reduce (fn [reduced_obj cmpref_packet]

                                                                (let [cmpref_tabid          (get-in cmpref_packet [:target_tab_id])
                                                                      cmpref_bubblepath     (get-in cmpref_packet [:bubblepath])
                                                                      cmpref_tab_obj        (fetchTargetTabObj cmpref_tabid reduced_obj)
                                                                      new_tabid             (checkUsernameOfTabid username cmpref_tabid)
                                                                      new_tabobj            (relabelPointerInsideInventoryByIndex cmpref_tab_obj
                                                                                                                                  target_parent_pointerindex
                                                                                                                                  child_nodeid
                                                                                                                                  new_tabid
                                                                                                                                  username)
                                                                      processed_obj         (updateDatastore reduced_obj {
                                                                                                                                :old_tabid      cmpref_tabid
                                                                                                                                :old_nodeid     target_parent_nodeid
                                                                                                                                :new_tabobj     new_tabobj
                                                                                                                                :new_nodeobj    new_nodeobj
                                                                                                                                :bubblepath     cmpref_bubblepath
                                                                                                                         })
                                                                      in_processed_bubble   (processBubblepath target_parent_nodeid 
                                                                                                               cmpref_bubblepath 
                                                                                                               processed_obj 
                                                                                                               username)]

; (println "wooz" cmpref_tabid)
; (println "wooz" cmpref_bubblepath)
                                                                        ; (println "wooz" cmpref_tabid "-"
                                                                        ;                   (checkOwnershipOfNodeid username (get-in (first bubblepath_array) [:ancestor_nodeid]) instance_datastore) "-"
                                                                        ;                   target_parent_nodeid)

                                                                        {
                                                                            :instance_datastore (get-in in_processed_bubble [:instance_datastore])
                                                                            :bubblepath         (into [{
                                                                                                        :ancestor_tabid     new_tabid 
                                                                                                        :pointer_index      target_parent_pointerindex 
                                                                                                        :ancestor_nodeid    new_nodeid    
                                                                                                      }]
                                                                                                      (get-in in_processed_bubble [:bubblepath])
                                                                                                )
                                                                        }
                                                                )
                                                            )
                                                            instance_datastore
                                                            similar_cmprefs
                                                    )
                      out_processed_bubble          (processBubblepath target_parent_nodeid 
                                                                       (rest bubblepath_array) 
                                                                       (get-in processed_cmprefs [:instance_datastore]) 
                                                                       username)]

                            { 
                                :instance_datastore     (get-in out_processed_bubble [:instance_datastore])
                                :bubblepath             (into []
                                                              (get-in out_processed_bubble [:bubblepath])
                                                        )
                            }
                )
    )
)


; (defn processBubblepath [child_nodeid bubblepath_array instance_datastore username] ;; cmpref_hash: when is this updated? comandeer?

    ; (cond ;;(nil? (get-in (first bubblepath_array) [:ancestor_nodeid])) ;; root ... this not needed -> (empty? bubblepath_array)

          ;           {
          ;               :instance_datastore  instance_datastore
          ;               :bubblepath          bubblepath_array    ;; root
          ;           }

          ; (if (checkOwnershipOfTabid username (get-in (first bubblepath_array) [:ancestor_tabid]) instance_datastore)

          ;           (let [target_bubble                 (first bubblepath_array)
          ;                 target_nodeid                 (get-in target_bubble [:ancestor_nodeid])
          ;                 processed_bubble              (processBubblepath target_nodeid 
          ;                                                                  (rest bubblepath_array) 
          ;                                                                  instance_datastore
          ;                                                                  username)
          ;                 return_instance               (get-in processed_bubble [:instance_datastore])
          ;                 return_bubble                 (into [(first bubblepath_array)] 
          ;                                                     (rest (get-in processed_bubble [:bubblepath])))]

          ;               {
          ;                   :instance_datastore  return_instance
          ;                   :bubblepath          return_bubble
          ;               }
          ;           )

          ; :else

          ;       (let [target_parent                 (first bubblepath_array)
          ;             target_parent_nodeid          (get-in target_parent [:ancestor_nodeid])
          ;             target_parent_nodeobj         (fetchTargetNodeObj target_parent_nodeid instance_datastore)
          ;             new_nodeid                    (checkUsernameOfNodeid username target_parent_nodeid)
          ;             new_nodeobj                   (assoc target_parent_nodeobj :_id new_nodeid)
          ;             target_parent_pointerindex    (get-in target_parent [:pointer_index])
          ;             similar_cmprefs               (get-in instance_datastore [:cmp_refs (keyword target_parent_nodeid)])
          ;             processed_cmprefs             (reduce (fn [reduced_obj cmpref_packet]

          ;                                                       ; (let [cmpref_tabid          (get-in cmpref_packet [:target_tab_id])
          ;                                                       ;       cmpref_bubblepath     (get-in cmpref_packet [:bubblepath])
          ;                                                       ;       cmpref_tab_obj        (fetchTargetTabObj cmpref_tabid reduced_obj)
          ;                                                       ;       new_tabid             (checkUsernameOfTabid username cmpref_tabid)
          ;                                                       ;       new_tabobj            (relabelPointerInsideInventoryByIndex cmpref_tab_obj
          ;                                                       ;                                                                   target_parent_pointerindex
          ;                                                       ;                                                                   child_nodeid
          ;                                                       ;                                                                   new_tabid
          ;                                                       ;                                                                   username)
          ;                                                       ;       processed_obj         (updateDatastore reduced_obj {
          ;                                                       ;                                                                 :old_tabid      cmpref_tabid
          ;                                                       ;                                                                 :old_nodeid     target_parent_nodeid
          ;                                                       ;                                                                 :new_tabobj     new_tabobj
          ;                                                       ;                                                                 :new_nodeobj    new_nodeobj
          ;                                                       ;                                                                 :bubblepath     cmpref_bubblepath
          ;                                                       ;                                                          })
          ;                                                       ;       in_processed_bubble   (processBubblepath target_parent_nodeid 
          ;                                                       ;                                                cmpref_bubblepath 
          ;                                                       ;                                                processed_obj 
          ;                                                       ;                                                username)]

          ;                                                       ;         (println "wooz" cmpref_tabid "-"
          ;                                                       ;                           (checkOwnershipOfNodeid username (get-in (first bubblepath_array) [:ancestor_nodeid]) instance_datastore) "-"
          ;                                                       ;                           target_parent_nodeid)

          ;                                                       ;         {
          ;                                                       ;             :instance_datastore (get-in in_processed_bubble [:instance_datastore])
          ;                                                       ;             :bubblepath         (into [{
          ;                                                       ;                                         :ancestor_tabid     new_tabid 
          ;                                                       ;                                         :pointer_index      target_parent_pointerindex 
          ;                                                       ;                                         :ancestor_nodeid    new_nodeid    
          ;                                                       ;                                       }]
          ;                                                       ;                                       (get-in in_processed_bubble [:bubblepath])
          ;                                                       ;                                 )
          ;                                                       ;         }
          ;                                                       ; )
          ;                                                   )
          ;                                                   instance_datastore
          ;                                                   similar_cmprefs
          ;                                           )
          ;             out_processed_bubble          (processBubblepath target_parent_nodeid 
          ;                                                              (rest bubblepath_array) 
          ;                                                              (get-in processed_cmprefs [:instance_datastore]) 
          ;                                                              username)]

          ;                   { 
          ;                       :instance_datastore     (get-in out_processed_bubble [:instance_datastore])
          ;                       :bubblepath             (into []
          ;                                                     (get-in out_processed_bubble [:bubblepath])
          ;                                               )
          ;                   }
          ;       )
    ; )
; )


(defn processSimilarCmpRefs [candidates_array 
                             outerindex 
                             iterator 
                             instance_datastore 
                             username 
                             destination_node_id
                             new_nodeobj]


    (get-in (reduce (fn [reduced_obj cmpref_bundle]

                        (let [wedgeindex                (get-in reduced_obj [:wedgeindex])
                              reduced_datastore         (get-in reduced_obj [:instance_datastore])
                              cmpref_tab_id             (get-in cmpref_bundle [:target_tab_id])
                              cmpref_bubblepath         (get-in cmpref_bundle [:bubblepath])
                              target_tab_obj            (fetchTargetTabObj cmpref_tab_id reduced_datastore)
                              new_tabobj                (assoc (bindPointersToInventory candidates_array target_tab_obj wedgeindex outerindex iterator)
                                                               :_id (checkUsernameOfTabid username cmpref_tab_id))   

                              processed_obj             (updateDatastore reduced_datastore {
                                                                                                :old_tabid      cmpref_tab_id
                                                                                                :old_nodeid     destination_node_id
                                                                                                :new_tabobj     new_tabobj
                                                                                                :new_nodeobj    new_nodeobj
                                                                                                :bubblepath     cmpref_bubblepath
                                                                                           })
                              processed_bubble          (processBubblepath (get-in new_nodeobj [:_id]) 
                                                                           cmpref_bubblepath 
                                                                           processed_obj
                                                                           username)]

                              ; (println "FLOOZIES" (get-in processed_bubble [:bubblepath]))

                                {
                                    :instance_datastore     (get-in processed_bubble [:instance_datastore])
                                    :wedgeindex             (inc wedgeindex)
                                }
                        )
                    )
                    {
                         :instance_datastore     instance_datastore
                         :wedgeindex 0
                    }
                    (get-in instance_datastore [:cmp_refs (keyword destination_node_id)])

            ) [:instance_datastore] 
    )
)



;; THIS FUNCTION CAN BE REMOVED IN THE FUTURE BECAUSE LISTENERS SHOULD REFERENCE CMP_REF's INSTEAD 
;; (BECAUSE LISTENERS SHOULD UPDATE WHEREVER A NODE INSTANCE IS FOUND IN THE TREE)
;; !! NOT URGENT !!
(defn processNewListenerObjs [candidates_array listeners_array iterator username instance_datastore]

    (reduce (fn [reduced_obj destination_bundle]

                (let [reduced_datastore         (get-in reduced_obj [:instance_datastore])
                      outerindex                (get-in reduced_obj [:outerindex])
                      destination_node_id       (get-in destination_bundle [:target_node_id])
                      destination_node_keyword  (keyword destination_node_id)
                      target_node_obj           (fetchTargetNodeObj destination_node_id reduced_datastore)
                      new_nodeid                (checkUsernameOfNodeid username (get-in target_node_obj [:_id]))
                      new_nodeobj               (assoc target_node_obj :_id new_nodeid)

                      processed_obj             (processSimilarCmpRefs candidates_array 
                                                                       outerindex 
                                                                       iterator 
                                                                       reduced_datastore
                                                                       username
                                                                       destination_node_id
                                                                       new_nodeobj)]


                      {
                        :outerindex              (inc outerindex)
                        :instance_datastore      processed_obj
                      }
                )
            )
            {
                :outerindex             0
                :instance_datastore     instance_datastore
            }
            listeners_array
    )
)



(defn fetchListenerKeys [ listeners_array ]

    (map (fn [element] {
            :tabs   (keyword (get-in element [:target_tab_id]))
            :nodes  (keyword (get-in element [:target_node_id]))
         }) 
         listeners_array
    )
)



(defn processInstantiationIds [urlarg_datastore instance_batch_listeners instance_datastore username]

    (reduce-kv (fn [reduced_obj destination_key candidates_array] 

                    (let [reduced_instance              (get-in reduced_obj [:reduced_datastore])
                          destkey                       (keyword destination_key)
                          listeners_array               (get-in instance_batch_listeners [ destkey ])
                          iterator                      (get-in reduced_obj [:iterator])
                          processed_obj                 (processNewListenerObjs candidates_array 
                                                                                listeners_array
                                                                                iterator
                                                                                username
                                                                                reduced_instance)]
                            {
                                :reduced_datastore (get-in processed_obj [:instance_datastore])
                                :iterator (inc iterator)
                            }
                    )
               ) 
               {
                    :reduced_datastore instance_datastore
                    :iterator 0
               } 
               urlarg_datastore
    )
)

(defn ALTfetchDestinationObj [ listeners_array instance_datastore ]

    (let [listen_node_keys      (map (fn [element] (keyword (get-in element [:target_node_id]))) listeners_array)
          old_nodes_datastore   (get-in instance_datastore [:old_nodes])]

        {
            :node_destinations  (select-keys old_nodes_datastore listen_node_keys)
        }
    )
)


; :_id                √  RANDOM                                                 "581d0e52bcc6ee00ea9626d3"
; :currentdate        ?                                                         "1451721600041"
; :currenttab         ?                                                         "basic"
; :grandparent        √ (getin destination_obj [:target_grandparent])           "mysyllabi_taxonomy_ALLRESOURCES_master"
; :grandparent-order  ? (getin destination_obj [:target_grandparent_order])     1000
; :instance           √  RANDOM                                                 "1464739200000"
; :isFavorite         √  DEFAULT FALSE                                          false
; :nodeid             √  candidate_string                                       "letter-a"
; :order              √  CALCULATION                                            1000
; :owner              √  DEFAULT GUEST                                          "mysyllabi"
; :style              ?                                                         {}
; :tailparent         √ (getin destination_obj [:target_tail])                  "language-arts_ALLRESOURCES_master"



; {
;      :target_node_id                nodeid
;      :target_tail                   tailparent
;      :target_grandparent            grandparent
;      :target_grandparent_order      grandparent_order
; }

(defn ALTcreateNewPointer [candidate_string destination_obj]

    {
        :_id                        (c/to-long (l/local-now))
        :currentdate                nil
        :currenttab                 "basic"
        :grandparent                (get-in destination_obj [:target_grandparent])
        :grandparent-order          (get-in destination_obj [:target_grandparent_order])
        :instance                   (c/to-long (l/local-now))
        :isFavorite                 false
        :nodeid                     candidate_string
        :order                      0
        :owner                      "GUEST"
        :style                      {}
        :tailparent                 (get-in destination_obj [:target_tail])
    }
)


(defn ALTqrocessNewListenerObjs [candidates_array destinations_array]

    (reduce (fn [reduced_obj destination_obj]

                (let [pointer_batch_array   (map (fn [candidate_string] (ALTcreateNewPointer candidate_string destination_obj)) candidates_array)
                      destination_key       (keyword (get-in destination_obj [:target_tail]))]

                    (assoc reduced_obj destination_key  {

                            :candidate_obj_batch    pointer_batch_array
                            :zero_index_order       1
                        }
                    )
                )
         )
         {}
         destinations_array
    )
)


(defn ALTqrocessInstantiationIds [urlarg_candidates listeners_destinations instance_datastore username]

    (reduce-kv (fn [reduced_obj destination_key candidates_array] 

                    (let [reduced_instance              (get-in reduced_obj [:reduced_datastore])
                          destkey                       (keyword destination_key)
                          listeners_array               (get-in listeners_destinations [ destkey ])
                          ; destination_obj               (ALTfetchDestinationObj listeners_array reduced_instance)
                          iterator                      (get-in reduced_obj [:iterator])]
                    ;       qrocessed_listener_objs       (qrocessNewListenerObjs candidates_array 
                    ;                                                             listeners
                    ;                                                             iterator
                    ;                                                             destination_obj
                    ;                                                             username
                    ;                                                             reduced_instance)
                    ;       updated_datastore             (qpdateDatastore reduced_instance username qrocessed_listener_objs)]
                          
                    ;     (if (nil? listeners)

                    ;         reduced_obj
                    ;         {
                    ;             :reduced_datastore updated_datastore
                    ;             :iterator (inc iterator)
                    ;             :karabas destination_obj
                    ;         }
                    ;     )
                        ; (conj reduced_obj {
                        ;             :candidates         candidates_array
                        ;             :destinations       listeners_array
                        ;         })
                        (assoc reduced_obj destkey (ALTqrocessNewListenerObjs candidates_array listeners_array) )
                        
                    )
               )  ;;
               {}
               ; {
               ;      ; :reduced_datastore instance_datastore
               ;      ; :iterator 0
               ; } 
               urlarg_candidates
    )
)


(defn get-instance-batch [instanceid argarray]

    (let [tree_traverse_depth                     4
          args_traverse_depth                     2
          application_pointer_obj_array           (list {
                                                     :_id         (str instanceid "_application")
                                                     :currenttab  "all"
                                                     :instance    "master"
                                                  })
          existing_instance_tree_obj_batch        (traversetree application_pointer_obj_array tree_traverse_depth)
          argument_instantiations_obj_batch       (if (not-empty argarray) (processUrlArgs argarray) {})

          ; instance_batch_obj                      (ALTTraverseTree application_pointer_obj_array tree_traverse_depth)
          instance_batch_obj                      (IMPOSTERTraverseTree [{
                                                                            :tabobj          {
                                                                                                :_id            "root"
                                                                                                :inventory      [
                                                                                                    {
                                                                                                        :_id        "mysyllabi_application"
                                                                                                        :instance   "master"
                                                                                                    }
                                                                                                ]
                                                                                             }
                                                                            :bubblepath      []
                                                                         }] 
                                                                         tree_traverse_depth)
          SECRETinstance_batch_obj                 (SECRETTraverseTree [{ 
                                                                            :nodeid         "mysyllabi_application" 
                                                                            :tailparent     "root" 
                                                                            :owner          "mysyllabi" 
                                                                            :instance       "master" 
                                                                            :order          1000 
                                                                            :currenttab     "all"
                                                                       }] 
                                                                       tree_traverse_depth {})
          instance_tab_datastore                  (createReportFromObjArray (get-in instance_batch_obj [:tabobjs]))
          instance_node_datastore                 (createReportFromObjArray (get-in instance_batch_obj [:nodeobjs]))
          urlarg_instantiation_datastore          (if (= 0 (count argarray)) {} (fetchInstantiationIds argarray))]


          ; ALT_instance_node_datastore             (createReportFromObjArray (get-in SECRETinstance_batch_obj [:nodeobjs]))
          ; ALT_instance_pointer_datastore          (ALTcreateReportFromObjArray (get-in SECRETinstance_batch_obj [:pointerobjs]))]


          ; {
          ;   :nodes (flatten (conj (get-in existing_instance_tree_obj_batch  [:nodeobjs]) 
          ;                         (get-in argument_instantiations_obj_batch [:nodeobjs])))
          ;   :tabs  (flatten (conj (get-in existing_instance_tree_obj_batch  [:tabobjs]) 
          ;                         (get-in argument_instantiations_obj_batch [:tabobjs])))
          ;   :experiment argument_instantiations_obj_batch
          ; }
          
        {
            :nodes                      (get-in existing_instance_tree_obj_batch [:nodeobjs]) 
            :tabs                       (get-in existing_instance_tree_obj_batch [:tabobjs]) 
            :url_instantiation_ids       urlarg_instantiation_datastore
            :instance_batch_listeners   (get-in instance_batch_obj [:tablisteners])
            :processed                  (processInstantiationIds (get-in urlarg_instantiation_datastore [:instantiation_hash]) 
                                                                 (get-in instance_batch_obj [:tablisteners])
                                                                 {
                                                                    :old_tabs           instance_tab_datastore
                                                                    :old_nodes          instance_node_datastore
                                                                    :cmp_refs           (get-in instance_batch_obj [:cmprefs])
                                                                    :comandeered_hash   {}
                                                                 }
                                                                 "GUEST")
            :IMPOSTER_batch             instance_batch_obj
            :SECRET_batch               SECRETinstance_batch_obj
            ; :jammer                     (ALTqrocessInstantiationIds (get-in urlarg_instantiation_datastore [:instantiation_hash]) 
            ;                                                         (get-in SECRETinstance_batch_obj [:listeners])
            ;                                                         {
            ;                                                            :old_nodes      ALT_instance_node_datastore
            ;                                                            :old_pointers   ALT_instance_pointer_datastore
            ;                                                         }
            ;                                                         "GUEST")
        }
    )
)



(defn get-tab [leafid]
  (let [_leafid leafid
        tcoll "tabs"]

    (mc/find-maps mongoconnection tcoll {:_id _leafid})  
))




 
(defn create-tab-batch [payloadarray]
  (let [_payloadarray payloadarray
        _tabs (get-in _payloadarray [:batchobj :tabs])
        tcoll "tabs"
        returntabs (if (mc/insert-batch mongoconnection tcoll _tabs)
                        true
                        false)
        tabids (map (fn [element] (get-in element [:_id])) _tabs)
        existingtabids (map (fn [element] (get-in element [:_id])) (mc/find-maps mongoconnection tcoll {:_id {$in tabids}}))
        newtabids (clojure.set/difference (set tabids) (set existingtabids))
        newtabobjs (remove nil? (map (fn [element] (if (contains? newtabids (get-in element [:_id])) element)) _tabs))
        returntabs (if (and (not (empty? newtabobjs)) (mc/insert-batch mongoconnection tcoll newtabobjs))
                      true
                      false)]

  ; (println "&&&&&&" _payloadarray)

        (if (= true returntabs)
          {
            :tabs _tabs
          }
          false)

))

(defn create-generic-pointer-obj [pointerid]

  (let [_pointerid pointerid]
    {
        :_id                       _pointerid
        :isFavorite                false
        :isOpen                    true
        :trueindex                 0
        :currenttab                "all"
        :currentdate               nil
    }
))

(defn decrement-leaf-count [payloadobj]
  (let [_nodeid (get-in payloadobj [:nodeid])
        _leaftype (get-in payloadobj [:leaftype])
        _decamount (get-in payloadobj [:decamount])
        _leafpath (str "leaves." _leaftype ".count")
          ncoll "nodes"
          tcoll "tabs"
        _returndeccounter (if (mc/update mongoconnection ncoll {:_id _nodeid} { "$inc" { _leafpath _decamount}} )
                  true
                  false)]

          (if (= true _returndeccounter) 
              {
                :nodeid _nodeid
              }
              false)
))
 
(defn edit-node-from-pointer [payloadobj]
  (let [_nodeid (get-in payloadobj [:nodeid])
        _linkobject (get-in payloadobj [:linkobject])
          uri mongo-uri
            {:keys [conn db]} (mg/connect-via-uri uri)
          ncoll "nodes"
        _returnnodeedits (if (mc/update mongoconnection ncoll {:_id _nodeid} { $set
                                                                                {
                                                                                    :name (get-in _linkobject [:name])
                                                                                    :background (get-in _linkobject [:background])
                                                                                }
                                                                })
                  true
                  false)]

          (if (= true _returnnodeedits) 
              {
                :nodeid _nodeid
                :linkobject _linkobject
              }
              false)
))

(defn update-tab [tabpayload]
  (let [_tabpayload tabpayload
        tcoll "tabs"
        _tabid (get-in tabpayload [:tabid])
        _tabobj (get-in tabpayload [:tabobj])
        _tabupdated (if (mc/update mongoconnection tcoll {:_id _tabid} _tabobj)
                            true
                            false)]

        _tabupdated
))


(defn update-is-open [pointerpayload]
  (let [_pointerpayload pointerpayload
        tcoll "tabs"
        _tabid (get-in pointerpayload [:tabid])
        _index (get-in pointerpayload [:index])
        _bool (get-in pointerpayload [:bool])
        _refineselection (str "inventory." _index ".isOpen")
        _pointerupdated (if (mc/update mongoconnection tcoll {:_id _tabid} { "$set" { _refineselection _bool }  } )
                          true
                          false)]

        (println "*** _bool" _bool)
        (println "*** _tabid" _tabid)
        (println "*** _refineselection" _refineselection)

        _pointerupdated
))


(defn update-current-tab [pointerpayload]
  (let [_pointerpayload pointerpayload
        tcoll "tabs"
        _tabid (get-in pointerpayload [:tabid])
        _index (get-in pointerpayload [:index])
        _label (get-in pointerpayload [:tabname])
        _refineselection (str "inventory." _index ".currenttab")
        _pointerupdated (if (mc/update mongoconnection tcoll {:_id _tabid} { "$set" { _refineselection _label }  } )
                          true
                          false)]

        (println "*** _label" _label)
        (println "*** _tabid" _tabid)
        (println "*** _refineselection" _refineselection)

        _pointerupdated
))


(defn update-current-date [pointerpayload]
  (let [_pointerpayload pointerpayload
        tcoll "tabs"
        _tabid (get-in pointerpayload [:tabid])
        _index (get-in pointerpayload [:index])
        _utcdate (get-in pointerpayload [:utcdate])
        _refineselection (str "inventory." _index ".currentdate")
        _pointerupdated (if (mc/update mongoconnection tcoll {:_id _tabid} { "$set" { _refineselection _utcdate }  } )
                          true
                          false)]

        (println "*** _utcdate" _utcdate)
        (println "*** _tabid" _tabid)
        (println "*** _refineselection" _refineselection)

        _pointerupdated
))


(defn update-is-favorite [pointerpayload]
  (let [_pointerpayload pointerpayload
        tcoll "tabs"
        _tabid (get-in pointerpayload [:tabid])
        _index (get-in pointerpayload [:index])
        _bool (get-in pointerpayload [:bool])
        _refineselection (str "inventory." _index ".isFavorite")
        _pointerupdated (if (mc/update mongoconnection tcoll {:_id _tabid} { "$set" { _refineselection _bool }  } )
                          true
                          false)]

        (println "RRR _bool" _bool)
        (println "RRR _tabid" _tabid)
        (println "RRR _refineselection" _refineselection)

        _pointerupdated
))


(defn unshift-node-batch [payloadarray]
    (let [_payloadarray payloadarray
          _inventory    (get-in _payloadarray [:batchobj :inventory])
          _nodes        (get-in _payloadarray [:batchobj :nodes])
          _tabs         (get-in _payloadarray [:batchobj :tabs])
          _instantiate? (get-in _payloadarray [:shouldinstantiate])
          _parenttabid  (get-in _payloadarray [:tabid])
          _splittabid   (clojure.string/split _parenttabid #"_")
          _parentnodeid (clojure.string/join "_" (butlast _splittabid))
          ncoll "nodes"
          tcoll "tabs"
          _nodestoinstantiate (if _instantiate? 
                                  (vals (get-in (first _nodes) [:instantiations])))
          nodeids (if _instantiate?
                      (set (flatten (conj (map (fn [element] (get-in element [:_id])) _nodes) _nodestoinstantiate)))
                      (map (fn [element] (get-in element [:_id])) _nodes))
          _instantiatethenodes (if _instantiate? 
                                  (keys (get-in (first _nodes) [:instantiations])))      
          existingnodeids (map (fn [element] (get-in element [:_id])) (mc/find-maps mongoconnection ncoll {:_id {$in nodeids}}))
          newnodeids (clojure.set/difference (set nodeids) (set existingnodeids))
          newnodeobjs  (remove nil? (map (fn [element] (if (contains? newnodeids (get-in element [:_id])) element)) _nodes))
          returnnodes (if (and (not (empty? newnodeobjs)) (mc/insert-batch mongoconnection ncoll newnodeobjs))
                        true
                        false)
          returnpointers (if (mc/update mongoconnection tcoll {:_id _parenttabid} { $push { "inventory" { "$each" _inventory "$position" 0 } }  } )
                            true
                            false)
          leafpath (str "leaves." (last _splittabid) ".count")
          returninccounter (if (mc/update mongoconnection ncoll {:_id _parentnodeid} { "$inc" { leafpath (count _nodes)}} )
                            true
                            false)
          tabids (map (fn [element] (get-in element [:_id])) _tabs)
          existingtabids (map (fn [element] (get-in element [:_id])) (mc/find-maps mongoconnection tcoll {:_id {$in tabids}}))
          newtabids (clojure.set/difference (set tabids) (set existingtabids))
          newtabobjs (remove nil? (map (fn [element] (if (contains? newtabids (get-in element [:_id])) element)) _tabs))
          returntabs (if (and (not (empty? newtabobjs)) (mc/insert-batch mongoconnection tcoll newtabobjs))
                        true
                        false)
          _instantiationdestinations (keys (get-in (first _nodes) [:instantiations]))
          _authorid (get-in _payloadarray [:authorid])
          _successfulinstantiations (if 
                                        (if _instantiate?
                                            (map (fn [element] 
                                                    (mc/update mongoconnection tcoll { :_id (str _authorid "_" (clojure.core/name element) "_ALLRESOURCES") } 
                                                        { $push 
                                                            { "inventory" 
                                                                 { "$each" 
                                                                      (map
                                                                          (fn [element] (create-generic-pointer-obj element))
                                                                          (get-in
                                                                              (get-in (first _nodes) [:instantiations]) 
                                                                              [(keyword (clojure.core/name element))]
                                                                          )
                                                                      )
                                                                      "$position" 
                                                                      0 
                                                                 } 
                                                            }  
                                                        } 
                                                    )
                                                 )
                                                 _instantiationdestinations))
                                        true
                                        false)]

  ; (println ">>> _nodestoinstantiate" _nodestoinstantiate)
  ; (println ">>> nodeids" nodeids)
  ; (println ">>> _instantiatethenodes" _instantiatethenodes)
  ; (println ">>> existingnodeids" existingnodeids)
  ; (println ">>> newnodeids" newnodeids)
  ; (println ">>> newnodeobjs" newnodeobjs)
  ; (println ">>> returnnodes" returnnodes)

          (if (and (= true returnnodes) 
                   (= true returnpointers) 
                   (= true returntabs) 
                   (= true returninccounter)
                   (if _instantiate? (= true _successfulinstantiations)))
              (do    
                  (println "INSIDE" _successfulinstantiations)
                  {
                    :nodes _nodes
                    :inventory _inventory
                    :tabs _tabs
                  })
              false)
  )
)



(defn push-batch [payloadarray]
    (let [_payloadarray payloadarray
          _inventory (get-in _payloadarray [:batchobj :inventory])
          _nodes (get-in _payloadarray [:batchobj :nodes])
          _tabs (get-in _payloadarray [:batchobj :tabs])
          _parentid (get-in _payloadarray [:tabid])
          _pointerindex (get-in _payloadarray [:pointerindex])
          ncoll "nodes"
          tcoll "tabs"
          ; returnnodes (if (mc/insert-batch mongoconnection ncoll _nodes)
          ;                  true
          ;                  false)
          nodeids (map (fn [element] (get-in element [:_id])) _nodes)
          existingnodeids (map (fn [element] (get-in element [:_id])) (mc/find-maps mongoconnection ncoll {:_id {$in nodeids}}))
          newnodeids (clojure.set/difference (set nodeids) (set existingnodeids))
          newnodeobjs  (remove nil? (map (fn [element] (if (contains? newnodeids (get-in element [:_id])) element)) _nodes))
          returnnodes (if (and (not (empty? newnodeobjs)) (mc/insert-batch mongoconnection ncoll newnodeobjs))
                        true
                        false)
          returnpointers (if (mc/update mongoconnection tcoll {:_id _parentid} { $push { "inventory" { "$each" _inventory } } } )
                              true
                              false)
          tabids (map (fn [element] (get-in element [:_id])) _tabs)
          existingtabids (map (fn [element] (get-in element [:_id])) (mc/find-maps mongoconnection tcoll {:_id {$in tabids}}))
          newtabids (clojure.set/difference (set tabids) (set existingtabids))
          newtabobjs (remove nil? (map (fn [element] (if (contains? newtabids (get-in element [:_id])) element)) _tabs))
          returntabs (if (and (not (empty? newtabobjs)) (mc/insert-batch mongoconnection tcoll newtabobjs))
                        true
                        false)]

          (println "_tabs" _tabs)
          (println "tabids" tabids)
          (println "existingtabids" existingtabids)
          (println "newtabids" newtabids)
          (println "newtabobjs" newtabobjs)
          (println "returntabs" returntabs)

          (if (and (= true returnnodes) (= true returnpointers) (= true returntabs))
              {
                :nodes _nodes
                :inventory _inventory
                :tabs _tabs
              }
              false)
  )
)




(defn replace-node-at-index-batch [payloadarray]
    (let [_payloadarray payloadarray
          _inventory (get-in _payloadarray [:batchobj :inventory])
          _nodes (get-in _payloadarray [:batchobj :nodes])
          _tabs (get-in _payloadarray [:batchobj :tabs])
          _index (get-in _payloadarray [:pointerindex])
          _parentid (get-in _payloadarray [:tabid])
          ncoll "nodes"
          tcoll "tabs"
          nodeids (map (fn [element] (get-in element [:_id])) _nodes)
          existingnodeids (map (fn [element] (get-in element [:_id])) (mc/find-maps mongoconnection ncoll {:_id {$in nodeids}}))
          newnodeids (clojure.set/difference (set nodeids) (set existingnodeids))
          newnodeobjs  (remove nil? (map (fn [element] (if (contains? newnodeids (get-in element [:_id])) element)) _nodes))
          returnnodes (if (and (not (empty? newnodeobjs)) (mc/insert-batch mongoconnection ncoll newnodeobjs))
                        true
                        false)
          returnpointers (if (mc/update mongoconnection tcoll {:_id _parentid} { $set { (str "inventory." _index) (first _inventory) } } )
                            true
                            false)
          tabids (map (fn [element] (get-in element [:_id])) _tabs)
          existingtabids (map (fn [element] (get-in element [:_id])) (mc/find-maps mongoconnection tcoll {:_id {$in tabids}}))
          newtabids (clojure.set/difference (set tabids) (set existingtabids))
          newtabobjs (remove nil? (map (fn [element] (if (contains? newtabids (get-in element [:_id])) element)) _tabs))
          returntabs (if (and (not (empty? newtabobjs)) (mc/insert-batch mongoconnection tcoll newtabobjs))
                        true
                        false)]

          ; (mc/update mongoconnection tcoll {:_id _parentid} { $push { "inventory" { "$each" _inventory "$position" 0 } }  } )
                             

          (if (and (= true returnnodes) (= true returnpointers) (= true returntabs))
              {
                :nodes _nodes
                :inventory _inventory
                :tabs _tabs
              }
              false)
  )
)

(defn add-leaf-to-node [nodeid leafid tabid]
  (let [_nodeid nodeid
        _leafid leafid
        _tabid tabid
        ncoll "nodes"
        _leafobj {
                :_id _tabid
                :count 0
        } 

          ; (mc/update mongoconnection ncoll {:_id _nodeid} { "$set" { (str "leaves." _leafid) _leafobj }  } )
        _createleaf (if (mc/update mongoconnection ncoll {:_id _nodeid} { "$set" { (str "leaves." _leafid) _leafobj }  } )
                          true
                          false)]

        _createleaf
))


(defn create-node-tab-batch [payloadarray]
    (let [_payloadarray payloadarray
          _tabs (get-in _payloadarray [:batchobj :tabs])
          _nodes (get-in _payloadarray [:batchobj :nodes])
          ncoll "nodes"
          tcoll "tabs"
          nodeids (map (fn [element] (get-in element [:_id])) _nodes)
          existingnodeids (map (fn [element] (get-in element [:_id])) (mc/find-maps mongoconnection ncoll {:_id {$in nodeids}}))
          newnodeids (clojure.set/difference (set nodeids) (set existingnodeids))
          newnodeobjs  (remove nil? (map (fn [element] (if (contains? newnodeids (get-in element [:_id])) element)) _nodes))
          returnnodes (if (and (not (empty? newnodeobjs)) (mc/insert-batch mongoconnection ncoll newnodeobjs))
                        true
                        false)
          tabids (map (fn [element] (get-in element [:_id])) _tabs)
          existingtabids (map (fn [element] (get-in element [:_id])) (mc/find-maps mongoconnection tcoll {:_id {$in tabids}}))
          newtabids (clojure.set/difference (set tabids) (set existingtabids))
          newtabobjs  (remove nil? (map (fn [element] (if (contains? newtabids (get-in element [:_id])) element)) _tabs))
          returntabs (if (and (not (empty? newtabobjs)) (mc/insert-batch mongoconnection tcoll newtabobjs))
                        true
                        false)]
          ; returntabs (if (mc/insert-batch mongoconnection tcoll _tabs)
          ;               true
          ;               false)]
          ; returnnodes (if (mc/insert-batch mongoconnection ncoll _nodes)
          ;               true
          ;               false)]

          (if (and (= true returnnodes) (= true returntabs))
              {
                :tabs _tabs
                :nodes _nodes
              }
              false)
  )
)











;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;
;; ******************************* BELOW ARE FUNCTIONS FOR AN APP CALLED PARROT ****************************** ;;
;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;
;; ************** EVERYTHING IS IN THE SAME REPO BECAUSE aROUND IS AN INPUT MECHANISM FOR PARROT ************* ;;
;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;







;; FIRST checks if there is present a userobject in the user mongo collection that matches in id;;
;; IF there is, then return that userobject
;; ELSE there is not, then create an empty user object prepopulated with just a category history and MAYBE SOME STRANGERS????
(defn checkOrInsertUser [userobject]
  (let [uobject userobject
        ucoll "users"]

      (do 
        (if (mc/any? mongoconnection ucoll {:_id (get-in uobject [:id])})
            (mc/find-one-as-map mongoconnection ucoll {:_id (get-in uobject [:id])})
            (mc/insert-and-return mongoconnection ucoll {:_id (get-in uobject [:id]) 
                                            :name (get-in uobject [:name]) 
                                            :picture (get-in uobject [:picture])
                                            :entitytype "user"
                                            :questionsauthored [] 
                                            :questionsanswered []
                                            :strangerlikes []
                                            :strangerfriends []
                                            :categoryhistory ["training" "math" "dating" "commercial" "personal" "disney" "WORLD"]}))

        ; (mc/update mongoconnection ucoll {:_id useridinput} {:upsert true});;;;
        ; (mc/update mongoconnection ucoll {useridinput "{}"} {$set {:questions-authored "[]" :questions-ansered "[]"}} {:upsert true})
      )))


;; Generic method to query a mongodb collection in batch with an array of ids
(defn getEntityBatchById [entityarrayofids entitytype]
  (let [earray entityarrayofids 
        etype entitytype]

        (mc/find-maps mongoconnection etype {:_id {$in earray}})
    ))



;; QUERIES the "questions" mongo collection for all id's in an array of question ids
;; RETURNS an array of question objects that match ids with that array
(defn getQuestionBatchById [questionarrayofids]
  (let [qarray questionarrayofids
        qcoll "questions"]

        (mc/find-maps mongoconnection qcoll {:_id {$in qarray}})
    ))


;; Generic method to query a mongodb collection in batch with an array of ids
(defn getQuestionBatchByAuthorId [authorid]
  (let [aid authorid
        qcoll "questions"]

        (mc/find-maps mongoconnection qcoll {:author aid})
    ))


;; FIRST queries the database for the like or friend object by id
;; IF it finds something, then returns it
;; ELSE it creates a new empty object
(defn getEntity [entityobject entitytype]
  (let [eobject entityobject
        eid (or
              (get-in eobject [:id])
              (get-in eobject [:_id]))
        typecoll entitytype]

        (do
            (if (mc/any? mongoconnection typecoll {:_id eid})
                (mc/find-one-as-map mongoconnection typecoll {:_id eid})     
                {
                  :id
                  (get-in eobject [:id])

                  :name
                  (get-in eobject [:name])

                  :picture
                  (get-in eobject [:picture])

                  :questionsanswered
                  []

                  :questionsauthored
                  []
                }
            ))))


;; INPUT is a question object with all the appropriate properties (NEED TO ADD VALIDATION)
;; QUERIES the database in multiple place input order to add the question object to the mongo questions collections
;; QUERIES also add the question id to the appropriate questions answered and questions authord propoerties
;; ******* a question authored is automatically made a question answered for the user who created the question
;; INSERTS and SORt also occurs with the category(ies) that the question has been tagged by
(defn insertQuestionAuthored [payload]
  (let [payloadinput payload
        uid (payloadinput :author)
        qcoll "questions"
        ucoll "users"
        ccoll "categories"
        qobject (mc/insert-and-return mongoconnection qcoll {
                                                :author (get-in payloadinput [:author])
                                                :author_type (get-in payloadinput [:author_type])
                                                :categories (get-in payloadinput [:categories])
                                                :correct_answer (get-in payloadinput [:correct_answer])
                                                :options (get-in payloadinput [:options])
                                                :question (get-in payloadinput [:question])
                                                :timestamp (System/currentTimeMillis)
                                                })
        qid (qobject :_id)]

        (do 
            ;; INSERT FOR USER'S "QUESTIONS-AUTHORED" CHILD BLOCK
            (mc/update mongoconnection ucoll {:_id uid} {$push {:questionsauthored qid}})

            ;; INSERT FOR USER'S "QUESTIONS-ANSWERED" CHILD BLOCK TOO - IF THEY ASKED THEN WE COUNT IT AS AN ANSWER TOO
            (mc/update mongoconnection ucoll {:_id uid} {$push {:questionsanswered qid}})


            ;; INSERT INTO CATEGORIES MONGO COLLECTION
            (mc/insert-batch mongoconnection ccoll 
              (filter identity 
                (map (fn [element] ;; validate whether they category is present before insert
                        (if (not (mc/any? mongoconnection ccoll {:_id element}))
                            {:_id element}
                        )
                     )
                     (get-in payloadinput [:categories]) ;; iterating over array of category strings
                )
              )
            )

            ;; RETURN THE QUESTION OBJECT
            qobject
        )
    ))


;; UPDATES query result for user id by reordering (adding) the array of category state history
;; INPUT IS A USERID AND AN ARRAY OF CATEGORY TITLE STRINGS TO REARRANGE (UNSHIFT TO TOP POSITION)
;; RETURNS AN ARRAY OF CATEGORY TITLE STRINGS WHICH IS SORTED APPROPRIATELY
(defn updateCategoryHistory [userid questioncategories]
  (let [uid userid
        qcat questioncategories
        ucoll "users"]

        (do 
          ;; THIS BLOCK BASICALLY CUTS LIKE A DECK OF CARDS
          (mc/update mongoconnection ucoll {:_id uid} {$pullAll {:categoryhistory qcat}})
          (mc/update mongoconnection ucoll {:_id uid} {$pushAll {:categoryhistory qcat}})
          (mc/find-maps mongoconnection ucoll {:_id uid} ["categoryhistory"])
        )
  )
)

;; simple crud Batch find by the cateogry property
(defn getQuestionsByCategory [categoryarrayofsearchstrings]
  (let [carrayofsearchstrings categoryarrayofsearchstrings
        qcoll "questions"]
        
        (mc/find-maps mongoconnection qcoll {:categories {$in carrayofsearchstrings}})
    )
)

;; simple crud Batch find by an array of cateogry strings
;; RETURN can be multiple question objects
(defn getCategorySearch [searchstringarray]
  (let [ssarray searchstringarray
        ccoll "categories"
        returnarray []]

        (do
          (distinct
            (flatten
              (map (fn [outerelement]
                       (map (fn [innerelement]
                                (get-in innerelement [:_id]))
                            (mc/find-maps mongoconnection ccoll {:_id {$regex (str ".*" outerelement ".*")}})
                       ))
                   ssarray
              );;
            )
          )
        )
    ))




















;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;
;; ******************************* BELOW ARE FUNCTIONS FOR AN APP CALLED PARROT ****************************** ;;
;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;
;; ************** EVERYTHING IS IN THE SAME REPO BECAUSE aROUND IS AN INPUT MECHANISM FOR PARROT ************* ;;
;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;
;; *********************************************************************************************************** ;;


; (let [uri               mongo-uri
;       {:keys [conn db]} (mg/connect-via-uri uri)
;       coll "skittles"]
; ;   ; (mc/insert mongoconnection "skittles" { :_id (ObjectId.) :first_name "John" :last_name "Lennon" })
;     (mc/remove mongoconnection coll)
;       (mc/insert-and-return mongoconnection coll {:name "blue" :age 30}))



; ; inserts one word into the database
; (defn insertword [word color payload]
;   (let [wordinput word
;         colorinput color
;         payloadinput payload
;         uri mongo-uri
;           {:keys [conn db]} (mg/connect-via-uri uri)
;         coll "skittles"]

;         ; (binding [*out* *err*]
;         ;             (println (type {:word wordinput :color colorinput :payload payloadinput})))
;     (mc/insert-and-return mongoconnection coll {:word wordinput :color colorinput :payload payloadinput})      
;   )
; )


; (defn insertPayload [payload]
;   (let [payloadinput payload
;         uri mongo-uri
;           {:keys [conn db]} (mg/connect-via-uri uri)
;         qcoll "questions"]

;           (mc/insert-and-return mongoconnection qcoll {:payload payloadinput})
;     ))

; (defn acceptpayload [payload]
;   (let [payloadinput payload
;         uri mongo-uri
;           {:keys [conn db]} (mg/connect-via-uri uri)
;         coll "skittles"]

;     (mc/insert-and-return mongoconnection coll {:payload payloadinput})  
;     ))

; ; gets all from the database
; (defn getall []
;   (let [uri mongo-uri
;       		{:keys [conn db]} (mg/connect-via-uri uri)
;         coll "skittles"]

;     ; (mc/find mongoconnection coll {:name "blue"})	
;     (from-db-object (mc/find-maps mongoconnection coll { }) true)	
;   )
; )
