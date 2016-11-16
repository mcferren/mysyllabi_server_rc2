;; the top section includes all of the libraries
;; injected so that we can use their namespace

(ns recursiftion.model
  (:use joda-time)
  (refer-clojure :exclude [merge partial iterate format print contains? max min])
  (:require [clojure.string]
            [recursiftion.dao_graph :as dgraph]
            [recursiftion.dao_dictionary :as dao]
            [clojure.data.json :as json]
            [clojurewerkz.neocons.rest.records :as neorec]
            [clojure.pprint :refer [pprint]]
            [clojure.set]
            [joda-time.accessors :as ja])
  (:import [com.mongodb MongoOptions ServerAddress]
           [org.bson.types.ObjectId])
  )


(defn get-graph [limit]
  (let [_limit limit]

      (recursiftion.dao_graph/get-graph _limit)
  )
)



(defn tally-leaves-report [arrayarg idarg]
  (let [_arrayarg arrayarg
        _idarg idarg]


      (zipmap (map (fn [element]
                      (last (clojure.string/split element #"_")))
                   (set _arrayarg))
              (map (fn [element] 

                    ; (println "##################")
                    ; (println element)
                    ; (println _idarg)
                    ; (println "##################")

                      {
                          :count (dorun (recursiftion.dao_graph/get-leaf-array-length _idarg element))
                          :id element
                      }
                    )
                    (set _arrayarg)))
  )
)

(defn tally-tags-report [arrayarg idarg]
  (let [_arrayarg arrayarg
        _idarg idarg]

      (zipmap (set _arrayarg)
              (map (fn [element] 
                      (count (filter #{element} _arrayarg))
                    )
              (set _arrayarg)))
  )
)


(defn process-node [nodeobj]
  (let [_nodeobj (first nodeobj)]

        { 
            :id              (get-in _nodeobj ["id"])
            :name            (get-in _nodeobj ["name"])
            :type            (clojure.string/lower-case (get-in _nodeobj ["type"]))
            :color           (get-in _nodeobj ["color"])
            :heightstate     (get-in _nodeobj ["heightstate"])
            :background      (get-in _nodeobj ["background"])
            :auxtabs         (get-in _nodeobj ["auxtabs"])
            :subscribers     (get-in _nodeobj ["subscribers"])
            :instantiations  (get-in _nodeobj ["instantiations"])
            :leaves          (get-in _nodeobj ["leaves"])
            ; :leaves          (tally-leaves-report (get-in _nodeobj ["leaves"]) (get-in _nodeobj ["id"]))
            :tags            (tally-tags-report (get-in _nodeobj ["tags"]) (get-in _nodeobj ["id"]))
        }
  )
)

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

(def our-formatter (formatter "yyyy/MM/dd"))




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


(defn createDefaultCalendarVacant [] {
  :_id           (str "HHHHHHHH-" (to-millis-from-epoch (date-time)))
  :name          ""
  :type          "vacant"
  :tags          {}
  :color         "blue"
  :background    ""
  :custom        ""
  :subscribers   []
  :auxtabs        []
})


(defn createDefaultCalendarStub [utcdatestring]
  (let [_utcdatestring utcdatestring]

    (println "huopla" (dateToString (date-time (read-string _utcdatestring))))
    (println "ria" utcdatestring)

      {
        :_id           (str "GGGGGGGGHOOLA-" (to-millis-from-epoch (date-time)))
        :name          (dateToString (date-time (read-string _utcdatestring)))
        :type          "stub"
        :tags          ()
        :color         "blue"
        :background    "https://upload.wikimedia.org/wikipedia/commons/thumb/3/39/Blank_Calendar_page_icon.svg/220px-Blank_Calendar_page_icon.svg.png"
        :custom        ""
        :subscribers   []
        :auxtabs        []
      }
))

(println "&&&&&&&&&&")
; (println "now" (print our-formatter (in-zone (date-time) (timezone :UTC)) ))


(println "now" (dateToString (local-date)))
(println "millisec" (to-millis-from-epoch (date-time)))
(println "MOBY" (date-time 1447372800000))


; (ja/day-of-month (in-zone (date-time) (timezone :UTC)) )

; (print our-formatter now)


; (def datastring "%s %s %s, %s")

(defn createDefaultCalendarTab [tabid vacantid stubid] 
  (let [_tabid tabid
        _vacantid vacantid
        _stubid stubid]

    (println "TIZZY @@@" tabid)
  
  {
    :_id                       _tabid
    :type                      "day"
    :heightstate               "dooper"
    :percentageheight          100
    :inventory                 [
        {
            :_id                       _vacantid
            :isFavorite                false
            :isOpen                    true
            :trueindex                 0
            :currenttab                "vacant"
            :currentdate               (to-millis-from-epoch (local-date))
            :style                     {
                                           :width    "250px"
                                           :height   "271px"
                                           :padding  "30px"
                                       }
        } {
            :_id                       _stubid
            :isFavorite                false
            :isOpen                    true
            :trueindex                 1
            :currenttab                "stub"
            :currentdate               (to-millis-from-epoch (local-date))
            :style                     {
                                           :width    "250px"
                                           :height   "271px"
                                           :padding  "30px"
                                       }
        }
    ] 
  }
))

(defn fetchInstanceBatch [instanceid argarray]
  (let [_instanceid instanceid
        _argarray argarray
        _lastarg (last argarray)
        _secondTOlastarg (if (not (empty? argarray))
                             (last (pop argarray))
                             (last argarray)
                         )]

        ;; IF ITS A CALENDAR DAY, FETCH (OR CREATE) NEW PERSISTENT DAY IN DATA STORE
        ; (if (not (nil? (String->Number _lastarg))) 
        ;   (let [_vacantobj (createDefaultCalendarVacant)
        ;         _stubobj (createDefaultCalendarStub _lastarg)]

        ;       (do (recursiftion.dao_dictionary/add-leaf-to-node _secondTOlastarg _lastarg (str _secondTOlastarg "_" _lastarg))
        ;           (recursiftion.dao_dictionary/create-node-tab-batch {
        ;             :batchobj {
        ;                 :nodes [ _vacantobj _stubobj ]
        ;                 :tabs [ (createDefaultCalendarTab (str _secondTOlastarg "_" _lastarg) (get-in _vacantobj [:_id]) (get-in _stubobj [:_id]))]
        ;             }
        ;           })))
        ; )
        
        (recursiftion.dao_dictionary/get-instance-batch _instanceid _argarray)
        ; {
        ;  :status 200
        ;  :headers {"Content-Type" "application/json"}
        ;  ; :body (process-node _response)
        ;  :body  _response
        ; }
  )
)


(defn get-node [nodeid]
  (let [_nodeid nodeid
        _response (recursiftion.dao_dictionary/get-node _nodeid)]

        {
         :status 200
         :headers {"Content-Type" "application/json"}
         ; :body (process-node _response)
         :body  _response
        }
  )
)



(defn get-tab [leafid] 
  (let [_leafid leafid
        _response (recursiftion.dao_dictionary/get-tab _leafid)
        _nodeidarray (map (fn [element] (get-in element [:_id])) (get-in (first _response) [:inventory]))
        _nodeobjarray (recursiftion.dao_dictionary/get-node-batch _nodeidarray)]

        (if (not (empty? _response))
            (do (println "first")
              {:status 200
               :headers {"Content-Type" "application/json"}
               :body { 
                :type            (get-in (first _response) [:type])
                :inventory       (get-in (first _response) [:inventory])
                :nodes           _nodeobjarray
                :heightstate     "dooper"
                :percentageheight  100
              }
            })
            (do (println "second")
                {:status 409
                 :body {:status "error"
                        :message "error trying to get-tab"}}
            )
        )
))



(defn fetchTabInstance [tabid tabprefix application_instance username]

    (let [tab_instance_response   (recursiftion.dao_dictionary/get-tab tabid)
          tab_master_response     (recursiftion.dao_dictionary/get-tab (str tabprefix "master"))
          tab_instance            (if (not (empty? tab_instance_response)) (first tab_instance_response) nil)
          tab_master              (if (not (empty? tab_master_response)) (first tab_master_response) nil)]

         {
            :application_tab_instance_obj tab_instance
            :application_tab_master_obj   tab_master
            :username_tab_instance_obj    tab_instance
            :username_tab_master_obj      tab_master
            :input_data {
                :user_tabid                    tabid
                :user_tabmasterid              (str tabprefix "master")
                :application_tabid             tabid
                :application_tabmasterid       (str tabprefix "master")
            }
         }
    )
)



; (defn get-tab-inventory [leafid]
;   (let [_leafid leafid
;         ; _splitarray (clojure.string/split _leafid #"_")
;         ; _nodeid (get _splitarray 0)
;         ; _leaftype (get _splitarray 1)


;         _response (recursiftion.dao_graph/get-tab-inventory _leafid)]

      
;         {:status 200
;          :headers {"Content-Type" "application/json"}
;          :body { 
;             :id              (get-in (first _response)  ["id"])
;             :type            (get-in (first _response)  ["type"])
;             :inventory       (get-in (first _response)  ["inventory"])
;             :nodes           (map (fn [element] (process-node element)) (get-in (first _response)  ["nodes"]))
;             :heightstate     "dooper"
;             :percentageheight  100
;           }
;         }
;   )
; )



(defn transferTab [tabpayload]
  (let [_tabobj           (get-in tabpayload [:tabobj])
        _tabidinformation (clojure.string/split (get-in _tabobj [:id]) #"_")
        _response (recursiftion.dao_graph/transfer-tab (nth _tabidinformation 0)
                                                       (nth _tabidinformation 1)
                                                       (nth _tabidinformation 2))]

        ;; first  username
        ;; second parentnodetitle
        ;; third  relationshiptype

        ; (println (clojure.string/split (get-in _tabobj [:id]) #"_"))
        ; (println (count (get-in _tabobj [:inventory])))

        ;; DELETE EXISTING RELATIONSHIPS OF THIS LABEL :id split
        ;; UNWIND new array of inventory objects

      _response
  )
)


(defn exchangeTab [tabpayload]
  (let [_tabpayload tabpayload
        _response (recursiftion.dao_graph/exchange-tab _tabpayload)]

        ; (println "$$$ dollarobj")
        ; (println (get-in _pointerobj [:tabid]))
        ; (println (get-in _pointerobj [:pointerobj]))

      _response
  )
)


(defn createTabBatch [tabpayload]
  (let [_tabpayload tabpayload
        _response (recursiftion.dao_dictionary/create-tab-batch _tabpayload)]

        ; (println "$$$ dollarobj")
        ; (println (get-in _pointerobj [:tabid]))
        ; (println (get-in _pointerobj [:pointerobj]))

      _response
  )
)


(defn createCalendarBatch [batchpayload]
  (let [_batchpayload batchpayload
        _response (recursiftion.dao_dictionary/create-node-tab-batch batchpayload)]

        (println "$$$ _batchpayload")
        (println "$$$ _response")
        ; (println (get-in _pointerobj [:pointerobj]))

      _response
  )
)


(defn processInheritance [inheritancepayload userid]
  (let [_historyadditions (get-in inheritancepayload [:history])]          

    (doseq [[key value] inheritancepayload ] 

      (recursiftion.dao_dictionary/unshift-node-batch { 
          :tabid (str userid "_" (clojure.core/name key) "_ALLRESOURCES")
          :batchobj value
          :shouldinstantiate false
      }))

))


(defn decrementLeafCount [payload]
  (let [_payload payload
        _response (recursiftion.dao_dictionary/decrement-leaf-count _payload)]

        _response
))


(defn editNodeFromPointer [payload]
  (let [_payload payload
        _response (recursiftion.dao_dictionary/edit-node-from-pointer _payload)]

        _response
))


(defn unshiftNodeBatch [decrementpayload]
  (let [_decrementpayload decrementpayload
        _response (recursiftion.dao_dictionary/unshift-node-batch _decrementpayload)]

        (println "@@@ BOOTISE" _decrementpayload)

      _response
  )
)

(defn updateTab [tabpayload]
  (let [_tabpayload tabpayload
        _response (recursiftion.dao_dictionary/update-tab _tabpayload)]

        _response

  )
)

(defn updateCurrentTab [pointerpayload]
  (let [_pointerpayload pointerpayload
        _response (recursiftion.dao_dictionary/update-current-tab pointerpayload)]

        _response

  )
)

(defn updateCurrentDate [pointerpayload]
  (let [_pointerpayload pointerpayload
        _response (recursiftion.dao_dictionary/update-current-date pointerpayload)]

        _response

  )
)

(defn updateIsOpen [pointerpayload]
  (let [_pointerpayload pointerpayload
        _response (recursiftion.dao_dictionary/update-is-open pointerpayload)]

        _response

  )
)

(defn updateIsFavorite [pointerpayload]
  (let [_pointerpayload pointerpayload
        _response (recursiftion.dao_dictionary/update-is-favorite pointerpayload)]

        _response

  )
)

(defn replaceNodeAtIndexBatch [tabpayload]
  (let [_tabpayload tabpayload
        _response (recursiftion.dao_dictionary/replace-node-at-index-batch _tabpayload)]

        ; (println _tabpayload)

      _response
  )
)







(defn pushTab [tabpayload]
  (let [_tabpayload tabpayload
        _parentid (get-in _tabpayload [:tabid])
        _pointerindex (get-in _tabpayload [:pointerindex])
        _pointerobj (get-in _tabpayload [:pointerobj])
        _response (recursiftion.dao_graph/push-tab _pointerobj _pointerindex _parentid)]

        ; (println "$$$ dollarobj")
        ; (println (get-in _pointerobj [:tabid]))
        ; (println (get-in _pointerobj [:pointerobj]))

      _response
  )
)


(defn pushBatch [tabpayload]
  (let [_tabpayload tabpayload
        _response (recursiftion.dao_dictionary/push-batch _tabpayload)]

        ; (println _tabpayload)

      _response
  )
)



(defn createNode [nodepayload]
  (let [_nodeobj (get-in nodepayload [:nodeobj])
        _response (recursiftion.dao_graph/create-node _nodeobj)]

      _response
  )
)

; ;; simle crud insert request
; (defn acceptQuestionAuthored [payload]
;   (let [payloadinput payload]

;       (recursiftion.dao_dictionary/insertQuestionAuthored payloadinput)
;   )
; )


; ;; METHOD THAT ACCEPTS AN ARRAY OF STRINGS AND USES THEM TO 
; ;; SEARCH FOR CATGORIES IN MONG BASED ON REGEX EXPRESSIONS
; (defn searchCategories [payload]
;   (let [payloadinput payload]
;     (do 

;       ; (binding [*out* *err*]
;       ;     (println payloadinput))
;       (recursiftion.dao_dictionary/getCategorySearch payloadinput)
;     )
;   )
; )


; ;; utility method to filter out each friend's questions array and each like's question array
; ;; and returns an object in identical format to the input arrayofobjects argument
; ;;  -> though the return object now also includes filtered array properties for 
; ;; :questionsanswered and :questionsauthored
; (defn addQuestionsToObjectArray [userobject arrayofobjects entitytype]
;   (let [affiliateidlist arrayofobjects ;; what we map over
;         uobject userobject
;         uid (get-in uobject [:id])
;         etype entitytype ;; to find the right mongo collection
;         userarrayofansweredquestionsids (set (get-in uobject [:questionsanswered]))] 
;         ;; to filter qanswered and qauthored for each entityobject

;       (map (fn [element];;lspobobmarley

;                 (let [entityobject (recursiftion.dao_dictionary/getEntity element etype)
;                       entityarrayofansweredqids (set (get-in entityobject [:questionsanswered]))
;                       entityarrayofauthoredqids (set (get-in entityobject [:questionsauthored]))]

;                     { :id
;                       (get-in element [:id])

;                       :name
;                       (get-in element [:name])

;                       :picture
;                       (get-in element [:picture])


;                       ;; ****** FILTER -IDS- FOR EACH FRIEND/LIKE'S QUESTION ANSWERED
;                       :questionsanswered
;                       (clojure.set/difference entityarrayofansweredqids userarrayofansweredquestionsids);;
;                         ;; MAYBE ADD SOME SORT OF LIMIT TO THE OUTPUT QUANTITY?


;                       ;; ****** FILTER -IDS- FOR EACH FRIEND/LIKE'S QUESTION AUTHORED
;                       :questionsauthored
;                       (clojure.set/difference entityarrayofauthoredqids userarrayofansweredquestionsids)
;                         ;; MAYBE ADD SOME SORT OF LIMIT TO THE OUTPUT QUANTITY?
;                     }
;                 )
;             )

;       affiliateidlist)
;   ))


; ;; utility method that fetches the question objects referenced 
; ;; in user, friend, & business like objects. it extracts these 
; ;; arrays out of each object and concats them all together

; ;; likes and friends are passed instead of userobject because we don't want to store fb info in the db 
; ;; payload has no like/friend questions and userobject has no friend/likes
; (defn getQuestionArrayofObjects [likesandfriendsobjects userquestiontofilterwith]   
;     (let [arrayoflikesandfriendsids (map (fn [element] (concat (get-in element [:questionsanswered]) 
;                                                                (get-in element [:questionsauthored]))
;                                          ) 
;                                          likesandfriendsobjects)
;           arrayofuserqansweredquestionids userquestiontofilterwith
;           concatlikesandfriends  (distinct (flatten arrayoflikesandfriendsids))
;           ]

;           (concat
;           ;     ;; this query output is an array of question objects from  
;           ;     ;; each friend and like object by gathering their
;           ;     ;; question answered ids and question authored ids
;               (recursiftion.dao_dictionary/getQuestionBatchById arrayofuserqansweredquestionids)


;           ;     ;; this query output is an array of question objects from  
;           ;     ;; the users list of answered and authord
;               (recursiftion.dao_dictionary/getQuestionBatchById concatlikesandfriends)
;           ;     ;; HERE WE DON'T NEED TO FILTER BECAUSE THEY HAVE ALREADY BEEN 
;           ;     ;; FILTERED EARLIER IN THE addQuestionsToObjectArray METHOD
;           ;     ;; JUST FLATTEN AND GET DISTINCT
;           )))




; ;; PREVIOUS attempt to use the same method  --  may need to revisit
; ;; INPUT is an array of friends or likes as well as a list of ids to compare with and contrast against
; ;; OUTPUT is an array of profile objects that have their questionsanswered and questions authored arrays' refined by the supplied criteria
; (defn FUNNYaddQuestionsToObjectArray [comparearrayofids notcomparearrayofids arrayofobjects entitytype]
;   (let [affiliateidlist arrayofobjects ;; what we map over
;         etype entitytype ;; to find the right mongo collection
;         carray (set comparearrayofids)
;         notcarray (set notcomparearrayofids)] 

;       (map (fn [element] 

;                 (let [entityobject (recursiftion.dao_dictionary/getEntity element etype)
;                       entityarrayofansweredqids (set (get-in entityobject [:questionsanswered]))
;                       entityarrayofauthoredqids (set (get-in entityobject [:questionsauthored]))]

;                     { :id
;                       (or
;                         (get-in element [:id]) ;; nice clojure feature to stay nimble amongst object formatting received
;                         (get-in element [:_id]))

;                       :name
;                       (get-in element [:name])

;                       :picture
;                       (get-in element [:picture])


;                       ;; ****** FILTER -IDS- FOR EACH FRIEND/LIKE'S QUESTION ANSWERED
;                       :questionsanswered
;                       (clojure.set/intersection
;                           (set (filter (fn [element] (not (contains? (set carray) (str element)))) 

;                               entityarrayofansweredqids))

;                           notcarray
;                       ) ;; MAYBE ADD SOME SORT OF LIMIT TO THE OUTPUT QUANTITY?


;                       ;; ****** FILTER -IDS- FOR EACH FRIEND/LIKE'S QUESTION AUTHORED
;                       :questionsauthored
;                       (clojure.set/intersection
;                           (set (filter (fn [element] (not (contains? (set carray) (str element)))) 

;                               entityarrayofauthoredqids))
;                           notcarray
;                       ) ;; MAYBE ADD SOME SORT OF LIMIT TO THE OUTPUT QUANTITY?
;                     }
;                 )
;             )

;       affiliateidlist)
;   ))  


; ;; METHOD THAT GETS QUESTIONS FIRST (BY CATEGORY) AND THEN 
; ;; FINDS PROFILES LAST (AND ORDER BY isFRIEND?)
; ;; RETURNS a new userobject to the client
; (defn acceptCategory [payload]
;   (let [payloadobject payload

;         ;; parse payload object received and convert appropriately to objects from the ids interpretted
;         uobject (get-in payload [:userobject])
;         categoryarrayofsearchstrings (get-in payload [:categoryarrayofsearchstrings])
;         categoryarrayofhistoryobject (recursiftion.dao_dictionary/updateCategoryHistory (get-in uobject [:id]) categoryarrayofsearchstrings)
;         categoryqueryobjects (set (recursiftion.dao_dictionary/getQuestionsByCategory categoryarrayofsearchstrings)) ;; set of questions found by category string refined by those the user has already answered
        
;         ;; generate and array of question that have been tagged by the search category string and also unseen by the user
;         questionarrayofusersanswersandauthoredids (set (distinct (concat (get-in uobject [:questionsanswered]) (get-in uobject [:questionsauthored])))) ;;realize this is redundant (bc a user/like auto-answers any question they author) ;; but it does prepare us for future enhancements
;         unseenquestionarrayofcategoryqueryobjects (filter (fn [element] (not (contains? questionarrayofusersanswersandauthoredids (str (get-in element [:_id]))))) categoryqueryobjects)
;         unseenquestionarrayofcategoryqueryids (map (fn [element] (get-in element [:_id])) unseenquestionarrayofcategoryqueryobjects)

;         ;; accumulate a list of profile affiliations by iterating through the author property of each question object in the array
;         likearrayofids (set (map (fn [element] (get-in element [:id])) (get-in uobject [:likes]))) ;; list of like id's;;
;         friendsarrayofids (set (map (fn [element] (get-in element [:id])) (get-in uobject [:friends]))) ;; list of friend id's

;         ;; cut up the array --  make the ones by strangers in a tuple --  wish I could iterate just once for this...
;         likeswithcatquestionsarrayofids (distinct (map (fn [element] (get-in element [:author])) (filter (fn [element] (contains? likearrayofids (str (get-in element [:author])))) unseenquestionarrayofcategoryqueryobjects)))
;         friendswithcatquestionsarrayofids (distinct (map (fn [element] (get-in element [:author])) (filter (fn [element] (contains? friendsarrayofids (str (get-in element [:author])))) unseenquestionarrayofcategoryqueryobjects)))
;         strangerswithcatquestionsarrayoftuples (distinct (map (fn [element] {:id (get-in element [:author]) :type (get-in element [:author_type])}) (filter (fn [element] (not (contains? (set (concat likearrayofids friendsarrayofids)) (str (get-in element [:author]))))) unseenquestionarrayofcategoryqueryobjects)))

;         ;; convert what has been cut up into seperate arrays of profile objects
;         likeswithcatquestionsarrayofobjects (filter (fn [element] (contains? (set likeswithcatquestionsarrayofids) (str (get-in element [:id])))) (get-in uobject [:likes]))
;         friendswithcatquestionsarrayofobjects (filter (fn [element] (contains? (set friendswithcatquestionsarrayofids) (str (get-in element [:id])))) (get-in uobject [:likes]))
;         strangerlikeswithcatquestionsarrayofobjects   (filter (fn [element] (= "business" (str (get-in element [:type])))) strangerswithcatquestionsarrayoftuples)
;         strangerfriendswithcatquestionsarrayofobjects (filter (fn [element] (= "user"     (str (get-in element [:type])))) strangerswithcatquestionsarrayoftuples)

;         ;; must filter each profile so that question ids listed in their qauthored and qanswered arrays have not been seen by the user
;         filteredlikeswithcatquestionsarrayofobjects (FUNNYaddQuestionsToObjectArray questionarrayofusersanswersandauthoredids unseenquestionarrayofcategoryqueryids likeswithcatquestionsarrayofobjects "businesses")
;         filteredfriendswithcatquestionsarrayofobjects (FUNNYaddQuestionsToObjectArray questionarrayofusersanswersandauthoredids unseenquestionarrayofcategoryqueryids friendswithcatquestionsarrayofobjects "users")
        
;         ; query the tuples to convert to lists of objects
;         prefilteredstrangerlikeswithcatquestionsarrayofobjects (recursiftion.dao_dictionary/getEntityBatchById (map (fn [element] (get-in element [:id])) strangerlikeswithcatquestionsarrayofobjects) "businesses")
;         prefilteredstrangerfriendswithcatquestionsarrayofobjects (recursiftion.dao_dictionary/getEntityBatchById (map (fn [element] (get-in element [:id])) strangerfriendswithcatquestionsarrayofobjects) "users")
        
;         ;; must filter each profile so that question ids listed in their qauthored and qanswered arrays have not been seen by the user
;         filteredstrangerlikeswithcatquestionsarrayofobjects (FUNNYaddQuestionsToObjectArray questionarrayofusersanswersandauthoredids unseenquestionarrayofcategoryqueryids prefilteredstrangerlikeswithcatquestionsarrayofobjects "businesses")
;         filteredstrangerfriendswithcatquestionsarrayofobjects (FUNNYaddQuestionsToObjectArray questionarrayofusersanswersandauthoredids unseenquestionarrayofcategoryqueryids prefilteredstrangerfriendswithcatquestionsarrayofobjects "users")
        
;         ;; query all the questions that the user has authored and concatenate it with the category-tagged-questions we have learned that the user hasn't seen
;         questionarrayofusersanswersandauthoredobjects (recursiftion.dao_dictionary/getQuestionBatchByAuthorId (get-in uobject [:id]))
;         filteredquestionarrayofobjects (concat unseenquestionarrayofcategoryqueryobjects questionarrayofusersanswersandauthoredobjects)
;         ]

;         (do
;             {
;               :id (get-in uobject [:id])

;               :name (get-in uobject [:name])

;               :questionsanswered (get-in uobject [:questionsanswered]) ;; THIS COULD USE SOME WORK TO CUT THE DECK AND PUT THE CATEGORY AFFILIATED QUESTIONS ONTOP

;               :questionsauthored (get-in uobject [:questionsauthored])

;               :categoryhistory categoryarrayofhistoryobject

;               :questionobjects filteredquestionarrayofobjects ;; STILL NEED TO GET THE QUESTION OBJECTS THAT THE USER HAS ANSWERED

;               :likes filteredlikeswithcatquestionsarrayofobjects

;               :friends filteredfriendswithcatquestionsarrayofobjects

;               :strangerlikes filteredstrangerlikeswithcatquestionsarrayofobjects

;               :strangerfriends filteredstrangerfriendswithcatquestionsarrayofobjects;;
;             }
;         )
;     )) 


; ;; converts and input payload object and returns it after adding 
; ;; property arrays for questions answered, questions authored, 
; ;; category history & questionobjects. 
; ;; RETURNS WHAT THE CLIENT WILL TOGGLE AS THE "USER_OBJECT"
; (defn acceptUser [payload]
;   (let [payloadinput payload
;         userobject (recursiftion.dao_dictionary/checkOrInsertUser payloadinput)
;         ;; likes and friends are passed into userobject because we don't want to store fb info in the db 
;         ;; payload has no like/friend questions and userobject has no friend/likes 

;         ;; have to filter w/in method because question arrays are stashed 
;         likearrayoffilteredobjects (addQuestionsToObjectArray userobject (get-in payloadinput [:likes :data]) "businesses") 
;         friendarrayoffilteredobjects (addQuestionsToObjectArray userobject (get-in payloadinput [:friends :data]) "users") 
;         ;; within the entity object that we map over for each friend and like

;         ;; filter early so we can polmorphicly map over the array inside the getQuestionArrayofObjects method
;         likesandfriendarrayoffilteredobjects (concat likearrayoffilteredobjects friendarrayoffilteredobjects)
;         questionarrayofids (distinct (concat (get-in userobject [:questionsanswered]) (get-in userobject [:questionsauthored])))
;         ;; gather all questions answered/authored from user object and format according to the getQuestionArrayofObjects parameter require
        
;         ;; returned are all the questions objects (accounted for by id above) -> each are an element we store as questionobjects property below
;         questionarrayoffilteredobjects (getQuestionArrayofObjects likesandfriendarrayoffilteredobjects questionarrayofids)
;         ]
  
;       (do
;           {

;               :id (get-in payloadinput [:id]);;mncfnneme

;               :name (get-in payloadinput [:name])

;               :email (get-in payloadinput [:email])

;               :friends (get-in payloadinput [:friends])

;               :likes (get-in payloadinput [:likes])

;               :categoryhistory ["mart" "math" "dating" "commercial" "personal" "disney" "WORLD"]

;               :questionsanswered []

;               :questionsauthored []

;               :browsehistory []

;               :answeredhistory []

;               :recordedanswers []
;               ;; :recordedanswers ["35" "San Francisco" "Programming" "Caucasion"]

;               :loggedin "1";;;

;               :strangerfriends []

;               :strangerlikes [];;;msmavermmelel;;

;               :questionobjects []
;           }
;       )
;       ; (do ;; ALL THIS METHOD BODY SHOULD JUST DO IS ASSEMBLE THE RETURN OBJECT 
;       ;     ;; THAT THE CONTROLLER WILL RELAY TO THE CLIENT
;       ;     {
;       ;       :id (get-in payloadinput [:id])

;       ;       :name (get-in payloadinput [:name])

;       ;       :questionsanswered (get-in userobject [:questionsanswered])

;       ;       :questionsauthored (get-in userobject [:questionsauthored])

;       ;       :categoryhistory (get-in userobject [:categoryhistory])

;       ;       :likes likearrayoffilteredobjects

;       ;       :friends friendarrayoffilteredobjects

;       ;       :questionobjects questionarrayoffilteredobjects

;       ;       :strangerlikes []

;       ;       :strangerfriends []
;       ;     }
;       ; )

; ))





; ;; converts and input payload object and returns it after adding 
; ;; property arrays for questions answered, questions authored, 
; ;; category history & questionobjects. 
; ;; RETURNS WHAT THE CLIENT WILL TOGGLE AS THE "USER_OBJECT"
; (defn facebookAcceptUser [payload]
;   (let [payloadinput payload]
  
;       (do
;           {
;               :id (get-in payloadinput [:id])

;               :name (get-in payloadinput [:name])

;               :email (get-in payloadinput [:email])

;               :friends (get-in payloadinput [:friends])

;               :likes (get-in payloadinput [:likes])

;               :categoryhistory ["mart" "math" "dating" "commercial" "personal" "disney" "WORLD"]

;               :questionsanswered []

;               :questionsauthored []

;               :browsehistory []

;               :answeredhistory []

;               :recordedanswers ["35" "San Francisco" "Programming" "Caucasion"]

;               :loggedin "0"

;               :strangerfriends []

;               :strangerlikes []

;               :questionobjects []
;           }
;       )
; ))

















; ;; *********************************************************************************************************** ;;
; ;; *********************************************************************************************************** ;;
; ;; *********************************************************************************************************** ;;
; ;; *********************************************************************************************************** ;;
; ;; *********************************************************************************************************** ;;
; ;; ******************************* BELOW ARE FUNCTIONS FOR AN APP CALLED PARROT ****************************** ;;
; ;; *********************************************************************************************************** ;;
; ;; *********************************************************************************************************** ;;
; ;; *********************************************************************************************************** ;;
; ;; ************** EVERYTHING IS IN THE SAME REPO BECAUSE aROUND IS AN INPUT MECHANISM FOR PARROT ************* ;;
; ;; *********************************************************************************************************** ;;
; ;; *********************************************************************************************************** ;;
; ;; *********************************************************************************************************** ;;
; ;; *********************************************************************************************************** ;;
; ;; *********************************************************************************************************** ;;




; ; (def pounce (recursiftion.dao_dictionary))

; ;; define a starter map object

; (def dictionary {"apple" {
;                     "noun" {
;                             "defins" ["A crisp fruit"
;                                      "Food from a tree"
;                                      "Can peel before eating"]
;                             "synons" [""]
;                             "tally" ["I like apple" "Sometimes I like to eat a green apple"] ;; each element is a get function returning from wordString / defins / index
;                             }
;                           }
;                  "smoke" {
;                     "verb" {
;                            "defins" [""]
;                            "synons" [""]
;                            "tally" [""] ;; each element is a get function returning from wordString / defins / index
;                            }
;                     "noun" {
;                            "defins" [""]
;                            "synons" [""]
;                            "tally" [""] ;; each element is a get function returning from wordString / defins / index
;                            }
;                          }
;                  "pear" {
;                     "noun" {
;                            "defins" [""]
;                            "synons" [""]
;                            "tally" [""] ;; each element is a get function returning from wordString / defins / index
;                            }
;                         }
;                 })


; ;; function for gaining access to dictionary map object

; (defn all []
;   dictionary)



; ; to test out crud operations
; (defn insertdb [word color payload]
;     (let [wordinput word
;           colorinput color
;           payloadinput payload]

;       (recursiftion.dao_dictionary/insertword wordinput colorinput payloadinput)     
;   )
; )


; (defn acceptdata [payload]
;   (let [payloadinput payload]
;       ; (do 
;       ;   (binding [*out* *err*]
;       ;       (println (type payload)))
;       ;       ; (with-pprint-dispatch *code-dispatch* (pprint payload)))
;       ;     ; payload
;       ;     ; payload
;       ;   )
;     ; payloadinput

;     ; (slurp (type (json/read-str payload)))
;     ; (json/read-str "{\"a\":1,\"b\":2}")
;     (recursiftion.dao_dictionary/insertPayload payloadinput)
;   )
; )


; (defn getrequesting []
;     (let [returnmap (recursiftion.dao_dictionary/getall)]
;       ; (do (binding [*out* *err*]
;       ;       (println "Goodbye, world!"))
;       ;     returnmap
;       ;   )
;           returnmap
        
;   ) 
; )




; ;; this function adds a new word map object to the dictionary

; (defn pushnewword [word pos defins synons]
;   (let [wordinput word
;         posinput pos
;         definsinput defins
;         synonsinput synons]
;     (def dictionary
;       (conj
;         {wordinput  {
;              posinput {
;                       "defins" [definsinput]
;                       "synons" [synonsinput]
;                       "tally" [""]
;                       }
;                     }
;         }
;         dictionary ))
;   )
; )



; ;; this function is for a scenario when only a word string is offered
; ;; it conditionally either creates a new word object or updates an
; ;; existing word object

; (defn updatenakedinput [word pos defins synons]

;   (let [wordinput word
;         posinput pos
;         definsinput defins
;         synonsinput synons
;         wordobject (dictionary word)
;         posobject ((dictionary word) pos)
;         existingpos (first (keys (dictionary wordinput)))
;         ]
;     (def dictionary ;; BAD BAD BAD BAD BAD - should be maintaining immutability everywhere
;       (conj  ;; if word exists and already has the same part of speech
;         {
;           wordinput  {;; if word exists but has no pos, then copy any existing definitions, synonems, and tallies into new word object that will replace the existing (naked) one

;                         existingpos {
;                           "defins" (if (= (first (((dictionary wordinput) existingpos) "defins")) "")
;                                       [definsinput]
;                                       (if (= definsinput "")
;                                           (((dictionary wordinput) existingpos) "defins")
;                                           (conj
;                                             (((dictionary wordinput) existingpos) "defins")
;                                             definsinput
;                                           )
;                                       )
;                                     )
;                           "synons" (if (= (first (((dictionary wordinput) existingpos) "synons")) "")
;                                      [synonsinput]
;                                      (if (= synonsinput "")
;                                          (((dictionary wordinput) existingpos) "synons")
;                                          (conj
;                                             (((dictionary wordinput) existingpos) "synons")
;                                             synonsinput
;                                          )
;                                      )
;                                    )
;                           "tally" (((dictionary wordinput) existingpos) "tally")
;                         }
;           }
;         }
;         (dissoc dictionary wordinput)
;       )
;     )
;   )
; )


; ;; this function is for a scenario when only a word string
; ;; and possibly other imputs are offered. It focuses on
; ;; updating an existing word object (specifically one that
; ;; was originall created without all properties populated)

; (defn updatenakedword [word pos defins synons]

;   (let [wordinput word
;         posinput pos
;         definsinput defins
;         synonsinput synons
;         wordobject (dictionary word)
;         posobject ((dictionary word) pos)
;         ]
;     (def dictionary ;; BAD BAD BAD BAD BAD - should be maintaining immutability everywhere
;       (conj  ;; if word exists and already has the same part of speech
;         {
;           wordinput  {;; if word exists but has no pos, then copy any existing definitions, synonems, and tallies into new word object that will replace the existing (naked) one

;                         posinput {
;                                   "defins" (if (= (first (((dictionary wordinput) "") "defins")) "")
;                                               [definsinput]
;                                               (if (= definsinput "")
;                                                   (((dictionary wordinput) "") "defins")
;                                                   (conj
;                                                     (((dictionary wordinput) "") "defins")
;                                                     definsinput
;                                                   )
;                                               )
;                                             )
;                                   "synons" (if (= (first (((dictionary wordinput) "") "synons")) "")
;                                              [synonsinput]
;                                              (if (= synonsinput "")
;                                                  (((dictionary wordinput) "") "synons")
;                                                  (conj
;                                                     (((dictionary wordinput) "") "synons")
;                                                     synonsinput
;                                                  )
;                                              )
;                                            )
;                                   "tally" (((dictionary wordinput) "") "tally")
;                                 }
;           }
;         }
;         (dissoc dictionary wordinput)
;       )
;     )
;   )
; )



; ;; this function is for a scenario when only a pos string
; ;; and possibly other inputs are offered. It focuses on
; ;; updating an existing word object (specifically one that
; ;; does not currently have a part of speech (nested) object
; ;; associated with the one given as an argument)

; (defn updatewordwithnewpos [word pos defins synons]

;   (let [wordinput word
;         posinput pos
;         definsinput defins
;         synonsinput synons
;         wordobject (dictionary word)
;         posobject ((dictionary word) pos)
;         ]

;     (def dictionary ;; BAD BAD BAD BAD BAD - should be maintaining immutability everywhere
;       (conj  ;; if word exists and already has the same part of speech
;         {wordinput
;               (conj
;                   { posinput {
;                           "defins" [definsinput]
;                           "synons" [synonsinput]
;                           "tally" [""]
;                           }
;                   }
;                   (dissoc wordobject posinput)
;                 )
;         }
;         (dissoc dictionary wordinput)
;       )
;     )
;   )
; )



; ;; this function is for a scenario when a pos string is not
; ;; offered. It focuses on updating an existing word object
; ;; (specifically one that currently has multiple parts of
; ;; speech objects

; (defn guessposandupdate [word pos defins synons]

;   (let [wordinput word
;         posinput pos
;         randompos (nth (keys (dictionary word))
;                        (rand-int (count (dictionary word)))
;                   )
;         definsinput defins
;         synonsinput synons
;         wordobject (dictionary word)
;         posobject ((dictionary word) randompos)
;         ]

;     (def dictionary ;; BAD BAD BAD BAD BAD - should be maintaining immutability everywhere
;       (conj  ;; if word exists and already has the same part of speech
;         {wordinput
;               (conj
;                   { randompos {
;                               "defins" (if (= (first (posobject "defins")) "")
;                                          [definsinput]
;                                          (if (= definsinput "")
;                                              (posobject "defins")
;                                              (conj
;                                                 (posobject "defins")
;                                                 definsinput
;                                              )
;                                          )
;                                        )
;                               "synons" (if (= (first (posobject "synons")) "")
;                                          [synonsinput]
;                                          (if (= synonsinput "")
;                                              (posobject "synons")
;                                              (conj
;                                                (posobject "synons")
;                                                synonsinput
;                                              )
;                                          )
;                                        )
;                               "tally" (posobject "tally")
;                               }
;                   }
;                   (dissoc wordobject randompos)
;                 )
;         }
;         (dissoc dictionary wordinput)
;       )
;     )
;   )
; )





; ;; this function is for a scenario when a word object
; ;; matches the word string and pos string input. It
; ;; appends the input sentences and synonems to the
; ;; correct place in that word object

; (defn updatepopulatedword [word pos defins synons]

;   (let [wordinput word
;         posinput pos
;         definsinput defins
;         synonsinput synons
;         wordobject (dictionary word)
;         posobject ((dictionary word) pos)
;         ]

;     (def dictionary ;; BAD BAD BAD BAD BAD - should be maintaining immutability everywhere
;       (conj  ;; if word exists and already has the same part of speech
;         {wordinput
;               (conj
;                   { posinput {
;                               "defins" (if (= (first (posobject "defins")) "")
;                                          [definsinput]
;                                          (if (= definsinput "")
;                                              (posobject "defins")
;                                              (conj
;                                                 (posobject "defins")
;                                                 definsinput
;                                              )
;                                          )
;                                        )
;                               "synons" (if (= (first (posobject "synons")) "")
;                                          [synonsinput]
;                                          (if (= synonsinput "")
;                                              (posobject "synons")
;                                              (conj
;                                                (posobject "synons")
;                                                synonsinput
;                                              )
;                                          )
;                                        )
;                               "tally" (posobject "tally")
;                                       ; (if (= (first (posobject "tally")) "")
;                                       ;    [definsinput]
;                                       ;    (conj
;                                       ;      (posobject "tally")
;                                       ;      definsinput
;                                       ;    )
;                                       ;  )
;                               }
;                   }
;                   (dissoc wordobject posinput)
;                 )
;         }
;         (dissoc dictionary wordinput)
;       )
;     )
;   )
; )



; ;; this function takes a word string as an argument
; ;; if the word exists in the dictionary then it returns
; ;; a word object; else it returns nil

; (defn retrieveword [id]
;   (let [wordstring id]
;     (if (nil? (dictionary wordstring))
;       nil
;       (dictionary wordstring))
;   )
; )



; ;; this function receives arguments from the controller and routes
; ;; to the correct functions mentioned above in order to create or
; ;; update existing word objects accordingly

; (defn acceptword [word pos defins synons]
;   (let [wordinput word
;         wordobject (retrieveword word)
;         posinput pos
;         definsinput (clojure.string/trimr
;                        (apply str
;                               (map #(str % " ")
;                                    (re-seq #"[a-zA-Z]+" defins))))
;         synonsinput synons]

;     (if (nil? wordobject)
;       (pushnewword wordinput posinput definsinput synonsinput) ;; means its a new word. just push it even if there are blank fields

;       (cond ;; means the word  already exists. pick which scenario

;           (and (= "" posinput) ;; means we are traversing sentence and don't know part of speach of the word we intend to inspect
;                (> (count wordobject) 1)) ;; means the word has multiple parts of speech
;             (guessposandupdate wordinput posinput definsinput synonsinput)


;           (and (= "" posinput) ;; means we are traversing sentence and don't know part of speach of the word we intend to inspect
;                (= (count wordobject) 1) ;; means the word has only one part of speech
;                (nil? (wordobject ""))) ;; means that the word's only part of speech is not an empty string
;             (updatenakedinput wordinput posinput definsinput synonsinput)


;           (and (= "" posinput) ;; means we are traversing sentence and don't know part of speach of the word we intend to inspect
;                (= (count wordobject) 1) ;; means the word has only one part of speech
;                (not (nil? (wordobject "")))) ;; means that the word's only part of speech is an empty string
;             (updatepopulatedword wordinput posinput definsinput synonsinput)


;           (and (not= "" posinput) ;; means that the input includes a value for part of speech
;                (= (count wordobject) 1) ;; means the word has only one part of speech
;                (not (nil? (wordobject ""))));; means that the word's only part of speech is an empty string
;             (updatenakedword wordinput posinput definsinput synonsinput)


;           (and (not= "" posinput) ;; means that the input includes a value for part of speech
;                (not (nil? (wordobject posinput))));; means that the pos already exists within the word object
;             (updatepopulatedword wordinput posinput definsinput synonsinput)


;           (and (not= "" posinput) ;; means that the input includes a value for part of speech
;                (nil? (wordobject posinput)));; means that the pos does not exists within the word object
;             (updatewordwithnewpos wordinput posinput definsinput synonsinput)

;           :else (updatepopulatedword wordinput posinput definsinput synonsinput))
;     )
;   )
; )




; ;; this function updates the tally of a word object
; ;; that may already has all of its properties populated
; ;; but it has multiple nested part of speech objects

; (defn guessword-incrementtally [word sentence]

;   (let [wordinput word
;         sentenceinput sentence
;         targetword (retrieveword word)
;         randompos (nth (keys (dictionary word))
;                        (rand-int (count (dictionary word)))
;                   )
;         posobject (targetword randompos)]

;         (def dictionary ;; BAD BAD BAD BAD BAD - should be maintaining immutability everywhere
;           (conj  ;; if word exists and already has the same part of speech
;             {wordinput
;                   (conj
;                       { randompos {
;                                   "defins" (posobject "defins")
;                                   "synons" (posobject "synons")
;                                   "tally" (if (= (first (posobject "tally")) "")
;                                              [sentenceinput]
;                                              (conj
;                                                 (posobject "tally")
;                                                 sentenceinput
;                                              )
;                                           )
;                                   }
;                       }
;                       (dissoc targetword randompos)
;                     )
;             }
;             (dissoc dictionary wordinput)
;           )
;         )
;   )
; )




; ;; this function updates the tally of a word object
; ;; that already has none of its properties populated

; (defn nakedword-incrementtally [word sentence]

;   (let [wordinput word
;         sentenceinput sentence
;         targetword (retrieveword word)
;         existingpos (first (keys (dictionary word)))
;         posobject (targetword existingpos)]

;         (def dictionary ;; BAD BAD BAD BAD BAD - should be maintaining immutability everywhere
;           (conj  ;; if word exists and already has the same part of speech
;             {wordinput
;                 { existingpos {
;                     "defins" (posobject "defins")
;                     "synons" (posobject "synons")
;                     "tally" (if (= (first (posobject "tally")) "")
;                                [sentenceinput]
;                                (conj
;                                   (posobject "tally")
;                                   sentenceinput
;                                )
;                             )
;                     }
;                 }
;             }
;             (dissoc dictionary wordinput)
;           )
;         )
;   )
; )


; ;; this function updates the tally of a word object
; ;; that already has all of its properties populated

; (defn populatedword-incrementtally [word sentence]

;   (let [wordinput word
;         sentenceinput sentence
;         targetword (retrieveword word)
;         existingpos (first (keys (dictionary word)))
;         posobject (targetword existingpos)]

;         (def dictionary ;; BAD BAD BAD BAD BAD - should be maintaining immutability everywhere
;           (conj  ;; if word exists and already has the same part of speech
;             {wordinput
;                 { existingpos {
;                     "defins" (posobject "defins")
;                     "synons" (posobject "synons")
;                     "tally" (if (= (first (posobject "tally")) "")
;                                [sentenceinput]
;                                (conj
;                                   (posobject "tally")
;                                   sentenceinput
;                                )
;                             )
;                     }
;                 }
;             }
;             (dissoc dictionary wordinput)
;           )
;         )
;   )
; )


; ;; After a definition sentence traversal has engaged, this function
; ;; analyzes what catefory a particular word falls into. it then routes
; ;; that word to the approproate function to update its tally counter

; (defn analyzetargetword [word sentence]

;   (let [wordinput word
;         sentenceinput sentence
;         targetword (retrieveword word)]

;     (cond

;         (> (count targetword) 1) ;; means the word has multiple parts of speech
;           (guessword-incrementtally wordinput sentenceinput)


;         (and (= (count targetword) 1) ;; means the word has only one part of speech
;              (nil? (targetword ""))) ;; means that the word's only part of speech is not an empty string
;           (nakedword-incrementtally wordinput sentenceinput)


;         (and (= (count targetword) 1) ;; means the word has only one part of speech
;              (not (nil? (targetword "")))) ;; means that the word's only part of speech is an empty string
;           (populatedword-incrementtally wordinput sentenceinput)
;     )

;   )
; )


; ;; this function establishes whether the wordobject already exists
; ;; when a definition sentence is being traversed. if not, it creates
; ;; the word without complementary properties. if so, then it further
; ;; triggers the mechanism/function to increase the word's tally property

; (defn routetotallyword [word sentence]

;   (let [wordinput word
;         sentenceinput sentence
;         targetword (dictionary word)]

;     (if (nil? (retrieveword wordinput))
;         (do (acceptword wordinput "" "" "")
;             (analyzetargetword wordinput sentenceinput)
;         )
;         (analyzetargetword wordinput sentenceinput)
;     )
;   )
; )



; ;; this function is triggered after a word is added to the dictionary
; ;; it traverses each word found in the submitted definition sentence
; ;; and it increases each word object's tally property

; (defn traversesentence [defins] ;; argument is a string

;   (let [trimmedsentence (clojure.string/trimr
;                           (apply str
;                              (map #(str % " ")
;                                 (re-seq #"[a-zA-Z]+" defins))))
;         sentencearray (clojure.string/split trimmedsentence #"\s")
;         justforkicks defins]

;     (dotimes [n (count sentencearray)]
;         (routetotallyword (sentencearray n) trimmedsentence))
;   )
; )



; ;; this function maps over each sentence in a word object's tally
; ;; and establishes a collection of counts. It then reduces in order to
; ;; provide a tally of how many times each sentence size/quantity occurs

; (defn getsentencereport [word pos]
;   (let [wordstring word
;         posstring pos
;         wordobject (dictionary word)
;         posobject ((dictionary word) pos)
;         tallyarray (posobject "tally")
;         a (map
;            #(count (clojure.string/split % #"\s"))
;            tallyarray
;         )
;         b (map
;              #(count
;                    (filter (fn [x] (= x %))
;                     a
;                  )
;               )
;            (set a))
;         ]

;     {
;      "tallies" (zipmap (set a) b)
;      "min" (apply min a)
;      "max" (apply max a)
;      "mean" (int (/ (apply + a) (count a)))
;      }
;   )
; )



; ;; this is a helper fucntion for the getwordpositionreport function
; ;; below. It checks how many times a word is found in a sentence
; ;; and it returns an array of the indices where it is found

; (defn sentenceoccurances [n word]

;   (def sentence n)
;   (def tally [])

;   (while (not= (.indexOf sentence word) -1)

;     (def tally (conj tally
;                      (.indexOf sentence word)))

;     (def sentence (subvec sentence (inc (.indexOf sentence word))))
;     )
;   tally
; )



; ; ;; this function maps over each sentence in a word object's tally
; ; ;; and establishes a collection of index positions where the word 1
; ; ;; string argument is found. It then flattens and reduces in order to
; ; ;; provide a tally of occurances at each index positions

; (defn getwordpositionreport [word pos]
;   (let [wordstring word
;         posstring pos
;         wordobject (dictionary word)
;         posobject ((dictionary word) pos)
;         tallyarray (posobject "tally")
;         a (flatten
;              (map #(sentenceoccurances (clojure.string/split % #"\s") wordstring)
;                   tallyarray
;              )
;           )
;         b (map
;              #(count
;                    (filter (fn [x] (= x %))
;                     a
;                  )
;               )
;            (set a))
;         ]

;     (println a)
;     (println b)
;     (println (zipmap (set a) b))

;     {
;      "tallies" (zipmap (set a) b)
;      "min" (apply min a)
;      "max" (apply max a)
;      "mean" (int (/ (apply + a) (count a)))
;      }
;   )
; )



; ;; this future function will traverse the word object's synonem
; ;; array and provide tallies to map reduce upon (just like we
; ;; do with definition sentences above)

; ; (defn traversesynonems [defins] ;; argument is a string

; ;   (let [synonem (clojure.string/split defins #"\s")]

; ;     (dotimes [n (count sentencearray)]
; ;         (routetotallyword (synonem n) defins))
; ;   )
; ; )


