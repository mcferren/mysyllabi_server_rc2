(ns recursiftion.dao_graph
  (:require [clojure.string]
            [clojurewerkz.neocons.rest        :as nr]
            [clojurewerkz.neocons.rest.cypher :as cy]
            [clojure.pprint :refer [pprint]])
  (:import [org.bson.types.ObjectId])
  )


;; connects to a Neo4J Server 
(def conn (nr/connect "http://app40992485:F6bNfeA6f35v0RijWJDE@app40992485.sb02.stations.graphenedb.com:24789/db/data/"))
; (def conn (nr/connect "http://mysyllabi_graph_datastore:rCp1WNnLuENLz8a1b90E@mysyllabigraphdatastore.sb02.stations.graphenedb.com:24789/db/data/"))

; (defn- env-var
;   [^String s]
;   (get (System/getenv) s))


; (def GRAPHENE-HTTP-URL (str "http://" 
;                             (env-var "GRAPHENE_HOST")
;                             ":24789/db/data/"))
; (def GRAPHENE-HTTPS-URL (str "https://"
;                             (env-var "GRAPHENE_HOST")
;                             ":24780/db/data/"))

; (println GRAPHENE-HTTP-URL)
; (def conn (nr/connect GRAPHENE-HTTP-URL))


(def graph-query "MATCH (m:Movie)<-[:ACTED_IN]-(a:Person)
                  RETURN m.title as movie, collect(a.name) as cast
                  LIMIT {limit};")

(defn get-graph
  [limit]
  (let   [lim      (if (some? limit)
                     (Integer/parseInt limit)
                     100)
          result   (cy/tquery conn graph-query {:limit lim})
          nodes    (map (fn [{:strs [cast movie]}]
                          (concat [{:title movie
                                    :label :movie}]
                                  (map (fn [x] {:title x
                                                :label :actor})
                                       cast)))
                        result)
          nodes        (distinct (apply concat nodes))
          nodes-index  (into {} (map-indexed #(vector %2 %1) nodes))
          links        (map (fn [{:strs [cast movie]}]
                              (let [target   (nodes-index {:title movie :label :movie})]
                                (map (fn [x]
                                       {:target target
                                        :source (nodes-index {:title x :label :actor})})
                                     cast)))
                            result)]
    {:nodes nodes :links (flatten links)}))




; (def search-query "MATCH (movie:Movie) WHERE movie.title =~ {_nodeid} RETURN movie;")





; MATCH (n)-[relone:RELATIONSHIP_ONE]->(children_one)
; WHERE n.id='839930493049039430'
; RETURN n.id          AS id,
;        n.name        AS name,
;        labels(n)[0]  AS type,
;        {
;            COLLECT({ 
;               children_one.name        : children_one.foo
;            }) AS rel_two_representation
;        } AS parent_keystring




;        ,
;        COLLECT({ 
;           children_one.name        : children_one.foo
;        }) AS rel_two_representation



; // GET NODE OBJECT by NODEID
; MATCH (n)-[inventory:ALL_RESOURCES]->(children)
; WHERE  n.id='organ-systems' // n.id={_nodeid}
; WITH   n, COUNT(inventory) AS cnt
; MATCH (n)-[tablist:INCLUDES]->(tab)
; RETURN n.id         AS id, 
;        n.name       AS name, 
;        n.color      AS color,
;        n.background AS background,
;        LABELS(n)[0] AS type, 
;        { 
;           allresources: { 
;               id      : (n.id + '_all'), 
;               count   : cnt 
;           }
;        } AS leaves, 
;        COLLECT({ 
;           label       : tab.label,
;           layout      : tab.layout,
;           name        : tab.name,
;           leaf        : tab.leaf,
;           refine      : tab.refine 
;        }) AS auxtabs;


; MATCH (n)-[inventory:ALL_RESOURCES]->(children),
;       (n)-[taglist:TAGGED_BY]->(tag),
;       (n)-[references:INSTANTIATES]->(link),
;       (n)-[tablist:INCLUDES]->(tab)
; WHERE n.id='organ-systems' // n.id={_nodeid}
; RETURN n.id          AS id,
;        n.name        AS name,
;        n.color       AS color,
;        n.background  AS background,
;        labels(n)[0]  AS type,
;        {
;          allresources: {
;            id: (n.id + '_all'),
;            count: count(inventory)
;          }
;        } AS leaves,
;        COLLECT({ 
;           label       : tab.label,
;           layout      : tab.layout,
;           name        : tab.name,
;           leaf        : tab.leaf,
;           refine      : tab.refine
;        }) AS auxtabs




; match (n)-[r]-()
; WHERE n.id = 'letter-a' 
;     AND NOT type(r) = 'BROADCASTING_TO'
;     AND NOT type(r) = 'TAGGED_BY'
;     AND NOT type(r) = 'INSTANTIATES'
;     AND NOT type(r) = 'INCLUDES'
; RETURN { 
;           type : distinct type(r),
;           count : COUNT type(r)
;        }


; { 
;    allresources: { 
;        id      : (n.id + '_all'), 
;        count   : cnt 
;    }
; } 

; (def node-query "MATCH (n)-[attributelist:RELATIONSHIPTYPE]->(target)
;                  RETURN n.id          AS id, 
;                         n.name        AS name, 
;                         COLLECT({
;                           target    : target.id
;                         })            AS attributes;")

; (def node-query "MATCH (n)-[attributelist:TAGGED_BY]->(target)
;                  WHERE n.id={_nodeid}
;                  RETURN n.id          AS id, 
;                         n.name        AS name, 
;                         COLLECT({
;                           target    : target.id
;                         })            AS attributes;")

; (def node-query "MATCH (n)-[r:ALL_RESOURCES]-()
;                  WHERE n.id={_nodeid}
;                  WITH n
;                  OPTIONAL MATCH (n)-[subscriberlist:BROADCASTING_TO]->(subscriber)
;                  RETURN n.id                   AS id, 
;                         COLLECT(subscriber.id) AS subscribers;")


                        ; CASE WHEN subscriber is NULL THEN [] AS subcsribers;
                        ;      ELSE [] END AS subscribers;")

; COLLECT(subscriber.id) AS subscribers;")


; (def node-query-one "MATCH (n)-[rel_one:RELATIONSHIP_ONE]-()
;                      WHERE n.id='EXAMPLE_ID_ONE'
;                      WITH n
;                      MATCH (n)-[rel_two:RELATIONSHIP_TWO]->(target)
;                      RETURN n.id                   AS id, 
;                              COLLECT(target.id) AS targetlist;")

; (def node-query-two "MATCH (n)-[rel_one:RELATIONSHIP_TWO]-()
;                      WHERE n.id='EXAMPLE_ID_TWO'
;                      WITH n
;                      MATCH (n)-[rel_two:RELATIONSHIP_TWO]->(target)
;                      RETURN n.id                   AS id, 
;                              COLLECT(target.id) AS targetlist;")







; (def node-query-one "MATCH (n)-[rel_one:RELATIONSHIP_ONE]-()
;                      WHERE n.id='EXAMPLE_ID_ONE'
;                      WITH n
;                      MATCH (n)-[rel_two:RELATIONSHIP_TWO]->(target)
;                      RETURN n.id                   AS id, 
;                             CASE WHEN target is NULL THEN []
;                                  ELSE COLLECT(target.id) END AS targetlist;")

; (def node-query-two "MATCH (n)-[rel_one:RELATIONSHIP_TWO]-()
;                      WHERE n.id='EXAMPLE_ID_TWO'
;                      WITH n
;                      MATCH (n)-[rel_two:RELATIONSHIP_TWO]->(target)
;                      RETURN n.id                   AS id, 
;                             CASE WHEN target is NULL THEN [];
;                                  ELSE COLLECT(target.id) END AS targetlist;")





; ########################################################################
; ########################################################################
; ########################################################################
; ########################################################################
; ########################################################################
; ########################################################################

; OPTIONAL MATCH (n {id: 123})-[ilist:INSTANTIATES]->(target)
; WITH n, COLLECT(
;   CASE
;     WHEN ilist.container IS NULL THEN NULL
;     ELSE { container : ilist.container, target : target.id } END
; ) AS iset
; RETURN n.id AS id, n.name AS name, n.color AS color, n.background AS background, LABELS(n)[0] AS type, iset AS instantiations;



(def get-leaf-array-length-query "MATCH p=(n {id:%s})-[r:%s*]->(c)
                                  RETURN MAX(length(p))")


(defn get-leaf-array-length [nodeid leafname]
  (let [_nodeid nodeid
        _leafname leafname]
        ; _response (cy/tquery conn get-leaf-array-length-query {
        ;                 :_nodeid _nodeid
        ;                 :_leafname _leafname
        ;         })]
        
        ; _response

        ; (println (format get-leaf-array-length-query (str "'" _nodeid "'") (str "`" _leafname "`")))

        (cy/tquery conn (format get-leaf-array-length-query (str "'" _nodeid "'") (str "`" _leafname "`"))
                        {})
    )
)


     ; OPTIONAL MATCH p=(n {id:'andrewc_jones-mess'})-[r:`andrewc_jones-mess_ALLRESOURCES`*]->(c)
     ;                              RETURN MAX(length(p))

     ;             WHERE left( type(r), LENGTH( {_nodeid} )) = {_nodeid}



     ;             WITH n, COLLECT({
     ;                id    : type(r),
     ;                count : 
     ;             }) AS leafset, AS hullaballoo

     ;             ; WITH n, COLLECT(type(r)) AS leafset

     ;             MATCH p=(n {id:'andrewc_jones-mess'})-[r:`andrewc_jones-mess_ALLRESOURCES`*]->(c)
     ;                              RETURN MAX(length(p))

; MATCH (n {id: {_nodeid}})
; MATCH (n)-[r]-()
; WHERE left( type(r), LENGTH( {_nodeid} )) = {_nodeid}
; MATCH p=(n)-[rs*]->(c) WHERE r IN rs
; WITH n, {
;     id    : type(r),
;     count : MAX(length(p))
; } AS leafset


; MATCH (n { id: {_nodeid} })
; OPTIONAL MATCH p=(n)-[r*]->(c)
; WHERE (type(r[0]) STARTS WITH {_nodeid}) AND NOT (c)-->()
; RETURN n, COLLECT({ 
;     id: type(r[0]), 
;     depth: length(p)
; }) AS leafreport;


; OPTIONAL MATCH????
;; or maybe match relationship label with those that start with NODEID
(def node-query "MATCH (n {id: {_nodeid}})
                 OPTIONAL MATCH (n)-[r]-()
                 WHERE left( type(r), LENGTH( {_nodeid} )) = {_nodeid}
                 WITH n, COLLECT(type(r)) AS leafset
                 OPTIONAL MATCH (n)-[tablist:INCLUDES]->(tab)
                 WITH n, leafset, COLLECT(
                    CASE
                        WHEN tab.layout IS NULL THEN NULL
                        ELSE { 
                           label       : tab.label,
                           layout      : tab.layout,
                           name        : tab.name,
                           leaf        : tab.leaf,
                           refine      : tab.refine 
                        }
                    END
                    ) AS tabset
                 OPTIONAL MATCH (n)-[taglist:TAGGED_BY]->(tagnode)
                 WITH n, leafset, tabset, COLLECT(tagnode.id) AS tagset
                 OPTIONAL MATCH (n)-[instantiationlist:INSTANTIATES]->(target)
                 WITH n, leafset, tabset, tagset, COLLECT(
                    CASE
                        WHEN instantiationlist.container IS NULL THEN NULL
                        ELSE {
                               container : instantiationlist.container,
                               target    : target.id
                             }
                    END
                    ) AS instantiationset
                 OPTIONAL MATCH (n)-[subscriberlist:BROADCASTING_TO]->(subscriber)
                 RETURN n.id                   AS id, 
                        n.name                 AS name, 
                        n.color                AS color,
                        n.background           AS background,
                        LABELS(n)[0]           AS type, 
                        leafset                AS leaves, 
                        tabset                 AS auxtabs, 
                        tagset                 AS tags,
                        instantiationset       AS instantiations,
                        COLLECT(subscriber.id) AS subscribers;")



(defn get-node [nodeid]
  (let [_nodeid nodeid
        _response (cy/tquery conn node-query {
                        :_nodeid _nodeid
                })]
        
        _response
    )
)



; COLLECT(subscriber.id) AS subscribers;")


; ########################################################################
; ########################################################################
; ########################################################################
; ########################################################################
; ########################################################################
; ########################################################################

; (def create-auxtab "MATCH (a);
;                     WHERE a.id = {_nodeid}
;                     CREATE (m:Tab {
;                         label    : {_tabobj.label},
;                         layout   : {_tabobj.layout},
;                         name     : {_tabobj.name},
;                         leaf     : {_tabobj.leaf},
;                         refine   : {_tabobj.refine}
;                     })
;                     CREATE  a-[:INCLUDES]->m;")


; create (a:{_nodeobj.type} { name: "foo" })-[:HELLO]->(b {name : "bar"}),
;        (a)-[:GOODBYE]->(d {name:"Quux"});


; (def create-relationship "MATCH (a:{_parentobj.type}),(b:{_childobj.type})
;                           WHERE a.id = {_parentobj.id} AND b.id = {_childobj.id}
;                           MERGE (a)-[r:{_relationshiptype} ]->(b)")






(def create-tab-relationship "MATCH (a:{_parenttype}),(b:{_childobj.type})
                              WHERE a.id = {_parentid} AND b.id = {_childobj.id}
                              MERGE (a)-[r:INCLUDES {
                                  label    : {_childobj.label},
                                  layout   : {_childobj.layout},
                                  name     : {_childobj.name},
                                  leaf     : {_childobj.leaf},
                                  refine   : {_childobj.refine}
                              } ]->(b)")



(def create-leaf-relationship "MATCH (a),(b)
                               WHERE a.id = {_parentnodeid} AND b.id = {_childnodeid}
                               CREATE (a)-[r:ALL_RESOURCES {
                                   isFavorite                : false,
                                   isOpen                    : false,
                                   trueindex                 : 0,
                                   currenttab                : 'all',
                                   currentdate               : null
                               } ]->(b)
                               RETURN r")


(def transfer-tabs-query "MATCH  n-[rel:%s]->r 
                          WHERE  n.id={_parentnodeid} 
                          DELETE rel
                          UNWIND {_inventoryarray} AS _inventoryobj
                          MATCH  (a:%s {id: {_parentnodeid}}),
                                 (t {id: _inventoryobj.id})
                          CREATE (a)-[r:%s {
                                   isFavorite                : _inventoryobj.isFavorite,
                                   isOpen                    : _inventoryobj.isOpen,
                                   trueindex                 : _inventoryobj.trueindex,
                                   currenttab                : _inventoryobj.currenttab,
                                   currentdate               : _inventoryobj.currentdate
                              }]-(t)"
)



; (def unshift-inventory-empty-query "MATCH a, c
;                                     WHERE n.id={_parentnodeid} AND c.id = {_id}
;                                     CREATE (a)-[relnew:%s {
;                                          isFavorite                : {_isFavorite},
;                                          isOpen                    : {_isOpen},
;                                          currenttab                : {_currenttab},
;                                          currentdate               : {_currentdate}
;                                     } ]->(c)"
; )


; MERGE (t:%s {id:{_id}})
; ON CREATE
; SET t.name = {_name}, 
;     t.color = {_color}, 
;     t.heightstate = {_heightstate}, 
;     t.background = {_background}
; RETURN t;



; MERGE (t:%s {id:{_id}})
; ON CREATE
; SET t.name = {_name}, 
;     t.color = {_color}, 
;     t.heightstate = {_heightstate}, 
;     t.background = {_background}
; RETURN t;



; MATCH (a:%s {id: {_parentnodeid}}), (b:%s {id: {_id}})
; MERGE  a-[relold:%s]->b
;   ON CREATE
;      SET relold.metadata = {_metaData}
;   ON MATCH
;      ...



; MATCH (parent:Parent {parentId: 1234})-[:CHILDREN]->()-[:NEXT*3]->(child)
; RETURN child


; (def unshift-inventory-query "MATCH  a-[relold:%s]->b, c
;                               WHERE  n.id={_parentnodeid} AND c.id = {_id}
;                               CREATE (a)-[relnew:%s {
;                                    isFavorite                : {_isFavorite},
;                                    isOpen                    : {_isOpen},
;                                    currenttab                : {_currenttab},
;                                    currentdate               : {_currentdate}
;                               } ]->(c)
;                               CREATE (c)-[relrepair:%s {
;                                    isFavorite                : rel.isFavorite,
;                                    isOpen                    : rel.isOpen,
;                                    currenttab                : rel.currenttab,
;                                    currentdate               : rel.currentdate
;                               } ]->(b)
;                               DELETE relold"
; )

; ; (defn is-small? [number]
; ;   (if (< number 100) "yes" "no"))

; (defn unshiftInventoryObject [parentnodeid inventoryobj relationshiptype]
;     (let [_parentnodeid         parentnodeid
;           _inventoryobj         inventoryobj]
;           ; _relationshiptype     relationshiptype
;           ; _relationshiplabel    (str parentnodeid "_" relationshiptype)
;           ; _unshiftquery         (if )]

;         ; (cy/tquery conn (format unshift-inventory-query _relationshiplabel _relationshiplabel _relationshiplabel)
;         ;                 {
;         ;                     :_parentnodeid _parentnodeid
;         ;                     :_id (get-in _inventoryobj [:id])
;         ;                     :_isFavorite (get-in _inventoryobj [:isFavorite])
;         ;                     :_isOpen (get-in _inventoryobj [:isOpen])
;         ;                     :_currenttab (get-in _inventoryobj [:currenttab])
;         ;                     :_currentdate (get-in _inventoryobj [:currentdate])
;         ;                 })
;     )
; )


(defn transfer-tab [parentnodeid relationshiptype inventoryarray]
  (let [_parentnodeid parentnodeid]
        ; _childnodeid childnodeid]

            ; (cy/tquery conn (format create-many-tab-relationships relationshiptype relationshiptype)
            ;     {
            ;         :_parentnodeid parentnodeid
            ;         :_inventoryarray inventoryarray
            ;     })
    )
)


;; CREATE CONSTRAINT on (n:Calendar) ASSERT n.id IS UNIQUE;
(def create-node-base-query "MERGE (t:%s {id:{_id}})
                             ON CREATE
                                SET t.name = {_name}, 
                                    t.color = {_color}, 
                                    t.heightstate = {_heightstate}, 
                                    t.background = {_background}
                             RETURN t;")
; ON CREATE SET keanu.created = timestamp()
; ON MATCH SET keanu.lastSeen = timestamp()



; (def hypotheticalobjarray [
;         {
;             :id       "paints-223"
;             :label    "Pause"
;             :layout   "folder"
;             :name     "pause"
;             :leaf     "ALLRESOURCES"
;             :refine   []  
;         }
;         {
;             :id       "park-224"
;             :label    "Park"
;             :layout   "folder"
;             :name     "park"
;             :leaf     "ALLRESOURCES"
;             :refine   [] 
;         }
;     ]
; )


(def create-many-tab-relationships "MATCH (a:%s {id: {_parentid}})
                                    WITH a
                                    UNWIND {_tabarray} AS _tabobj
                                    MERGE (t:Tab {id:_tabobj.id})
                                     ON CREATE
                                        SET t.label = _tabobj.label, 
                                            t.layout = _tabobj.layout, 
                                            t.name = _tabobj.name, 
                                            t.leaf = _tabobj.leaf, 
                                            t.refine = _tabobj.refine
                                    MERGE (a)-[r:INCLUDES]->(t);")




; (def hypotheticaltagsarray {
;         :language-arts  3
;         :science        7
;         :third          5
;         :fourth         5
;         :fifth          4
;     }
; )



(def create-many-tag-relationships "UNWIND range(1,{_copies}) AS iterator
                                      MATCH (a:%s),(b)
                                      WHERE a.id = {_mothernodeid} AND b.id = {_tagnodeid}
                                      CREATE (a)-[r:TAGGED_BY]->(b);")


(def create-tag-relationship "MATCH (a:%s),(b)
                              WHERE a.id = {_mothernodeid} AND b.id = {_tagnodeid}
                              MERGE (a)-[r:TAGGED_BY]->(b);")


(defn capitalize-words 
  "Capitalize every word in a string"
  [s]
  (->> (clojure.string/split (str s) #"(-|_)") 
       (map clojure.string/capitalize)
       (clojure.string/join " ")))


; (def create-many-subscription-relationships "FOREACH (subscriptionnodeid IN {_subscribersarray} |
;                                                MATCH (a:{_broadcastnodetype}),(b)
;                                                WHERE a.id = {_broadcastnodeid} AND b.id = {subscriptionnodeid}
;                                                MERGE (a)-[r:BROADCASTING_TO]->(b);")
; (def hypotheticalsuscriberarray [
;         "letter-b"
;         "letter-c"
;     ]
; )

(def create-many-subscription-relationships "UNWIND {_subscribersarray} AS subscriptionnodeid
                                              MATCH (a:%s),(b)
                                              WHERE a.id = {_broadcastnodeid} AND b.id = subscriptionnodeid
                                              MERGE (a)-[r:BROADCASTING_TO]->(b);")


; (def create-subscription-relationship "MATCH (a:{_broadcastnodetype}),(b:Tag)
;                                        WHERE a.id = {_broadcastnodeid} AND b.id = {_listeninnodeid}
;                                        MERGE (a)-[r:BROADCASTING_TO]->(b);")

; (def unshift-node-query "MATCH (p:Parent {name:"Fred"})
;                          OPTIONAL MATCH (p)-[r:Foo]->(c:Child)
;                          WITH p, r, COLLECT(c) AS cs
;                          MERGE (cNew:Child {id:123})
;                          CREATE (p)-[rNew:Foo]->(cNew)
;                          FOREACH (x IN cs | 
;                            CREATE (cNew)-[:Foo]->(x)
;                            DELETE r)
;                          RETURN p, rNew, cNew;")







;; NEXT IS TO REWRITE THIS CYPHER QUERY TO SATISFY THE CONTRAINTS DISCUSSED WITH CYBERSAM ON STACKOVERFLOW THE PAST FEW DAYS

; (def exchange-node-query "MATCH (p {id: {_parentnodeid}})
;                           MATCH (p)-[:%s*%s]->()-[r:%s]->(c)
;                           OPTIONAL MATCH (c)-[r1:%s]->(c1)
;                           WITH c, r1, COLLECT(c1) AS c1s
;                           MERGE (cNew {
;                             id:{_childid},
;                             isFavorite:{_childisfavorite},
;                             isOpen:{_childisopen},
;                             currenttab:{_childcurrenttab},
;                             currentdate:{_childcurrentdate}
;                          })
;                           CREATE (c)-[rNew:%s]->(cNew)
;                           FOREACH (x IN c1s | 
;                             CREATE (cNew)-[:%s]->(x)
;                             DELETE r1)
;                           RETURN c, rNew, cNew;")




; (def hypotheticalinstantiationarray [
;         {
;             :container     "mysyllabi-histroy"
;             :targetid      "organ-systems"
;         }
;         {
;             :container     "mysyllabi-history"
;             :targetid      "letter-a"
;         }
;     ]
; )


(def create-many-instantiation-relationships "UNWIND {_instantiationsarray} AS _instid
                                              MATCH (a:%s {id: {_callerid}}),
                                                    (t {id: _instid})
                                              MERGE (a)-[r:INSTANTIATES {container: {_destination}}]-(t)")


;; NEW FOLDER NODE
(defn create-node [nodeobj]
  (let [_nodeobj nodeobj]

        ; (println "&& BURKA")
        ; (println _nodeobj)

        (cy/tquery conn (format create-node-base-query (clojure.string/capitalize (get-in _nodeobj [:type])))
                        { 
                            :_nodetype       (get-in _nodeobj [:type])
                            :_name           (get-in _nodeobj [:name])
                            :_id             (get-in _nodeobj [:id])
                            :_color          (get-in _nodeobj [:color])
                            :_heightstate    (get-in _nodeobj [:heightstate])
                            :_background     (get-in _nodeobj [:background])
                        })

        ;; instantiations
        (doseq [[key value] (get-in _nodeobj [:instantiations]) ] 

            (cy/tquery conn (format create-many-instantiation-relationships (clojure.string/capitalize (get-in _nodeobj [:type])))
                            {
                                :_instantiationsarray      value
                                :_callerid                 (get-in _nodeobj [:id])
                                :_destination              key
                            }) 
        ) ; eg) mysyllabi-history : [ organ-systems, letter-a ], mysyllabi-favorites : [ letter-b, letter-c ], etc...
      

        ; ; auxtabs 
        (if (seq (get-in _nodeobj [:auxtabs])) ;; TEST IF ANY - OPPOSITE OF empty?
            (cy/tquery conn (format create-many-tab-relationships (clojure.string/capitalize (get-in _nodeobj [:type])))
                            {
                                :_parentid    (get-in _nodeobj [:id])
                                :_parenttype  (get-in _nodeobj [:type])
                                :_tabarray    (get-in _nodeobj [:auxtabs])
                            })
        )

        ; ; tags 
        (doseq [[key value] (get-in _nodeobj [:tags]) ] 

            (cy/tquery conn (format create-many-tag-relationships (clojure.string/capitalize (get-in _nodeobj [:type])))
                            { 
                                :_mothernodeid    (get-in _nodeobj [:id])
                                :_mothernodetype  (get-in _nodeobj [:type])
                                :_tagnodeid       (str (name key))          
                                :_copies          value   
                                :_newnodecolor    (get-in _nodeobj [:color])
                                :_newnodename     (capitalize-words (name key))
                            }
            )
        ) ; eg) language-arts, 3rd, college, kinder, etc...


        ; ; leaves
        ; ; NO LEAVES IN NEO DATASTORE FOR NEWLY CREATED NODES


        ; subscribers
        (if (seq (get-in _nodeobj [:subscribers])) ;; TEST IF ANY - OPPOSITE OF empty?
            (cy/tquery conn (format create-many-subscription-relationships (clojure.string/capitalize (get-in _nodeobj [:type])))
                            { 
                              :_broadcastnodeid      (get-in _nodeobj [:id])
                              :_broadcastnodetype    (get-in _nodeobj [:type])
                              :_subscribersarray     (get-in _nodeobj [:subscribers])
                            }
            ) ; eg) bmcferren-organ-system, dpadilla-aikenprep-organ-system, etc...
        )

    )
)


(def exchange-node-query "MATCH (p {id: {_parentnodeid}})
                          MATCH (p)-[:%s]->(c0)-[r:%s]->(c)
                          OPTIONAL MATCH (c)-[r1:%s]->(c1)
                          WITH c0, r, c, r1, COLLECT(c1) AS c1s
                          MERGE (cNew { 
                            id:{_childid}
                          })
                          CREATE (c0)-[rNew:%s {
                            isFavorite:{_childisfavorite},
                            isOpen:{_childisopen},
                            currenttab:{_childcurrenttab},
                            currentdate:{_childcurrentdate}
                          }]->(cNew)
                          DELETE r, c
                          FOREACH (x IN c1s | 
                             CREATE (cNew)-[:%s {
                                isFavorite:r.isFavorite,
                                isOpen:r.isOpen,
                                currenttab:r.currenttab,
                                currentdate:r.currentdate
                             }]->(x)
                             DELETE r1)
                          RETURN c0, rNew, cNew;")

; MATCH (p { name:"Fred" })
; MATCH (p)-[:Foo*4]->(c0)-[r:Foo]->(c:Child)
; OPTIONAL MATCH (c)-[r1:Foo]->(c1:Child)
; WITH c0, r, c, r1, COLLECT(c1) AS c1s
; MERGE (cNew:Child { id:123 })
; CREATE (c0)-[rNew:Foo]->(cNew)
; DELETE r, c
; FOREACH (x IN c1s | 
;    CREATE (cNew)-[:Foo]->(x)
;    DELETE r1)
; RETURN c0, rNew, cNew;


(defn exchange-tab [payloadobj]
    (let [_tabid           (get-in payloadobj [:tabid])
          ; _parentid        (first (clojure.string/split _tabid #"_") )
          _parentid        (str (nth (clojure.string/split _tabid #"_") 0)
                             "_"
                           (nth (clojure.string/split _tabid #"_") 1))
          _pointerindex    (get-in payloadobj [:pointerindex])
          _pointerobj      (first (get-in payloadobj [:payloadobj :inventory ]))
          _nodeobj         (first (get-in payloadobj [:payloadobj :nodes ]))
          _linkedlistlabel (str "`" _tabid "`")
          _linkedlistindex (case _pointerindex
                                1 _linkedlistlabel
                                  (str "`" _tabid  "`*" _pointerindex))]



          ; (println "$$$ dollarobj")
          ; (println _tabid)
          ; (println _parentid)
          ; (println _pointerindex)
          ; (println _pointerobj)
          ; (println _linkedlistlabel)


          ; (case _pointerindex
          ;     1 (str (str "`" _tabid "`"))
          ;     (str "`" _tabid "*" _pointerindex "`"))

    ; (println (format exchange-node-query _linkedlistindex
    ;                                      _linkedlistlabel
    ;                                      _linkedlistlabel
    ;                                      _linkedlistlabel
    ;                                      _linkedlistlabel))

        (create-node _nodeobj)


        (cy/tquery conn (format exchange-node-query _linkedlistindex
                                                    _linkedlistlabel
                                                    _linkedlistlabel
                                                    _linkedlistlabel
                                                    _linkedlistlabel)
                        { 
                            :_parentnodeid      _parentid
                            :_childid           (get-in _pointerobj [:id])
                            :_childisfavorite   (get-in _pointerobj [:isFavorite])
                            :_childisopen       (get-in _pointerobj [:isOpen])
                            :_childcurrenttab   (get-in _pointerobj [:currenttab])
                            :_childcurrentdate  (get-in _pointerobj [:currentdate])
                        })

))


;; 1. put the tab and the node each in an array and package them in an object as properties invenotry and nodes
;; 2. make use of existing batch functions to just read an array with one elemen
;; 3. consolidate silent addnode

(def unshift-node-query "MATCH (p {id: {_parentnodeid}})
                         OPTIONAL MATCH (p)-[r:%s]->(c)
                         WITH p, r, COLLECT(c) AS cs
                         MERGE (cNew {
                            id:{_childid}
                         })
                         CREATE (p)-[rNew:%s {
                            isFavorite:{_childisfavorite},
                            isOpen:{_childisopen},
                            currenttab:{_childcurrenttab},
                            currentdate:{_childcurrentdate}
                         }]->(cNew)
                         FOREACH (x IN cs | 
                           CREATE (cNew)-[:%s {
                                isFavorite:r.isFavorite,
                                isOpen:r.isOpen,
                                currenttab:r.currenttab,
                                currentdate:r.currentdate
                           }]->(x)
                           DELETE r)
                         RETURN p, rNew, cNew;")

(defn unshift-tab [pointerobj]
    (let [_tabid        (get-in pointerobj [:tabid])
          ; _parentid     (first (clojure.string/split _tabid #"_") )
          _parentid     (str (nth (clojure.string/split _tabid #"_") 0)
                             "_"
                             (nth (clojure.string/split _tabid #"_") 1))
          ; _tabttype     (clojure.string/capitalize (get-in pointerobj [:tabtype]))
          _pointerobj   (get-in pointerobj [:pointerobj])
          _linkedlistlabel (str "`" _tabid "`")]

          ; (println "$$$ dollarobj")
          ; (println _tabid)
          ; (println _parentid)
          ; (println _pointerobj)
          ; (println _linkedlistlabel)


        (cy/tquery conn (format unshift-node-query _linkedlistlabel
                                                   _linkedlistlabel
                                                   _linkedlistlabel)
                        { 
                            :_parentnodeid      _parentid
                            :_childid           (get-in _pointerobj [:id])
                            :_childisfavorite   (get-in _pointerobj [:isFavorite])
                            :_childisopen       (get-in _pointerobj [:isOpen])
                            :_childcurrenttab   (get-in _pointerobj [:currenttab])
                            :_childcurrentdate  (get-in _pointerobj [:currentdate])
                        })

))



(defn unshift-tabby [pointerobj pointerid]
    (let [_pointerid pointerid
          _parentid (cond
                        (= (count (clojure.string/split _pointerid #"_")) 2) 
                                  (first (clojure.string/split _pointerid #"_") )
                        (= (count (clojure.string/split _pointerid #"_")) 3) 
                                  (str (nth (clojure.string/split _pointerid #"_") 0)
                                            "_"
                                       (nth (clojure.string/split _pointerid #"_") 1))
                                  )
          _pointerobj   pointerobj
          _linkedlistlabel (str "`" _pointerid "`")]

 


        (cy/tquery conn (format unshift-node-query _linkedlistlabel
                                                   _linkedlistlabel
                                                   _linkedlistlabel)
                        { 
                            :_parentnodeid      _parentid
                            :_childid           (get-in _pointerobj [:id])
                            :_childisfavorite   (get-in _pointerobj [:isFavorite])
                            :_childisopen       (get-in _pointerobj [:isOpen])
                            :_childcurrenttab   (get-in _pointerobj [:currenttab])
                            :_childcurrentdate  (get-in _pointerobj [:currentdate])
                        })

))

(defn unshift-batch [payloadarray]
    (let [_payloadarray payloadarray
          _inventory (get-in _payloadarray [:batchobj :inventory])
          _nodes (get-in _payloadarray [:batchobj :nodes])
          _parentid (get-in _payloadarray [:tabid])]

          ; (println "********************")
          ; (println _inventory)
          ; (println _nodes)
          ; (println _parentid)
          ; (println "********************")

        ;; http://stackoverflow.com/questions/10857690/mapping-over-a-vector-performing-side-effects
        (dorun (map (fn [element] (create-node element)) _nodes)) 
        (dorun (map (fn [element] (unshift-tabby element _parentid)) _inventory ))
))


(def push-node-query "MATCH (p {id: {_parentnodeid}})
                      MATCH (p)-[:%s]->()-[r:%s]->(c)
                      MERGE (cNew { 
                        id:{_childid}
                      })
                      CREATE (c)-[rNew:%s {
                            isFavorite:{_childisfavorite},
                            isOpen:{_childisopen},
                            currenttab:{_childcurrenttab},
                            currentdate:{_childcurrentdate}
                          }]->(cNew);")

(defn push-tab [pointerobj pointerindex parentid tabid]
    ; (let [_tabid           (get-in pointerobj [:tabid])
    ;       _parentid        (str (nth (clojure.string/split _tabid #"_") 0)
    ;                             "_"
    ;                             (nth (clojure.string/split _tabid #"_") 1))
    ;       _lastindex       (get-in pointerobj [:lastindex])
    ;       _pointerobj      pointerobj
    ;       _pointerindex    pointerindex
    ;       _linkedlistlabel (str "`" _tabid "`")
    ;       _linkedlistindex (case _lastindex
    ;                             1 _linkedlistlabel
    ;                               (str "`" _tabid  "`*" _lastindex))]
    (let [_pointerobj pointerobj
          _pointerindex pointerindex
          _tabid tabid
          _parentid parentid
          _linkedlistlabel (str "`" _tabid "`")
          _linkedlistindex (case _pointerindex
                                1 _linkedlistlabel
                                  (str "`" _tabid  "`*" _pointerindex))]


          ; (println "******************")
          ; (println _pointerindex)
          ; (println _parentid)
          ; (println _linkedlistlabel)
          ; (println _linkedlistindex)
          ; (println "##################")
          ; (println (get-in _pointerobj [:id])) ;; _childid
          ; (println (get-in _pointerobj [:isFavorite])) ;; _childisfavorite
          ; (println (get-in _pointerobj [:isOpen])) ;; _childisopen
          ; (println (get-in _pointerobj [:currenttab])) ;; _childcurrenttab
          ; (println (get-in _pointerobj [:currentdate])) ;; _childcurrentdate
          ; (println (format push-node-query _linkedlistindex
          ;                                       _linkedlistlabel
          ;                                       _linkedlistlabel))


        (cy/tquery conn (format push-node-query _linkedlistindex
                                                _linkedlistlabel
                                                _linkedlistlabel)
                        { 
                            :_parentnodeid      _parentid
                            :_childid           (get-in _pointerobj [:id])
                            :_childisfavorite   (get-in _pointerobj [:isFavorite])
                            :_childisopen       (get-in _pointerobj [:isOpen])
                            :_childcurrenttab   (get-in _pointerobj [:currenttab])
                            :_childcurrentdate  (get-in _pointerobj [:currentdate])
                        })
))


(defn push-batch [payloadarray]
    (let [_payloadarray payloadarray
          _inventory (get-in _payloadarray [:batchobj :inventory])
          _nodes (get-in _payloadarray [:batchobj :nodes])
          _tabid (get-in _payloadarray [:tabid])
          _parentid     (str (nth (clojure.string/split _tabid #"_") 0)
                             "_"
                        (nth (clojure.string/split _tabid #"_") 1))
          _pointerindex (get-in _payloadarray [:pointerindex])]

          ; (println _payloadarray)

        ; (println _inventory)
        ; (println "************")
        ; (println _nodes)
        ; (println "************")
        ; (println _parentid)
; (take 5 (iterate inc 5))

; (loop [iter 7
;        acc  0]
;   (if (> iter 10)
;     (println iter)
;     (recur (inc iter) (+ acc iter))))


; (map #(setCell 0 %1 %2) (iterate inc 0) data)


        ; (map #(push-tab %1 %2 %3) (iterate inc _pointerindex) _parentid _inventory)


        ; (zipmap [:a :b :c :d :e] [1 2 3 4 5])

        ; (reverse (take (count _inventory) (iterate inc _pointerindex))) ;; like (7 8 9 10)

        
        (dorun (map (fn [element] (create-node element)) _nodes))
        (dorun (map (fn [element] (push-tab element (- (get-in element [:trueindex]) 1) _parentid _tabid)) _inventory))
))



; MATCH (parent:Parent {parentId: 1234})-[:CHILDREN|NEXT*]->(child)
; RETURN parent, COLLECT(child) as children


; MATCH (n { id: 'andrew_jones-mess' })-[r:`andrewc_jones-mess_ALLRESOURCES`*]->(c)
; RETURN n, r, COLLECT(c) as children


; MATCH (n { id: '123A' })
; OPTIONAL MATCH p=(n)-[r:%s*]->(c)
; WHERE (type(r[0]) STARTS WITH '123A') AND NOT (c)-->()
; RETURN n, COLLECT({ 
;     id: type(r[0]), 
;     depth: length(p)
; }) AS leafreport;





; MATCH (n { id: {_nodeid} })-[inventory:%s*]->(links)
; RETURN c


(def tab-inventory-query "MATCH n-[inventory]->links
                          WHERE n.id={_nodeid} AND type(inventory)={_leaftype}
                          WITH n, inventory, links
                          ORDER BY inventory.trueindex



                          OPTIONAL MATCH (links)-[relationship]->(tab)
                          WHERE NOT type(relationship) = 'BROADCASTING_TO'
                                AND NOT type(relationship) = 'TAGGED_BY'
                                AND NOT type(relationship) = 'INSTANTIATES'
                                AND NOT type(relationship) = 'INCLUDES'
                          WITH n, inventory, links, COLLECT(type(relationship)) AS leafset
                          OPTIONAL MATCH (links)-[tablist:INCLUDES]->(tab)
                          WITH n, inventory, links, leafset, COLLECT(
                            CASE
                                WHEN tab.layout IS NULL THEN NULL
                                ELSE { 
                                  label       : tab.label,
                                  layout      : tab.layout,
                                  name        : tab.name,
                                  leaf        : tab.leaf,
                                  refine      : tab.refine 
                                }
                            END
                            ) AS tabset
                          OPTIONAL MATCH (links)-[taglist:TAGGED_BY]->(tagnode)
                          WITH n, inventory, links, leafset, tabset, COLLECT(tagnode.id) AS tagset
                          OPTIONAL MATCH (links)-[instantiationlist:INSTANTIATES]->(target)
                          WITH n, inventory, links, leafset, tabset, tagset, COLLECT(
                            CASE
                                WHEN instantiationlist.container IS NULL THEN NULL
                                ELSE {
                                       container : instantiationlist.container,
                                       target    : target.id
                                     }
                            END
                            ) AS instantiationset
                          OPTIONAL MATCH (links)-[subscriberlist:BROADCASTING_TO]->(subscriber)
                          WITH n, inventory, links, leafset, tabset, tagset, instantiationset, COLLECT(subscriber.id) AS subscriberset
                          RETURN (n.id + '_ALLRESOURCES') AS id,
                                 labels(n)[0]  AS type,
                                 COLLECT({ 
                                    id:          links.id, 
                                    isFavorite:  inventory.isFavorite,
                                    isOpen:      inventory.isOpen,
                                    trueindex:   inventory.trueindex,
                                    currenttab:  inventory.currenttab,
                                    currentdate: inventory.currentdate 
                                 }) AS inventory,
                                 COLLECT({
                                    id:          links.id, 
                                    name:        links.name,
                                    color:       links.color,
                                    background:  links.background,
                                    type:        LABELS(links)[0],
                                    auxtabs:     tabset,
                                    leaves:      leafset,
                                    tags:        tagset,
                                    instantiations: instantiationset,
                                    subscribers: subscriberset
                                 }) AS nodes;")

(defn get-tab-inventory [leafid]
  (let [_leafid leafid
        _nodeid (cond
                      (= (count (clojure.string/split _leafid #"_")) 2) 
                                (first (clojure.string/split _leafid #"_") )
                      (= (count (clojure.string/split _leafid #"_")) 3) 
                                (str (nth (clojure.string/split _leafid #"_") 0)
                                          "_"
                                     (nth (clojure.string/split _leafid #"_") 1))
                                )
        _leaftype (last (clojure.string/split _leafid #"_"))]
        ; _response (cy/tquery conn tab-inventory-query {:_nodeid _nodeid :_leaftype _leaftype})]

        ; (println "$$$ _nodeid" nodeid)
        ; (println "$$$ _leaftype" _leaftype)

        (cy/tquery conn (format push-node-query _leafid)
                { 
                    :_nodeid _nodeid 
                    :_leaftype _leaftype
                })
        
        ; _response
        ; _nodeid
    )
)


; (defn get-movie
;   [title]
;   (let [[result]   (cy/tquery conn title-query {:title title})]
;     result))


