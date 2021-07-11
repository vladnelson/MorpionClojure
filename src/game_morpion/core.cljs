(ns game_morpion.core
    (:require [reagent.core :as reagent :refer [atom]]
           
    )
)

(enable-console-print!)

(def board-size 3)

(defn new-board [n]
  (vec (repeat n (vec (repeat n "B"))))
)




;;==========================================================================================
;; Joeur VS Ordinateur
;;==========================================================================================
(defonce app-state 
  (atom 
    {:text "Joueur VS Ordinateur"
     :board (new-board 3)
    }
  )
)


(defn computer-move[app ]
  (let [board (:board @app) 
                remaining-spots (for [
                              i (range board-size)
                              j (range board-size)
                              :when (= (get-in board [j i]) "B")
                             ]
                             [i j]
                        )
       move (rand-nth remaining-spots)
       path (into [:board] (reverse move))]
    (swap! app assoc-in path "C")
  )
)


;;------------------------------------------------------------------------------------------
;; Status Case
;;------------------------------------------------------------------------------------------
  (defn blank [i j app]
    [:rect 
      {
        :width 0.9
        :height 0.9
        :fill "gray"
        :x (+ 0.05 i)
        :y (+ 0.05 j)
        :on-click
        (fn rect-click[e]
          (prn "You clicked me !" i j)
          (prn (swap!  app assoc-in [:board j i] "P"))
          (computer-move app)
        )
      }
    ]
  )


  (defn circle [i j]
    [:circle
      {
        :r 0.45
        :fill "green"
        :cx (+ 0.5 i)
        :cy (+ 0.5 j)
      }
    ]
  )

  (defn cross[i j]
    [:g  
      {
        :stroke "darkred"
        :stroke-width 0.35
        :stroke-linecap "round"
        :transform
          (str "translate(" (+ 0.5 i) "," (+ 0.5 j) ")" 
               "scale(0.35)"
          )
      }

      [:line       { :x1 -1 :y1 -1 :x2 1  :y2 1
       }
      ]

       [:line       { :x1 1 :y1 -1  :x2 -1 :y2 1  }
      ]
    ]
  )

;;------------------------------------------------------------------------------------------
;; End - Status Case
;;------------------------------------------------------------------------------------------

  (defn game_morpion1VSO [] 
    [:center
      [:h4 (:text @app-state)]
       [:p 
        [:button
          {:on-click
            (fn new-game [e]
               (swap! app-state assoc
               :board (new-board board-size)
               :game-status :in-progress)
            )  
          :class "btn waves-purple"            
          }
          "New Game"
        ]      
      ]
      (into
        [:svg
          {:view-box "0 0 3 3"
           :width 500
           :height 500
          }   
          ;; Creation du quadrillage 3 x 3
          (for [i (range (count (:board @app-state)))
                j (range (count (:board @app-state)))
              ]               
              (case (get-in @app-state [:board j i ])
                  "B" [blank i j app-state]
                  "P" [circle i j]
                  "C" [cross i j]
              )
          )
        ]
      )

     
    ]
  )


;;==========================================================================================
;; Joeur VS Joueur (Local)
;;==========================================================================================
(defonce app-state1Vs1L
  (atom 
    {:text "Joueur VS Joueur (Local)"
     :board (new-board 3)
    }
  )
)

 (defn game_morpion1VS1L [] 
    [:center
      [:h4 (:text @app-state1Vs1L)]
       [:p 
        [:button
          {:on-click
            (fn new-game [e]
               (swap! app-state1Vs1L assoc
               :board (new-board board-size)
               :game-status :in-progress)
            )  
          :class "btn btn-warming"            
          }
          "New Game"
        ]      
      ]
      (into
        [:svg
          {:view-box "0 0 3 3"
           :width 500
           :height 500
          }   
          ;; Creation du quadrillage 3 x 3
          (for [i (range (count (:board @app-state1Vs1L)))
                j (range (count (:board @app-state1Vs1L)))
              ]               
              (case (get-in @app-state1Vs1L [:board j i ])
                  "B" [blank i j app-state1Vs1L]
                  "P" [circle i j]
                  "C" [cross i j]
              )
          )
        ]
      )

     
    ]
  )


;;==========================================================================================
;; Joeur VS Joueur Online
;;==========================================================================================

(defonce app-state1Vs1O
  (atom 
    {:text "Joueur VS Joueur (Online)"
     :board (new-board 3)
    }
  )
)


   (defn game_morpion1VS1O [] 
    [:center
      [:h4 (:text @app-state1Vs1O)]
       [:p 
        [:button
          {:on-click
            (fn new-game [e]
               (swap! app-state1Vs1O assoc
               :board (new-board board-size)
               :game-status :in-progress)
            )  
          :class "btn btn-warming"            
          }
          "New Game"
        ]      
      ]
      (into
        [:svg
          {:view-box "0 0 3 3"
           :width 500
           :height 500
          }   
          ;; Creation du quadrillage 3 x 3
          (for [i (range (count (:board @app-state1Vs1O)))
                j (range (count (:board @app-state1Vs1O)))
              ]               
              (case (get-in @app-state1Vs1O [:board j i ])
                  "B" [blank i j app-state1Vs1O]
                  "P" [circle i j]
                  "C" [cross i j]
              )
          )
        ]
      )

     
    ]
  )



(reagent/render-component [game_morpion1VSO]
  (. js/document (getElementById "app"))
)


(reagent/render-component [game_morpion1VS1L]
  (. js/document (getElementById "app1VS1L"))
)


(reagent/render-component [game_morpion1VS1O]
  (. js/document (getElementById "app1VS1O"))
)




(defn on-js-reload []
  (prn (:board @app-state))
    (prn (:board @app-state))
      (prn (:board @app-state))
  
)
