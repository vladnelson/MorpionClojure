(ns game_morpion.core
    (:require [reagent.core :as reagent :refer [atom]]
           
    )
)

(enable-console-print!)

;;==========================================================================================
;; function retournant une valeur..0
;;==========================================================================================
(def board-size 3)

(def win-length 3)

(defn new-board [n]
  (vec (repeat n (vec (repeat n "B"))))
)





;;==========================================================================================
;; Joueur VS Ordinateur 
;;==========================================================================================
(defonce app-state 
  (atom 
    {:text "Joueur VS Ordinateur"
     :board (new-board 3)
     :game-status :in-progress
     :name "JVSO"
     :Joueur 0
     :Ordinateur 0
     :draw 0
    }
  )
)


;;------------------------------------------------------------------------------------------
;; L'IA Joue
;;------------------------------------------------------------------------------------------
(defn computer-move [board]
  (let [remaining-spots (for [
                              i (range board-size)
                              j (range board-size)
                              :when (= (get-in board [j i]) "B")
                             ]
                             [j i]
                        )
          move (when (seq remaining-spots) 
                      (rand-nth remaining-spots)
                )
          ]
     (if move
      (assoc-in board move "C")
      board)
  )
)


(defn straight [owner board [x y] [dx dy] n]
  (every? true?
    (for [i (range n)]
      (= (get-in  board [
                          (+ (* dx i) x)
                          (+ (* dy i) y)
                        ]
         )
         owner
      )
    )    
  )
)

;;------------------------------------------------------------------------------------------
;; Check if win.
;;------------------------------------------------------------------------------------------
(defn is_win [owner board n]
  (some true? 
      (for [
            i (range board-size)
            j (range board-size)
            dir [[1 0] [0 1] [1 1] [1 -1]]
           ]
           (straight owner board [i j] dir n)
      )
  )
)

;;------------------------------------------------------------------------------------------
;; Check if all case is played.
;;------------------------------------------------------------------------------------------
(defn is_finish [board]
  (every? #{"P" "C" "PL1" "PL2" "PO1" "PO2"}
    (apply concat board)
  )
)
 
;;------------------------------------------------------------------------------------------
;; Enum Status .
;;------------------------------------------------------------------------------------------
(defn game-status [board] 
  (cond
      (is_win "P" board win-length) :player-victory
      (is_win "C" board win-length) :computer-victory
      (is_win "PL1" board win-length) :playerL1-victory
      (is_win "PL2" board win-length) :playerL2-victory
      (is_win "PO1" board win-length) :playerO2-victory
      (is_win "PO2" board win-length) :playerO2-victory
      (is_finish board) :draw
      :else :in-progress
  )
)

;;------------------------------------------------------------------------------------------
;; Update status 
;;------------------------------------------------------------------------------------------
(defn update-status [state]
  (assoc state :game-status 
    (game-status 
      (:board state)
    )
  )
)

;;------------------------------------------------------------------------------------------
;; Update status 
;;------------------------------------------------------------------------------------------
(defn check-game-status [state]
  (-> state
    (update-in [:board] computer-move)
    (update-status)
    
  )
  
)

(defn check-game-statusL [state]
  (-> state
    ;;(update-in [:board] computer-move)
    (update-status)
  )
)


;;------------------------------------------------------------------------------------------
;; Fonction sur une case
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
        (prn (:name @app))
          (prn (:board @app))
          (prn (:game-status @app))
          (prn (:is_turn_player1 @app))
          (case (:name @app) 
            "JVSJL" 
               (when (= (:game-status @app) :in-progress)
                 (if (:is_turn_player1 @app)
                    ( 
                      (swap! app assoc-in [:board j i] "PL1")
                      (swap! app assoc :is_turn_player1 false)
                      
                    )

                    ( 
                      (swap! app assoc-in [:board j i] "PL2")
                      (swap! app assoc :is_turn_player1 true)
                      
                    )                 
                 )     

                  (if (is_win "PL1" (:board @app) win-length)
                    (  ;; Si Oui
                      (swap! app assoc :game-status :playerL1-victory) 
                      (swap! app assoc :Joueur1 (+ (:Joueur1 @app) 1))
                    )
                   
                   
                    (if (is_win "PL2" (:board @app) win-length) ;; SI Non
                      (   ;; Si Oui
                        (swap! app assoc :game-status :playerL2-victory) 
                        (swap! app assoc :Joueur2 (+ (:Joueur2 @app) 1))
                      )
                     
                      (
                        (swap! app check-game-statusL)
                        (if  (= (game-status  (:board @app)) :draw )
                          (swap! app assoc :draw (+ (:draw @app) 1))    
                          (prn @app)                  
                        )
                      ) ;; Si Non
                    )
                  )
                )
            "JVSO"  
               (when (= (:game-status @app) :in-progress)
                  (swap! app assoc-in [:board j i] "P")
                  (if (is_win "P" (:board @app) win-length)
                    (
                      (swap! app assoc :game-status :player-victory)
                      (swap! app assoc :Joueur (+ (:Joueur @app) 1))
                    )

                    (
                      (swap! app check-game-status)
                      (if  (= (game-status  (:board @app)) :draw )
                        (swap! app assoc :draw (+ (:draw @app) 1))  
                     
                        (if  (= (game-status  (:board @app)) :computer-victory )
                           (swap! app assoc :Ordinateur (+ (:Ordinateur @app) 1))    
                            (prn app)    
                        )               
                      )
                    )                    
                  )
                )

            "JVSJO" (prn "Joeueur Online")
          )             
        )
      }
    ]
  )


  (defn circle [i j]
    [:circle
      {
        :r 0.35
        :stroke "green"
        :stroke-width 0.12
        :fill "none"
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


;;----------------------------------------------------------------------------------------------
;; Add-on 1 (3 points) : un mode joueur vs ordinateur (+3 points si l'ordinateur joue bien ...)
;;----------------------------------------------------------------------------------------------
  (defn game_morpion1VSO [] 
    [:center
      [:h4 (:text @app-state)]
      [:h5
          (case (:game-status @app-state)
              :player-victory "Vous avez gagnée !"
              :computer-victory "Vous avez perdu !"
              :draw "Match Null"
              ""
          )
      ]
       [:p 
        [:button
          {:on-click
            (fn reset-game [e]
               (swap! app-state assoc
                  :Joueur 0
                  :Ordinateur 0
                  :draw 0
                  :board (new-board board-size)
                  :game-status :in-progress
               )
            )  
          :class "btn red"            
          }
          "Reset Score"
        ]      
        [:button
          {:on-click
            (fn new-game [e]
               (swap! app-state assoc
               :board (new-board board-size)
               :game-status :in-progress)
            )  
          :class "btn green"            
          }
          "Nouvelle Partie"
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

      [:table { :class "table highlight striped centered responsive-table bordered"}
        [:thead 
          [:tr 
            [:th "Joueur"]
            [:th "Points"]
          ]
        ]
        [:tbody
          [:tr
            [:td "Joueur"]
            [:td ( :Joueur @app-state)]
          ]
          [:tr
            [:td "Ordinateur"]
           [:td ( :Ordinateur @app-state)]
          ]
          [:tr
            [:td "Null"]
           [:td ( :draw @app-state)]
          ]           
        ]      
      ]
    ]
  )


;;==============================================================================================================================
;; Joeur VS Joueur (Local) - Base (14 points) : le morpion 3x3 (tic-tac-toe), en mode deux joueurs jouant sur la même page web
;;==============================================================================================================================
                    
(defonce app-state1Vs1L
  (atom 
    {:text "Joueur VS Joueur (Local)"
     :board (new-board 3)
     :game-status :in-progress
     :is_turn_player1 true
     :name "JVSJL"
     :Joueur1 0
     :Joueur2 0
     :draw 0
    }
  )
)

 (defn game_morpion1VS1L [] 
    [:center
      [:h4 (:text @app-state1Vs1L)]
       [:h5 
          (case (:game-status @app-state1Vs1L)
              :playerL1-victory "Joueur 1 a gagné !"
              :playerL2-victory "Joueur 2 a gagné !"
              :draw "Match Null"
              ""
          )
      ]
       [:p 
         [:button
          {:on-click
            (fn reset-game [e]
              (swap! app-state1Vs1L assoc
                  :Joueur1 0
                  :Joueur2 0
                  :draw 0
                  :board (new-board board-size)
                  :game-status :in-progress
              )
            )  
          :class "btn red"            
          }
          "Reset Score"
        ]      
        [:button
          {:on-click
            (fn new-game [e]
               (swap! app-state1Vs1L assoc
               :board (new-board board-size)
               :game-status :in-progress)
            )  
          :class "btn green"            
          }
          "Nouvelle Partie"
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
                  "PL1" [circle i j]
                  "PL2" [cross i j]
              )
          )
        ]
      )

      [:table { :class "table highlight striped centered responsive-table" }
        [:thead 
          [:tr 
            [:th "Joueur"]
            [:th "Points"]
          ]
        ]
        [:tbody
          [:tr
            [:td "Joueur 1"]
            [:td (:Joueur1 @app-state1Vs1L)]
          ]
          [:tr
            [:td "Joueur 2"]
            [:td (:Joueur2 @app-state1Vs1L)]
          ]
          [:tr
            [:td "Null"]
            [:td (:draw @app-state1Vs1L)]
          ]
        ]      
      ]
    ]
  )


;;==========================================================================================
;; Joeur VS Joueur Online
;;==========================================================================================

(defonce app-state1Vs1O
  (atom 
    {:text "Joueur VS Joueur (Online)"
     :board (new-board 3)
     :game-status :in-progress
     :name "JVSJO"
    }
  )
)


   (defn game_morpion1VS1O [] 
    [:center
      [:h4 (:text @app-state1Vs1O)]
         [:h5
          (case (:game-status @app-state1Vs1O)
              :playerO1-victory "Le Joueur 1 a gagné !"
              :playerO2-victory "Le Joueur 2 a gagné !"
              :draw "Match Null"
              ""
          )
        ]
       [:p 
         [:button
          {:on-click
            (fn reset-game [e]
              (swap! app-state1Vs1O assoc
                  :board (new-board board-size)
                  :game-status :in-progress
              )
            )   
          :class "btn red"            
          }
          "Reset Score"
        ]      
        [:button
          {:on-click
            (fn new-game [e]
               (swap! app-state1Vs1O assoc
               :board (new-board board-size)
               :game-status :in-progress)
            )  
          :class "btn green"            
          }
          "Nouvelle Partie"
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
                  "PO1" [circle i j]
                  "PO2" [cross i j]
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
   
  
)
