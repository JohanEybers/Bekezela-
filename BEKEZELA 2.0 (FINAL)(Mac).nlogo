;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BEKEZELA - A Spatial Agent Based Model of the COVID-19 PANDEMIC IN SOUTH AFRICA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;CREATED BY: Johan Eybers (JUNE 2021)

;; THINGS TO NOTE
;; This is not a predicitve model but rather an educational one aimed at showing the utility of Agent-Based Models for pandemic modelling.
;; turtles = people
;; underscore used for variables and dash used for functions
;; inoculate while in a run: pause run. set inoculation percentage. write the word 'inoculate' in observer command box and press enter. Press model run again.
;; Use more than 1000 and less than 100000 for initial population for best results.
;; Running RSA experience simulates both the Beta and Delta variants only.


globals [                                                                          ;; declaration of global variables

  housecor                                                                         ;; used for the matrix of home addresses
  infect_matrix                                                                    ;; used for matrix of transition probabilites
  distance_to_infect                                                               ;; distance within which transmission can happen... SET TO ZERO IN THIS MODEL
  daily_new_cases
  time
  probability_of_transmission
]

turtles-own [                                                                      ;; declaration of turtle specific variables

  age                                                                              ;; age of the turtle
  homex                                                                            ;; x-coordinate of the turtle's home
  homey                                                                            ;; y-coordinate of the turtle's home
  schoolx                                                                          ;; x-coordinate of school
  schooly                                                                          ;; y-coordinate of school
  workx                                                                            ;; x-coordinate of workplace
  worky                                                                            ;; y-coordinate of workplace
  localx                                                                           ;; x-coordinate of local shop
  localy                                                                           ;; y-coordinate of local shop
  mallx                                                                            ;; x-coordinate of mall
  mally                                                                            ;; y-coordinate of mall
  socialx1                                                                         ;; x-coordinate of first social gathering spot
  socialy1                                                                         ;; y-coordinate of first social gathering spot
  socialx2                                                                         ;; x-coordinate of second gathering spot
  socialy2                                                                         ;; y-coordinate of second gathering spot
  socialx3                                                                         ;; x-coordinate of third gathering spot
  socialy3                                                                         ;; y-coordiante of third gathering spot

  state                                                                            ;; health state: susceptible, infected, critical, recovered
  condition                                                                        ;; not_infected or infected is used for spread-virus function
  t0                                                                               ;; time of infection
  t1                                                                               ;; time at which contagion begins
  t2                                                                               ;; time at which contagion ends (if c = 0: recover, if c = 1: recover or become critical at this time)
  t3                                                                               ;; if c = 0: time at which susceptible again. if c = 1: time at which susceptible/recover/dead
  t4                                                                               ;; only for c=1: time at which susceptible if recover at time t3
  c                                                                                ;; 0 for non-critical symptomatic or asymptomatic infection and 1 for critical and symptomatic infection
  occupation                                                                       ;; 0 = unemployed, 1 = employed, 2 = essential worker

  newi                                                                             ;; used for when inserting a new virus
  test                                                                             ;; used for testing turtles for the disease. 0 = not tested. 1 = negative test. 2 = positive test.
  isolate                                                                          ;; used for self-isolation
]

extensions [                                                                       ;; declaration of extensions used
  matrix                                                                           ;; importing functions to build and use matrices
  time
]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETUP PROCEDURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to setup

  clear-all

  if SETUP_RSA_COVID-19_EXPERIENCE = true [default]
  if SETUP_RSA_COVID-19_EXPERIENCE = true [set time time:create "2020/03/01"]
  if SETUP_RSA_COVID-19_EXPERIENCE = false [set time "N/A"]
  check-lockdown?                                                                                   ;; lockdown button

  create-initial-population                                                                         ;; setup population/turtles according to user specified demographics
  give-each-person-a-home                                                                           ;; each turtle gets an 'x' and 'y' coordinate representing their home

  create-infect-matrix                                                                              ;; infect_matrix is created
  initial-sick                                                                                      ;; make initial number of turtles sick
  inoculate

  create-workforce                                                                                  ;; create essential and non-essential workers
  create-workplaces                                                                                 ;; make workplaces ;;workforce before workplace
  create-schools
  create-shops
  create-malls
  create-gathering-spots1
  create-gathering-spots2
  create-gathering-spots3

  reset-ticks

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GO PROCEDURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to go
  if virus-gone? = true [stop]                                                                       ;; simulation stops if everyone is in one of: 'susceptible' state and 'not_infected' condition or 'inoculated' or "recovered", i.e. the pandemic is over
  if SETUP_RSA_COVID-19_EXPERIENCE = true [run-RSA-Covid-19]
  if Do_Testing? = true [testing]

  check-lockdown?                                                                                    ;; lockdown button

  spread-virus
  change-state
  color-change

  go-to-school
  go-to-work
  spread-virus
  change-state

  go-home
  go-to-local-shop
  go-to-mall
  spread-virus
  change-state

  go-home
  go-to-gathering
  spread-virus
  change-state
  color-change
  go-home

  if Self_isolation = true [self-isolate]
  if SETUP_RSA_COVID-19_EXPERIENCE = true [set time time:plus time 1 "days"]
  tick

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUNCTIONS USED IN GO AND SETUP PROCEDURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; CREATING INITIAL POPULATION ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to create-initial-population                                                                          ;; setup population according to user specified population and age demographics

  create-turtles   initial_population * _0y_to_9y    / 100 [set age 9 ]                               ;; 9 age categories in 10 year bands
  create-turtles   initial_population * _10y_to_19y  / 100 [set age 19]
  create-turtles   initial_population * _20y_to_29y  / 100 [set age 29]
  create-turtles   initial_population * _30y_to_39y  / 100 [set age 39]
  create-turtles   initial_population * _40y_to_49y  / 100 [set age 49]
  create-turtles   initial_population * _50y_to_59y  / 100 [set age 59]
  create-turtles   initial_population * _60y_to_69y  / 100 [set age 69]
  create-turtles   initial_population * _70y_to_79y  / 100 [set age 79]
  create-turtles   initial_population * _80y+        / 100 [set age 89]

  ask turtles [                                                                                       ;; command to all turtles
  set state "susceptible"                                                                             ;; set state "susceptible
  set condition "not_infected"                                                                        ;; set condition "not_infected"
  set size 10                                                                                         ;; set size 10
  set color blue - 10                                                                                 ;; set colour light blue
    set shape "person"
  ]

end

to give-each-person-a-home                                                                            ;; each turtle gets a home according to user specified household demographics

  ;; First, we need to create a number of households
  ;; we will create 30 households per 100 turtles in initial population

  set housecor (list [-1 0] )                                                                           ;; coordinates of first household in housecor
  let i 0                                                                                               ;; counter variable for while loop
  let num_households (initial_population * number_of_households_per_100_people / 100 )                  ;; total number of households
  set num_households precision num_households 0                                                         ;; makes it an integer value

  while [i < num_households + 1] [                                                                      ;; creates (num_households + 2) number of house coordinates
  set housecor lput list (random-xcor) (random-ycor)  housecor                                          ;; adding house coordinates to list
  set i i + 1                                                                                           ;; counter variable increases by one
  ]
  set housecor matrix:from-row-list housecor                                                            ;; converts list of coordinates to a (num_households + 2) x (2) matrix


  ;; Next, the turtles need to be divided into households
  ;; the following assumptions are made on the households
  let single single_member_households / 100                                                             ;; % of households that consists of a single turtle
  let two_three two/three_member_households / 100                                                       ;; % of households that consists of 2 to 3 turtles
  let four_five four/five_member_households / 100                                                       ;; % of households that consists of 4 to 5 turtles
  let six_plus six_or_more_member_households / 100                                                      ;; % of households that consists of 6 to 12 turtles

  ;; HOUSEHOLD COMPOSITION FOR SOUTH AFRICA
  if South_Africa_Household_Composition = true [

  ;; The single member/turtle households
  set i 0                                                                                               ;; recycling the dummy variable
  foreach (range 0 (single * num_households) ) [                                                        ;; "forloop"
    ask one-of turtles with [age > 20] [                                                                ;; only turtles with age > 20 are allowed to live alone, i.e. 30 and upwards
    set homex matrix:get housecor i 0
    set homey matrix:get housecor i 1
    set i i + 1
    ]
  ]


  ;; The 2 to 3 member/turtle households
  foreach (range 0 (two_three * num_households) ) [                                                     ;; "forloop"
    let s random 100                                                                                    ;; s is a random variable between 0 and 100

    ;; childless couple
    if s < 33 [                                                                                         ;; 33% of 37% = 12% of households are turtles with age > 20 live in a couple
      repeat 2 [ask one-of turtles with [homex = 0 and age > 20] [                                      ;; i.e. a childless couple
        set homex matrix:get housecor i 0
        set homey matrix:get housecor i 1
    ] ] ]

    ;; single parent & single child
    if s >= 33 and s < 49  [                                                                            ;;  16% of 37% = 6% of households have two turtles with one age > 20 and one age < 20
    ask one-of turtles with [homex = 0 and age > 20] [                                                  ;; i.e. one child and one parent
      set homex matrix:get housecor i 0
      set homey matrix:get housecor i 1
    ]
      ask one-of turtles with [homex = 0 and age < 20] [
        set homex matrix:get housecor i 0
        set homey matrix:get housecor i 1
      ] ]

      ;; single parent & two children                                                                   ;; 16% of 37% = 6% of households have three turtles. two with age < 20 and one age > 20
      if s >= 49 and s < 65 [                                                                           ;; i.e. one parent and two children
      ask one-of turtles with [homex = 0 and age > 20] [
        set homex matrix:get housecor i 0
        set homey matrix:get housecor i 1
      ]
        repeat 2 [ask one-of turtles with [homex = 0 and age < 20] [
        set homex matrix:get housecor i 0
        set homey matrix:get housecor i 1
      ] ] ]

      ;; two parents and single child                                                                   ;; 35% of 37% = 13% of households have three turtles. two age > 20 and one age < 20
      if s >= 65 [                                                                                      ;; i.e. two parents and one child
        repeat 2 [
         ask one-of turtles with [homex = 0 and age > 20] [
          set homex matrix:get housecor i 0
          set homey matrix:get housecor i 1
        ]
       ]
        ask one-of turtles with [homex = 0 and age < 20] [
          set homex matrix:get housecor i 0
          set homey matrix:get housecor i 1
        ]
      ]

    set i i + 1                                                                                        ;; increase dummy variable to move to next address in housecor
    ]


  ;; The 4 to 5 member/turtle households
   foreach (range 0 (four_five * num_households) ) [                                                   ;; 50% of 24% = 12% of households have five turtles with three age > 20 and two age < 20
    if random 100 < 50 [                                                                               ;; i.e. 3 adults and two children
     ask one-of turtles with [homex = 0 and age > 20] [
     set homex matrix:get housecor i 0
     set homey matrix:get housecor i 1
      ]
   ]
   repeat 2 [ask one-of turtles with [homex = 0 and age < 20] [                                        ;; 50% of 24% = 12% of households have 4 turtles with two age > 20 and two age < 20
      set homex matrix:get housecor i 0                                                                ;; i.e. two adults and two children
      set homey matrix:get housecor i 1
    ]
     ask one-of turtles with [homex = 0 and age > 20] [
     set homex matrix:get housecor i 0
     set homey matrix:get housecor i 1
    ]
  ]
    set i i + 1                                                                                         ;; increase dummy variable to move to next address in housecor
  ]

  ;; The 6 to 12 member/turtle households
   foreach (range 0 (six_plus * num_households) )  [
      ifelse (count turtles with [homex = 0 ]) < 13 [ ]                                                 ;; the last households stays at point (0,0)
    [ repeat ( 6 + random 7) [ask one-of turtles with [homex = 0] [                                     ;; 1/7 * 14% of households consist of 6,7,8,9,10,11 or 12 members
      set homex matrix:get housecor i 0
      set homey matrix:get housecor i 1
      ]
    ]
  ]
    set i i + 1
    ]
  ]

  ;; GENERAL HOUSEHOLD COMPOSITION
  if South_Africa_Household_Composition = false [

  ;; The single member/turtle households
  set i 0                                                                                               ;; recycling the dummy variable
  foreach (range 0 (single * num_households) ) [                                                        ;; "forloop"
    ask one-of turtles with [age > 20] [                                                                ;; only turtles with age > 20 are allowed to live alone, i.e. 30 and upwards
    set homex matrix:get housecor i 0
    set homey matrix:get housecor i 1
    set i i + 1
    ]
  ]


  ;; The 2 to 3 member/turtle households
  foreach (range 0 (two_three * num_households) ) [                                                     ;; "forloop"

    if random 100 < 50 [                                                                                ;; 50% 2 and 50% 3 and always at least one adult
       ask one-of turtles with [homex = 0] [
        set homex matrix:get housecor i 0
        set homey matrix:get housecor i 1
    ] ]

      ask one-of turtles with [homex = 0 and age > 20] [
        set homex matrix:get housecor i 0
        set homey matrix:get housecor i 1
      ]
        repeat 2 [ask one-of turtles with [homex = 0] [
        set homex matrix:get housecor i 0
        set homey matrix:get housecor i 1
     ]
    ]
    set i i + 1                                                                                        ;; increase dummy variable to move to next address in housecor
    ]


  ;; The 4 to 5 member/turtle households
   foreach (range 0 (four_five * num_households) ) [                                                  ;; 50% 4 and 50% 5 member with at least one adult
    if random 100 < 50 [
     ask one-of turtles with [homex = 0] [
     set homex matrix:get housecor i 0
     set homey matrix:get housecor i 1
      ]
   ]
   repeat 3 [ask one-of turtles with [homex = 0] [
      set homex matrix:get housecor i 0
      set homey matrix:get housecor i 1
      ] ]
     ask one-of turtles with [homex = 0 and age > 20] [
     set homex matrix:get housecor i 0
     set homey matrix:get housecor i 1
    ]
    set i i + 1                                                                                         ;; increase dummy variable to move to next address in housecor
  ]

  ;; The 6 to 10 member/turtle households
   foreach (range 0 (six_plus * num_households) )  [                                                    ;; 6 to 10 member households
      ifelse (count turtles with [homex = 0 ]) < 11 [ ]                                                 ;; the last households stays at point (0,0)
    [ repeat ( 6 + random 5) [ask one-of turtles with [homex = 0] [
      set homex matrix:get housecor i 0
      set homey matrix:get housecor i 1
      ]
    ]
  ]
    set i i + 1
    ]
  ]

   ask turtles [setxy homex homey]                                                                      ;; each turtle stands on its home coordinates
   set housecor 0                                                                                       ;; "remove housecor"
end

to create-workforce                                                                                   ;; make workers (non-essential + essential) according to user specified input: LFPR
  let workforce count turtles with [age > 20 and age < 60]                                            ;; older than 20 and younger than 60
  repeat (workforce * Labour_force_absorption_rate / 100) [ask one-of turtles with [age > 20 and age < 60 and occupation = 0 ] [
    set occupation 1
    ]
  ]
  ;; creating essential workers
  repeat essential_workers / 100 * count turtles with [occupation = 1] [ask one-of turtles with [age > 20 and age < 60 and occupation = 1] [
    set occupation 2
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; DISEASE CHARACTERISITICS ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to initial-sick                                                                                       ;; infect initial number of turtles
  repeat initial_number_infected [ ask one-of turtles with [state = "susceptible"] [                               ;; make (initial_number_infected) turtles sick/infected
    set state "infected"
    set color red
    set t1 0                                                                                                       ;; time contagion starts
    set t2 (Recovery_min + random (Recovery_max - Recovery_min + 1) )                                              ;; time contagion ends / recover
    set t3 (t2 + Immunity_min + random (Immunity_max - Immunity_min + 1) )                                         ;; time susceptible again
    set c 0                                                                                                        ;; non-critical infection
    ]
  ]
end

to create-infect-matrix                                                                               ;; create the matrix of transition probabilities according to user specification

   let i 0                                  ;; dummy variable
   set infect_matrix matrix:from-row-list [

;; matrix skeleton
    [1 1 1 1]                               ;; rows represent age groups (there are 9)
    [1 1 1 1]                               ;; 1st column: probability of infection/transmission if exposed
    [1 1 1 1]                               ;; 2nd column: probability of symptopmatic infection given infected
    [1 1 1 1]                               ;; 3rd column: probability of critical condition given symptomatic infection
    [1 1 1 1]                               ;; 4th column: probability of death given critical condition
    [1 1 1 1]                               ;; all are percentages
    [1 1 1 1]
    [1 1 1 1]
    [1 1 1 1]

  ]

    ;; probability infected given contact
    matrix:set infect_matrix 0 0 trans_0y_to_9y
    matrix:set infect_matrix 1 0 trans_10y_to_19y
    matrix:set infect_matrix 2 0 trans_20y_to_29y
    matrix:set infect_matrix 3 0 trans_30y_to_39y
    matrix:set infect_matrix 4 0 trans_40y_to_49y
    matrix:set infect_matrix 5 0 trans_50y_to_59y
    matrix:set infect_matrix 6 0 trans_60y_to_69y
    matrix:set infect_matrix 7 0 trans_70y_to_79y
    matrix:set infect_matrix 8 0 trans_80y+

    ;; probability symptomatic given infected
    matrix:set infect_matrix 0 1 symp_0y_to_9y
    matrix:set infect_matrix 1 1 symp_10y_to_19y
    matrix:set infect_matrix 2 1 symp_20y_to_29y
    matrix:set infect_matrix 3 1 symp_30y_to_39y
    matrix:set infect_matrix 4 1 symp_40y_to_49y
    matrix:set infect_matrix 5 1 symp_50y_to_59y
    matrix:set infect_matrix 6 1 symp_60y_to_69y
    matrix:set infect_matrix 7 1 symp_70y_to_79y
    matrix:set infect_matrix 8 1 symp_80y+

   ;; probability critical given symptomatic
   matrix:set infect_matrix 0 2 crit_0y_to_9y
   matrix:set infect_matrix 1 2 crit_10y_to_19y
   matrix:set infect_matrix 2 2 crit_20y_to_29y
   matrix:set infect_matrix 3 2 crit_30y_to_39y
   matrix:set infect_matrix 4 2 crit_40y_to_49y
   matrix:set infect_matrix 5 2 crit_50y_to_59y
   matrix:set infect_matrix 6 2 crit_60y_to_69y
   matrix:set infect_matrix 7 2 crit_70y_to_79y
   matrix:set infect_matrix 8 2 crit_80y+

  ;; probability dead given critial
   matrix:set infect_matrix 0 3 d_0y_to_9y
   matrix:set infect_matrix 1 3 d_10y_to_19y
   matrix:set infect_matrix 2 3 d_20y_to_29y
   matrix:set infect_matrix 3 3 d_30y_to_39y
   matrix:set infect_matrix 4 3 d_40y_to_49y
   matrix:set infect_matrix 5 3 d_50y_to_59y
   matrix:set infect_matrix 6 3 d_60y_to_69y
   matrix:set infect_matrix 7 3 d_70y_to_79y
   matrix:set infect_matrix 8 3 d_80y+

   set distance_to_infect 0
end

to spread-virus

  ask turtles with [condition = "not_infected"] [                                               ;; only susceptible turtles get infected

    let total_contacts count turtles in-radius distance_to_infect with [state = "infected" and newi = 0 and isolate = 0]                                        ;; count total contacts with infected turtles
    set probability_of_transmission matrix:get infect_matrix( (([age] of self + 1) / 10) - 1) 0         ;; Social distancing halves infection rate

    if SETUP_RSA_COVID-19_EXPERIENCE = false [
     if Social_distancing = "On" [set probability_of_transmission (probability_of_transmission * 0.3676)]
     if Social_distancing = "Off" [set probability_of_transmission (probability_of_transmission * 1 / 0.3676)]
    ]

    if SETUP_RSA_COVID-19_EXPERIENCE = true [
      if Social_distancing = "On" [set probability_of_transmission (probability_of_transmission * 0.3676)]
      ;; Beta variant
      if Social_distancing = "On" and ticks > 254 and ticks < 396 [set probability_of_transmission (probability_of_transmission * 1.5625)]
      ;; Delta variant
      if Social_distancing = "On" and ticks >= 396 [set probability_of_transmission (probability_of_transmission * 3) ]
    ]

    if total_contacts > 0 [                                                                      ;; if in contact with at least one infected turtle
      foreach (range 0 total_contacts) [                                                         ;; forloop for getting infection from each contact
        if random 10000 / 100 < probability_of_transmission [
          set condition "infected"
        ]
      ]
      if condition = "infected" [                                                                ;; if infected from contacts with infected turtle
        let prob_asymptomatic (1 - matrix:get infect_matrix( (([age] of self + 1) / 10) - 1) 1  );; probability of asymptomatic infection

        ;; Path 1: asymptomatic infection -> recover -> susceptible again
        ifelse random 100 < prob_asymptomatic [
          set t0 ticks                                                                           ;; time of infection
          set t1 (t0 + Latency_min + random (Latency_max - Latency_min + 1 ) )                   ;; time when contagion begins
          set t2 (t1 + Recovery_min + random (Recovery_max - Recovery_min + 1) )                 ;; time when contagion ends / recovery begins
          set t3 (t2 + immunity_min + random (Immunity_max - Immunity_min + 1) )                 ;; time when susceptible again
        ]

        ;; Path 2: symptomatic infection -> recover -> susceptible again
        [ let prob_critical matrix:get infect_matrix( (([age] of self + 1)/ 10) - 1) 2                ;; probability of becoming critical given symptomatic infection
          ifelse random 100 > prob_critical[
            set t0 ticks                                                                         ;; time of infection
            set t1 (t0 + latency_min + random (Latency_max - Latency_min + 1) )                  ;; time when contagion begins
            set t2 (t1 + Recovery_min + random (Recovery_max - Recovery_min + 1) )               ;; time when contagion ends / recovery begins
            set t3 (t2 + Immunity_min + random (Immunity_max - Immunity_min + 1) )               ;; time when susceptible again
          ]

          ;; Path 3: symptomatic infection -> critical -> recover -> susceptible again
          [ set c 1                                                                              ;; this is a critical infection
            let prob_death matrix:get infect_matrix( (([age] of self + 1) / 10) - 1) 3                 ;; probability of death given in critical condition
            ifelse random 100 > prob_death[
              set t0 ticks                                                                       ;; time of infection
              set t1 (t0 + Latency_min + random (Latency_max - Latency_min + 1) )                ;; time when contagion begins
              set t2 (t1 + Recovery_min + random (Recovery_max - Recovery_min + 1) )             ;; time when contagion ends and become critical / go to hospital
              set t3 (t2 + Critical_min + random (Critical_max - Critical_min + 1) )             ;; time when out of hospital / recovered
              set t4 (t3 + Immunity_min + random (Immunity_max - Immunity_min + 1) )             ;; time when susceptible again
            ]

            ;; Path 4: symptomatic infection -> critical -> dead
            [ set t0 ticks                                                                       ;; time of infection
              set t1 (t0 + Latency_min + random (Latency_max - Latency_min + 1) )                ;; time when contagion begins
              set t2 (t1 + Recovery_min + random (Recovery_max - Recovery_min + 1) )             ;; time when contagion ends and become critical / go to hospital
              set t3 (t2 + Critical_min + random (Critical_max - Critical_min + 1) )             ;; time of death
            ]
          ]
        ]
      ]
    ]
  ]
end

to change-state                                                                                       ;; change state according to time and infection parameters
  ask turtles with [state != "inoculated"][


    ;; non-critical infections (symptomatics + asymptomatics)
    if ticks = [t1] of self and c = 0 [set state "infected"]                                            ;; contagion starts / in "infected" state for non-critical infected turtle

    if ticks = [t2] of self and c = 0 [set state "recovered"]                                           ;; contagion ends / recovered = immunity begins for non-critical infected turtle

    if ticks = [t3] of self and c = 0 [                                                                 ;; immunity ends / back into susceptible state
      set state "susceptible"
      set condition "not_infected"
      set t0 0
      set t1 0
      set t2 0
      set t3 0
      set t4 0
      set isolate 0
    ]


    ;; critical infections
    if ticks = [t1] of self and c = 1 [set state "infected"]                                            ;; contagion starts / in "infected" state for critical-infection turtle

    if ticks = [t2] of self and c = 1 [set state "critical"]                                            ;; contagion ends and into critical state (hospitalization) for critical-infection turtle

    if ticks = [t3] of self and c = 1 and t4 != 0 [set state "recovered"]                               ;; recover (discharged from hospital)

    if ticks = [t3] of self and c = 1 and t4 = 0 [die]                                                  ;; turtle dies in hospital

    if ticks = [t4] of self and c = 1 [                                                                 ;; immunity ends for turtle who was discharged from hospital
      set state "susceptible"
      set condition "not_infected"
      set t0 0
      set t1 0
      set t2 0
      set t3 0
      set t4 0
      set c 0
      set isolate 0
    ]

    if newi = 1 and ticks = [t2] of self and c = 0 [
      set state "susceptible"
      set condition "not_infected"
      set t0 0
      set t1 0
      set t2 0
      set t3 0
      set t4 0
      set newi 0
      set isolate 0
     ]

    if newi = 1 and ticks = [t3] of self and c = 1 and t4 != 0 [
      set state "susceptible"
      set condition "not_infected"
      set t0 0
      set t1 0
      set t2 0
      set t3 0
      set t4 0
      set newi 0
      set isolate 0
    ]
  ]

end

to color-change                                                                                       ;; change color according to state - visual representation
  ask turtles [
    if state = "susceptible" [set color blue - 10]
    if state = "infected" [set color orange]
    if state = "recovered" [set color green]
    if state = "critical" [set color red]
    if isolate = 1 [set color white]
  ]
end

to inoculate                                                                                          ;; can't inoculate while in a run FIX????

  repeat count turtles * inoculation_percentage / 100 [ask one-of turtles with [state != "inoculation"] [           ;; inoculate turtles according to user specified percentage
    set state "inoculated"
    set condition ""
    set color pink
    ]
  ]
end

to-report virus-gone?                                                                                 ;; if true is reported then program stops
  let s count turtles with [state = "susceptible" and condition = "not_infected"]                                   ;; number of turtles in 'susceptible' state
  let i count turtles with [state = "inoculated"]                                                                   ;; number of turtles in 'inoculation' state
  let r count turtles with [state = "recovered"]                                                                    ;; number of turtles in "recovered" state

  ifelse (s + i + r = count turtles) [report true]                                                                  ;; reports 'true' if all turtles in either one of above states
   [report false]                                                                                                   ;; otherwise false
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; TURTLE ACTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go-home                                                                                            ;; after shopping for adults (but used for everyone)
  ask turtles [
    setxy homex homey
  ]

end

to go-to-school                                                                                       ;; Turtles with ages < 20 go to school
  ask turtles with [age < 20] [

  if Schools = "5 days a week" [                                                     ;; School Mondays to Fridays
    ifelse (ticks mod 7 = 6 or ticks mod 7 = 0) [
     setxy homex homey
    ]
    [ setxy schoolx schooly]
  ]

    if Schools = "4 days a week" [
      ifelse (ticks mod 7 = 6 or ticks mod 7 = 0 or ticks mod 7 = 3) [                                     ;; No school on Wednesdays, Saturdays or Sundays
      setxy homex homey
      ]
      [setxy schoolx schooly]
    ]

    if Schools = "3 days a week" [
      ifelse (ticks mod 7 = 6 or ticks mod 7 = 0 or ticks mod 7 = 2 or ticks mod 7 = 4) [                   ;; Only school on Mondays, Wednesdays and Fridays
        setxy homex homey
      ]
      [setxy schoolx schooly]
    ]

    if Schools = "2 days a week" [                                                   ;; Only school on Tuesdays and Thursdays
      ifelse (ticks mod 7 = 2 or ticks mod 7 = 4) [
        setxy schoolx schooly
      ]
      [setxy homex homey]
    ]

    if Schools = "1 day a week" [                                                    ;; Only school on Thursdays
      ifelse ticks mod 7 = 4 [
      setxy schoolx schooly
      ]
      [setxy homex homey]
    ]

    if Schools = "Closed"  [                                                            ;; Schools are closed
      setxy homex homey
  ]


  ]
end

to go-to-work                                                                                         ;; workforce go to work

  ;; essential workers always work 5 days a week (Monday to Friday)
  ask turtles with [ occupation = 2 ] [
    ifelse ticks mod 7 = 6 or ticks mod 7 = 0 [
      setxy homex homey
    ]
    [setxy workx worky]
  ]

  ;; non-essential workers
  ask turtles with [occupation = 1] [

    if Workplaces = "5 days a week" [                                       ;; work Mondays to Fridays
      ifelse ticks mod 7 = 6 or ticks mod 7 = 0 [
        setxy homex homey
      ]
      [setxy workx worky]
    ]

    if Workplaces = "4 days a week" [                                       ;; No work on Wednesdays, Saturdays or Sundays
      ifelse ticks mod 7 = 6 or ticks mod 7 = 0 or ticks mod 7 = 3 [
        setxy homex homey
      ]
      [setxy workx worky]
    ]

    if Workplaces = "3 days a week" [                                       ;; Only work on Mondays, Wednesdays and Fridays
      ifelse ticks mod 7 = 6 or ticks mod 7 = 0 or ticks mod 7 = 2 or ticks mod 7 = 4 [
        setxy homex homey
      ]
      [setxy workx worky]
    ]

    if Workplaces = "2 days a week" [                                       ;; Only work on Tuesdays and Thursdays
      ifelse ticks mod 7 = 2 or ticks mod 7 = 4 [
        setxy workx worky
      ]
      [setxy homex homey]
    ]

    if Workplaces = "1 day a week" [                                         ;; Only work on Thursdays
      ifelse ticks mod 7 = 4 [
        setxy workx worky
      ]
      [setxy homex homey]
    ]

    if Workplaces = "Only essential workers" [                                 ;; don't go to work
      setxy homex homey
    ]
  ]

end

to go-to-local-shop                                                                                   ;; turtles over the age of 20 go shopping

  ;; local shop visits
  ask turtles with [age > 20] [

    if Shop_visits = "3 times per week" [
      if random 100 < 33 [setxy localx localy]
    ]


    if Shop_visits = "2 times per week" [
      if random 100 < 22 [setxy localx localy]
   ]

    if Shop_visits = "Once per week" [
      if random 100 < 11 [setxy localx localy]
   ]
  ]
end

to go-to-mall
  ask turtles with [age > 20 and xcor != localx] [

    if Shop_visits = "3 times per week" [
      if ticks mod 7 = 6 and random 100 < 33 [setxy mallx mally]
    ]

    if Shop_visits = "2 times per week" [
      if ticks mod 7 = 6 and random 100 < 22 [setxy mallx mally]
  ]

    if Shop_visits = "Once per week" [
      if ticks mod 7 = 6 and random 100 < 11 [setxy mallx mally]
    ]
  ]

end

to go-to-gathering

  ask turtles [

    ifelse Socialize = "Stay at home" [setxy homex homey]

    [
    if Socialize = "3 times per week"  [
        if ticks mod 7 = 5 [setxy socialx1 socialy1]
        if ticks mod 7 = 6 [setxy socialx2 socialy2]
        if ticks mod 7 = 0 [setxy socialx3 socialy3]
      ]

    if Socialize = "2 times per week" [
        if ticks mod 7 = 6 [setxy socialx1 socialy1]
        if ticks mod 7 = 0 [setxy socialx2 socialy2]
      ]

      if Socialize = "Once per week" [
        if ticks mod 7 = 6 [setxy socialx1 socialy1]
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; CREATE PLACES OF INTERACTIONS ;;;;;;;;;;;;;                            {OUTSIDE OF HOME ENVIRONMENT)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to create-workplaces

  while [count turtles with [occupation != 0 and workx = 0] > number_of_contacts_at_work_per_day * 2] [

    let x_cor random-xcor
    let y_cor random-ycor

    repeat random-poisson number_of_contacts_at_work_per_day [
      ask one-of turtles with [workx = 0 and occupation != 0] [
        set workx x_cor
        set worky y_cor]
    ]
  ]

  if count turtles with [occupation != 0 and workx = 0] <= number_of_contacts_at_work_per_day * 2 [
    let x_cor random-xcor
    let y_cor random-ycor
    ask turtles with [occupation != 0 and workx = 0] [
      set workx x_cor
      set worky y_cor
   ]
  ]

end

to create-schools

  while [count turtles with [age < 20 and schoolx = 0] > number_of_contacts_per_school_day * 2] [

    let x_cor random-xcor
    let y_cor random-ycor

    repeat random-poisson number_of_contacts_per_school_day [
      ask one-of turtles with [age < 20 and schoolx = 0] [
        set schoolx x_cor
        set schooly y_cor]
    ]
  ]

  if count turtles with [age < 20 and schoolx = 0] <= number_of_contacts_at_work_per_day * 2 [
    let x_cor random-xcor
    let y_cor random-ycor
    ask turtles with [age < 20 and schoolx = 0] [
    set schooly x_cor
    set schoolx y_cor
   ]
  ]

end

to create-shops

  while [count turtles with [age > 20 and localx = 0] > number_of_contacts_per_local_shop_visit * (1 / 0.33) * 2] [
    let x_cor random-xcor
    let y_cor random-ycor

    repeat (1 / 0.33) * number_of_contacts_per_local_shop_visit [
      ask one-of turtles with [age > 20 and localx = 0] [
      set localx x_cor
      set localy y_cor
      ]
    ]
  ]

  if count turtles with [age > 20 and localx = 0] <= number_of_contacts_per_local_shop_visit  * (1 / 0.33) * 2 [
    let x_cor random-xcor
    let y_cor random-ycor
    ask turtles with [age > 20 and localx = 0] [
    set localx x_cor
    set localy y_cor
    ]
  ]
end

to create-malls

  while [count turtles with [age > 20 and mallx = 0] > number_of_contacts_per_shopping_mall_visit * (1 / 0.33) * 2] [
    let x_cor random-xcor
    let y_cor random-ycor

    repeat (1 / 0.33) * number_of_contacts_per_shopping_mall_visit [
      ask one-of turtles with [age > 20 and mallx = 0] [
      set mallx x_cor
      set mally y_cor
      ]
    ]
  ]

  if count turtles with [age > 20 and mallx = 0] <= number_of_contacts_per_shopping_mall_visit * (1 / 0.33) * 2 [
    let x_cor random-xcor
    let y_cor random-ycor
    ask turtles with [age > 20 and mallx = 0] [
      set mallx x_cor
      set mally y_cor
    ]
  ]
end

to create-gathering-spots1

  ;; gathering place number 1
  let x_cord 0
  let total (number_of_families_at_gatherings)
  set total (floor total)

  while [ count turtles with [socialx1 = 0] > total * 4 ] [
    let x_cor random-xcor
    let y_cor random-ycor

    repeat total [ask one-of turtles with [socialx1 = 0] [
      set x_cord homex
      set socialx1 x_cor
      set socialy1 y_cor
    ]

      if count turtles with [homex = x_cord] > 0 [
    ask turtles with [homex = x_cord] [
      set socialx1 x_cor
      set socialy1 y_cor
    ]
      ]
    ]
  ]

  let x_cor random-xcor
  let y_cor random-ycor
  ask turtles with [socialx1 = 0] [
    set socialx1 x_cor
    set socialy1 y_cor
  ]

end

to create-gathering-spots2

  ;; gathering place number 2
  let x_cord 0
  let total (number_of_families_at_gatherings)
  set total (floor total)

  while [ count turtles with [socialx2 = 0] > total * 4] [
    let x_cor random-xcor
    let y_cor random-ycor

    repeat total [ask one-of turtles with [socialx2 = 0] [
      set x_cord homex
      set socialx2 x_cor
      set socialy2 y_cor
    ]
      if count turtles with [homex = x_cord] > 0 [
    ask turtles with [homex = x_cord] [
      set socialx2 x_cor
      set socialy2 y_cor
     ]
    ]
    ]
  ]

  let x_cor random-xcor
  let y_cor random-ycor
  ask turtles with [socialx2 = 0] [
    set socialx2 x_cor
    set socialy2 y_cor
  ]

end

to create-gathering-spots3

  ;; gathering place number 3
  let x_cord 0
  let total (number_of_families_at_gatherings)
  set total (floor total)

  while [ count turtles with [socialx3 = 0] > total * 4] [
    let x_cor random-xcor
    let y_cor random-ycor

    repeat total [ask one-of turtles with [socialx3 = 0] [
      set x_cord homex
      set socialx3 x_cor
      set socialy3 y_cor
    ]

      if count turtles with [homex = x_cord] > 0 [
    ask turtles with [homex = x_cord] [
      set socialx3 x_cor
      set socialy3 y_cor
     ]
    ]
    ]
  ]

  let x_cor random-xcor
  let y_cor random-ycor
  ask turtles with [socialx3 = 0] [
    set socialx3 x_cor
    set socialy3 y_cor
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CODE FOR BUTTONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to zoom-in
  ask turtles [
    let i [size] of self
    set size i + 1
  ]
end

to zoom-out
  ask turtles [
    let i [size] of self
    set size i - 1
  ]
end

to default                                                                                            ;; set all input parameters to default (i.e. for South Africa and COVID-19)

  ;; population size
  set initial_population 100000

  ;; age demograhics
  set _0y_to_9y   19.6
  set _10y_to_19y 17.6
  set _20y_to_29y 17.1
  set _30y_to_39y 17.2
  set _40y_to_49y 11.9
  set _50y_to_59y 8.3
  set _60y_to_69y 5.4
  set _70y_to_79y 2.5
  set _80y+       0.4

  ;; households demographics
  set number_of_households_per_100_people 30
  set single_member_households 24
  set two/three_member_households 37
  set four/five_member_households 25
  set six_or_more_member_households 14
  set South_Africa_Household_Composition true

  ;; Labour force
  set labour_force_absorption_rate 42
  set essential_workers 46

  ;; number of contacts
  set number_of_contacts_per_school_day 27
  set number_of_contacts_at_work_per_day 8
  set number_of_contacts_per_shopping_mall_visit 50
  set number_of_contacts_per_local_shop_visit 7
  set number_of_families_at_gatherings 2

  ;; permanent immunity beforehand
  set inoculation_percentage 0

  ;;some COVID-19 characterisitics
  set initial_number_infected 5
  set Latency_min 2
  set Latency_max 4
  set Recovery_min 10
  set Recovery_max 14
  set Critical_min 4
  set Critical_max 12
  set immunity_min 700
  set immunity_max 700

  ;;probability infected given contact
  set trans_0y_to_9y   1.36
  set trans_10y_to_19y 1.29
  set trans_20y_to_29y 2.69
  set trans_30y_to_39y 2.92
  set trans_40y_to_49y 2.72
  set trans_50y_to_59y 2.79
  set trans_60y_to_69y 2.99
  set trans_70y_to_79y 2.52
  set trans_80y+       2.52

  ;;probability symptomatic given infected
  set symp_0y_to_9y   29
  set symp_10y_to_19y 21
  set symp_20y_to_29y 27
  set symp_30y_to_39y 33
  set symp_40y_to_49y 40
  set symp_50y_to_59y 49
  set symp_60y_to_69y 63
  set symp_70y_to_79y 69
  set symp_80y+       69

  ;;probability critical given symptomatic
  set crit_0y_to_9y   0.1
  set crit_10y_to_19y 0.1
  set crit_20y_to_29y 0.38
  set crit_30y_to_39y 0.38
  set crit_40y_to_49y 0.8
  set crit_50y_to_59y 0.8
  set crit_60y_to_69y 4.34
  set crit_70y_to_79y 4.34
  set crit_80y+       18.35

  ;;probability death given critical
  set d_0y_to_9y   0.05
  set d_10y_to_19y 2.1
  set d_20y_to_29y 5.3
  set d_30y_to_39y 12.6
  set d_40y_to_49y 22.1
  set d_50y_to_59y 30.3
  set d_60y_to_69y 56.6
  set d_70y_to_79y 65.3
  set d_80y+       76.5

end

to check-lockdown?                                                                                    ;; to set lockdown parameters

  if Lockdown = "Alert level 5"  [

    set schools  "Closed"
    set workplaces  "Only essential workers"
    set socialize  "Stay at home"
    set shop_visits  "Once per week"
    set social_distancing "On"

  ]

  if Lockdown = "Alert level 4" [

    set schools "1 day a week"
    set workplaces "1 day a week"
    set socialize "Stay at home"
    set shop_visits "Once per week"
    set social_distancing "On"

  ]

   if Lockdown = "Alert level 3" [

    set schools "2 days a week"
    set workplaces "2 days a week"
    set socialize "Stay at home"
    set shop_visits "Once per week"
    set social_distancing "On"

  ]

  if Lockdown = "Alert level 2" [

    set schools "3 days a week"
    set workplaces "3 days a week"
    set socialize "Once per week"
    set shop_visits "2 times per week"
    set social_distancing "On"

  ]

  if Lockdown = "Alert level 1" [

    set schools "4 days a week"
    set workplaces "4 days a week"
    set socialize "2 times per week"
    set shop_visits "2 times per week"
    set social_distancing "On"

  ]

end

to insert-new-virus                                                                                   ;; not quite finished; what about those that are currently infected? = introduce new parameter?

  ask turtles with [state != "critical" and state != "infected"][
    set state "susceptible"
    set condition "not_infected"
    set t0 0
    set t1 0
    set t2 0
    set t3 0
    set t4 0
  ]
  ask turtles with [state = "critical" or state = "infected"] [
    set newi 1
  ]

  create-infect-matrix
  initial-sick
  inoculate

end

to insert-infected

  if count turtles with [state = "susceptible"] > How_many_new? [
    repeat How_many_new? [
      ask one-of turtles with [state = "susceptible"] [
        set state "infected"
        set color red
        set t1 ticks                                                                                          ;; time contagion starts
        set t2 (t1 + Recovery_min + random (Recovery_max - Recovery_min + 1) )                                ;; time contagion ends / recover
        set t3 (t2 + Immunity_min + random (Immunity_max - Immunity_min + 1) )
      ]
    ]
  ]
end

to run-RSA-Covid-19

  if ticks >= 0 and ticks <= 25 [
   set Schools "5 days a week"
   set Workplaces "5 days a week"
   set Shop_visits "3 times per week"
   set Socialize "3 times per week"
   set Social_distancing "Off"
   set Lockdown "None"
   set Likelihood_to_self_isolate 0
   set Self_isolation true
  ]

  if ticks > 25 and ticks <= 61 [
    set Lockdown "Alert level 5"
    set Likelihood_to_self_isolate 5
  ]

  if ticks > 61 and ticks <= 92 [
    set Lockdown "Alert level 4"
    set Likelihood_to_self_isolate 5
  ]

  if ticks > 92 and ticks <= 170 [
    set Lockdown "Alert level 3"
    set Likelihood_to_self_isolate 5
  ]

  if ticks > 170 and ticks <= 204 [
    set Lockdown "Alert level 2"
    ;;set Likelihood_to_self_isolate 50
  ]

  if ticks > 204 and ticks <= 303 [
    set Lockdown "Alert level 1"
    ;;set Likelihood_to_self_isolate 35
  ]

  if ticks > 303 and ticks <= 365 [
    set Lockdown "Alert level 3"
    ;;set Likelihood_to_self_isolate 20
  ]

  if ticks > 365 and ticks <= 456 [
    set Lockdown "Alert level 1"
    ;;set Likelihood_to_self_isolate 5
  ]

  if ticks > 456 and ticks <= 472 [
    set Lockdown "Alert level 2"
    ;;set Likelihood_to_self_isolate 7
  ]

  if ticks > 472 and ticks <=  483[
    set Lockdown "Alert level 3"
    ;;set Likelihood_to_self_isolate 12
  ]

  if ticks > 483 [set Lockdown "Alert level 4"]

end

to testing

  let prop (daily_testing_of_population * Initial_Population)
  set prop precision prop 0

  set daily_new_cases count turtles with [test = 2]

  repeat prop [ask one-of turtles with [state != "critical" or state != "inoculated" or test != 2] [
    if [state] of self = "infected" [set test 2]
    if [condition] of self = "not_infected" [set test 1]
    ]
  ]
  set daily_new_cases (count turtles with [test = 2] - daily_new_cases)
  if daily_new_cases < 0 [set daily_new_cases (daily_new_cases * -1)]
end

to self-isolate

  ask turtles with [state = "infected" and ticks = t1] [
    if random 100 < Likelihood_to_self_isolate [set isolate 1]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REPORTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report number-infected
  report count turtles with [state = "infected"]
end

to-report number-dead
  report Initial_population - count turtles
end

to-report number-critical
  report count turtles with [state = "critical"]
end

to-report number-recovered
  report count turtles with [state = "recovered"]
end

to-report number-susceptible
  report count turtles with [state = "susceptible"]
end

to-report number-inoculated
  report count turtles with [state = "inoculated"]
end
@#$#@#$#@
GRAPHICS-WINDOW
246
49
711
515
-1
-1
2.274
1
10
1
1
1
0
1
1
1
-100
100
-100
100
1
1
1
ticks
30.0

INPUTBOX
1524
78
1697
138
Initial_Population
100000.0
1
0
Number

SLIDER
1525
150
1697
183
_0y_to_9y
_0y_to_9y
0
100
19.6
0.1
1
%
HORIZONTAL

SLIDER
1525
195
1697
228
_10y_to_19y
_10y_to_19y
0
100 - _0y_to_9y
17.6
0.1
1
%
HORIZONTAL

SLIDER
1525
239
1698
272
_20y_to_29y
_20y_to_29y
0
100 - _0y_to_9y - _10y_to_19y
17.1
0.1
1
%
HORIZONTAL

SLIDER
1525
281
1698
314
_30y_to_39y
_30y_to_39y
0
100 - _0y_to_9y - _10y_to_19y - _20y_to_29y
17.2
0.1
1
%
HORIZONTAL

SLIDER
1525
363
1696
396
_50y_to_59y
_50y_to_59y
0
100 - _0y_to_9y - _10y_to_19y - _20y_to_29y - _30y_to_39y - _40y_to_49y
8.3
0.1
1
%
HORIZONTAL

SLIDER
1525
322
1698
355
_40y_to_49y
_40y_to_49y
0
100 - _0y_to_9y - _10y_to_19y - _20y_to_29y - _30y_to_39y
11.9
0.1
1
%
HORIZONTAL

SLIDER
1525
404
1697
437
_60y_to_69y
_60y_to_69y
0
100 - _0y_to_9y - _10y_to_19y - _20y_to_29y - _30y_to_39y - _40y_to_49y - _50y_to_59y
5.4
0.01
1
%
HORIZONTAL

SLIDER
1526
444
1698
477
_70y_to_79y
_70y_to_79y
0
100 - _0y_to_9y - _10y_to_19y - _20y_to_29y - _30y_to_39y - _40y_to_49y - _50y_to_59y - _60y_to_69y
2.5
0.1
1
%
HORIZONTAL

BUTTON
45
96
193
160
SETUP MODEL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1527
488
1699
521
_80y+
_80y+
0
100 - _0y_to_9y - _10y_to_19y - _20y_to_29y - _30y_to_39y - _40y_to_49y - _50y_to_59y - _60y_to_69y - _70y_to_79y
0.4
0.1
1
%
HORIZONTAL

BUTTON
45
185
195
251
RUN / PAUSE  MODEL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
12
10
1048
50
BEKEZELA - A Spatial Agent-Based Model of the COVID-19 Pandemic
25
0.0
1

TEXTBOX
930
10
1415
52
                         Output\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
25
0.0
1

TEXTBOX
1448
10
1482
640
+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+
11
0.0
1

TEXTBOX
1985
33
2073
75
Input
25
0.0
1

TEXTBOX
1490
53
1738
88
Population and Age demographics
14
0.0
1

TEXTBOX
1779
63
1966
97
Household demographics
14
0.0
1

SLIDER
1749
139
1985
172
Single_member_households
Single_member_households
0
100
24.0
1
1
%
HORIZONTAL

SLIDER
1748
192
1987
225
Two/Three_member_households
Two/Three_member_households
0
100 - Single_member_households
37.0
1
1
%
HORIZONTAL

SLIDER
1750
243
1985
276
Four/Five_member_households
Four/Five_member_households
0
100 - Single_member_households - two/three_member_households
25.0
1
1
%
HORIZONTAL

SLIDER
1748
291
1985
324
Six_or_more_member_households
Six_or_more_member_households
0
100 - single_member_households - two/three_member_households - four/five_member_households
14.0
1
1
%
HORIZONTAL

BUTTON
46
280
195
349
ZOOM IN
zoom-in
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
46
383
194
452
ZOOM OUT
zoom-out
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
918
48
1418
509
ACTIVE CASES OF COVID-19
DAYS SINCE FIRST INFECTION
NUMBER OF PEOPLE 
0.0
100.0
0.0
50.0
true
false
"" ""
PENS
"Infected" 1.0 0 -5298144 true "" "plot count turtles with [state = \"infected\"]"

BUTTON
2049
335
2349
447
DEFAULT PARAMETERS FOR SOUTH AFRICA
default
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
736
83
887
123
Change behaviours
16
0.0
1

TEXTBOX
1837
401
1924
421
Labour force 
14
0.0
1

SLIDER
1748
429
1992
462
Labour_force_absorption_rate
Labour_force_absorption_rate
0
100
42.0
1
1
%
HORIZONTAL

SLIDER
1749
479
1992
512
Essential_workers
Essential_workers
0
100
46.0
1
1
%
HORIZONTAL

SLIDER
2049
81
2349
114
number_of_contacts_per_school_day
number_of_contacts_per_school_day
0
100
27.0
1
1
NIL
HORIZONTAL

SLIDER
2048
130
2349
163
number_of_contacts_at_work_per_day
number_of_contacts_at_work_per_day
0
50
8.0
1
1
NIL
HORIZONTAL

SLIDER
2049
226
2351
259
number_of_contacts_per_local_shop_visit
number_of_contacts_per_local_shop_visit
0
50
7.0
1
1
NIL
HORIZONTAL

SLIDER
2049
179
2351
212
number_of_contacts_per_shopping_mall_visit
number_of_contacts_per_shopping_mall_visit
0
100
50.0
1
1
NIL
HORIZONTAL

TEXTBOX
2133
56
2283
74
Number of contacts
14
0.0
1

INPUTBOX
2429
77
2559
137
Initial_number_infected
5.0
1
0
Number

SLIDER
2090
487
2294
520
Inoculation_percentage
Inoculation_percentage
0
100
0.0
1
1
%
HORIZONTAL

CHOOSER
745
113
881
158
Schools
Schools
"5 days a week" "4 days a week" "3 days a week" "2 days a week" "1 day a week" "Closed"
4

CHOOSER
746
172
883
217
Workplaces
Workplaces
"5 days a week" "4 days a week" "3 days a week" "2 days a week" "1 day a week" "Only essential workers"
4

CHOOSER
747
229
883
274
Shop_visits
Shop_visits
"3 times per week" "2 times per week" "Once per week"
2

CHOOSER
748
287
885
332
Socialize
Socialize
"3 times per week" "2 times per week" "Once per week" "Stay at home"
3

CHOOSER
748
347
885
392
Social_distancing
Social_distancing
"On" "Off"
0

TEXTBOX
2469
54
2563
73
Initial Infections
14
0.0
1

TEXTBOX
2631
109
2781
127
NIL
11
0.0
1

TEXTBOX
2468
146
2560
164
Latency period
14
0.0
1

TEXTBOX
2428
269
2578
287
NIL
11
0.0
1

TEXTBOX
2441
239
2571
257
Time to recovery
14
0.0
1

TEXTBOX
2449
332
2569
350
Time in critical state
14
0.0
1

TEXTBOX
2427
425
2577
443
Temporary Immunity
14
0.0
1

TEXTBOX
2125
463
2333
481
Permanent immunity beforehand
14
0.0
1

SLIDER
2640
82
2812
115
Trans_0y_to_9y
Trans_0y_to_9y
0
10
1.36
0.01
1
%
HORIZONTAL

SLIDER
2641
125
2813
158
Trans_10y_to_19y
Trans_10y_to_19y
0
10
1.29
0.01
1
%
HORIZONTAL

SLIDER
2641
168
2813
201
Trans_20y_to_29y
Trans_20y_to_29y
0
10
2.69
0.01
1
%
HORIZONTAL

SLIDER
2641
213
2813
246
Trans_30y_to_39y
Trans_30y_to_39y
0
10
2.92
0.01
1
%
HORIZONTAL

SLIDER
2640
259
2812
292
Trans_40y_to_49y
Trans_40y_to_49y
0
10
2.72
0.01
1
%
HORIZONTAL

SLIDER
2639
306
2814
339
Trans_50y_to_59y
Trans_50y_to_59y
0
10
2.79
0.01
1
%
HORIZONTAL

SLIDER
2638
351
2814
384
Trans_60y_to_69y
Trans_60y_to_69y
0
10
2.99
0.01
1
%
HORIZONTAL

SLIDER
2638
395
2815
428
Trans_70y_to_79y
Trans_70y_to_79y
0
10
2.52
0.01
1
%
HORIZONTAL

SLIDER
2637
440
2815
473
Trans_80y+
Trans_80y+
0
10
2.52
0.01
1
%
HORIZONTAL

TEXTBOX
2612
53
2859
87
Probability: Infected given Contact
14
0.0
1

TEXTBOX
3000
93
3150
111
NIL
11
0.0
1

SLIDER
2914
82
3086
115
Symp_0y_to_9y
Symp_0y_to_9y
0
100
29.0
1
1
%
HORIZONTAL

SLIDER
2913
127
3085
160
Symp_10y_to_19y
Symp_10y_to_19y
0
100
21.0
1
1
%
HORIZONTAL

SLIDER
2914
171
3086
204
Symp_20y_to_29y
Symp_20y_to_29y
0
100
27.0
1
1
%
HORIZONTAL

SLIDER
2914
215
3086
248
Symp_30y_to_39y
Symp_30y_to_39y
0
100
33.0
1
1
%
HORIZONTAL

SLIDER
2914
257
3086
290
Symp_40y_to_49y
Symp_40y_to_49y
0
100
40.0
1
1
%
HORIZONTAL

SLIDER
2913
302
3085
335
Symp_50y_to_59y
Symp_50y_to_59y
0
100
49.0
1
1
%
HORIZONTAL

SLIDER
2912
347
3084
380
Symp_60y_to_69y
Symp_60y_to_69y
0
100
63.0
1
1
%
HORIZONTAL

SLIDER
2912
390
3084
423
Symp_70y_to_79y
Symp_70y_to_79y
0
100
69.0
1
1
%
HORIZONTAL

SLIDER
2910
436
3082
469
Symp_80y+
Symp_80y+
0
100
69.0
1
1
%
HORIZONTAL

TEXTBOX
2874
54
3151
89
Probability: Symptomatic given Infected
14
0.0
1

SLIDER
3196
84
3368
117
Crit_0y_to_9y
Crit_0y_to_9y
0
100
0.1
1
1
%
HORIZONTAL

SLIDER
3196
129
3368
162
Crit_10y_to_19y
Crit_10y_to_19y
0
100
0.1
1
1
%
HORIZONTAL

SLIDER
3195
174
3367
207
Crit_20y_to_29y
Crit_20y_to_29y
0
100
0.38
1
1
%
HORIZONTAL

SLIDER
3195
216
3367
249
Crit_30y_to_39y
Crit_30y_to_39y
0
100
0.38
1
1
%
HORIZONTAL

SLIDER
3195
259
3367
292
Crit_40y_to_49y
Crit_40y_to_49y
0
100
0.8
1
1
%
HORIZONTAL

SLIDER
3195
304
3367
337
Crit_50y_to_59y
Crit_50y_to_59y
0
100
0.8
1
1
%
HORIZONTAL

SLIDER
3195
347
3367
380
Crit_60y_to_69y
Crit_60y_to_69y
0
100
4.34
1
1
%
HORIZONTAL

SLIDER
3195
391
3367
424
Crit_70y_to_79y
Crit_70y_to_79y
0
100
4.34
1
1
%
HORIZONTAL

SLIDER
3194
435
3366
468
Crit_80y+
Crit_80y+
0
100
18.35
1
1
%
HORIZONTAL

TEXTBOX
3157
56
3431
79
Probability: Critical given Symptomatic
14
0.0
1

SLIDER
3446
85
3618
118
D_0y_to_9y
D_0y_to_9y
0
100
0.05
1
1
%
HORIZONTAL

SLIDER
3446
129
3618
162
D_10y_to_19y
D_10y_to_19y
0
100
2.1
1
1
%
HORIZONTAL

SLIDER
3446
174
3618
207
D_20y_to_29y
D_20y_to_29y
0
100
5.3
1
1
%
HORIZONTAL

SLIDER
3446
216
3618
249
D_30y_to_39y
D_30y_to_39y
0
100
12.6
1
1
%
HORIZONTAL

SLIDER
3446
260
3618
293
D_40y_to_49y
D_40y_to_49y
0
100
22.1
1
1
%
HORIZONTAL

SLIDER
3446
302
3618
335
D_50y_to_59y
D_50y_to_59y
0
100
30.3
1
1
%
HORIZONTAL

SLIDER
3447
346
3619
379
D_60y_to_69y
D_60y_to_69y
0
100
56.6
1
1
%
HORIZONTAL

SLIDER
3447
389
3619
422
D_70y_to_79y
D_70y_to_79y
0
100
65.3
1
1
%
HORIZONTAL

SLIDER
3447
433
3619
466
D_80y+
D_80y+
0
100
76.5
1
1
%
HORIZONTAL

TEXTBOX
3432
57
3648
92
Probability: Death given Critical
14
0.0
1

TEXTBOX
3723
10
3787
654
+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n
11
0.0
1

BUTTON
591
970
880
1034
 MAKE AGENTS INFECTED WITH NEW VIRUS
insert-new-virus
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1748
89
1986
122
number_of_households_per_100_people
number_of_households_per_100_people
0
100
30.0
1
1
NIL
HORIZONTAL

SLIDER
2049
272
2350
305
number_of_families_at_gatherings
number_of_families_at_gatherings
0
10
2.0
1
1
NIL
HORIZONTAL

BUTTON
169
770
351
833
MAKE AGENTS INFECTED
insert-infected
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
748
407
887
452
Lockdown
Lockdown
"None" "Alert level 1" "Alert level 2" "Alert level 3" "Alert level 4" "Alert level 5"
4

TEXTBOX
248
593
711
634
                           Advanced Features\n\n\n\n\n\n\n\n\n\n\n\n\n\n
19
0.0
1

TEXTBOX
80
669
479
712
Make agents infected with same virus or mutation
16
0.0
1

INPUTBOX
203
701
311
761
How_many_new?
20.0
1
0
Number

TEXTBOX
585
945
928
973
Pause model. set input. make agents infected. run model
11
0.0
1

TEXTBOX
547
919
947
943
Make agents infected with noval virus/mutation
16
0.0
1

TEXTBOX
556
680
926
706
Simulate South Africa's COVID-19 Experience
16
0.0
1

TEXTBOX
652
707
814
735
March 2020 - August 2021\n    Setup RSA. Setup. Run.
11
0.0
1

SWITCH
617
740
843
773
SETUP_RSA_COVID-19_EXPERIENCE
SETUP_RSA_COVID-19_EXPERIENCE
0
1
-1000

SWITCH
1749
342
1985
375
South_Africa_Household_Composition
South_Africa_Household_Composition
0
1
-1000

SWITCH
1464
713
1588
746
Do_Testing?
Do_Testing?
0
1
-1000

SLIDER
1418
752
1638
785
Daily_testing_of_population
Daily_testing_of_population
0
10
0.4
0.2
1
%
HORIZONTAL

TEXTBOX
1390
681
1669
701
Testing population for COVID-19
16
0.0
1

PLOT
1105
792
1952
1125
DAILY NEW CASES (REPORTED FROM TESTING POPULATION)
DAYS SINCE FIRST INFECTION
NUMBER OF  PEOPLE
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "plot daily_new_cases"

TEXTBOX
491
635
506
1153
+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n
11
0.0
1

TEXTBOX
35
873
964
891
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
11
0.0
1

SWITCH
196
945
327
978
Self_Isolation
Self_Isolation
0
1
-1000

SLIDER
162
1014
368
1047
Likelihood_to_self_isolate
Likelihood_to_self_isolate
0
100
5.0
1
1
%
HORIZONTAL

TEXTBOX
212
913
320
933
Self Isolation
16
0.0
1

TEXTBOX
198
993
340
1012
Probability to self isolate
12
0.0
1

TEXTBOX
35
1139
979
1157
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
11
0.0
1

TEXTBOX
35
635
3757
653
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
11
0.0
1

TEXTBOX
963
635
978
1153
+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n
11
0.0
1

TEXTBOX
35
635
50
1153
+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n
11
0.0
1

TEXTBOX
963
1139
3738
1196
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
11
0.0
1

TEXTBOX
3723
635
3812
1153
+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+
11
0.0
1

TEXTBOX
2091
635
2111
1139
+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n+\n
11
0.0
1

TEXTBOX
1448
10
3729
38
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
11
0.0
1

INPUTBOX
2497
167
2574
227
Latency_max
4.0
1
0
Number

INPUTBOX
2417
167
2495
227
Latency_min
2.0
1
0
Number

INPUTBOX
2413
260
2492
320
Recovery_min
10.0
1
0
Number

INPUTBOX
2494
260
2575
320
Recovery_max
14.0
1
0
Number

INPUTBOX
2416
354
2495
414
Critical_min
4.0
1
0
Number

INPUTBOX
2497
354
2575
414
Critical_max
12.0
1
0
Number

INPUTBOX
2415
446
2494
506
Immunity_min
700.0
1
0
Number

INPUTBOX
2496
446
2577
506
Immunity_max
700.0
1
0
Number

MONITOR
1001
517
1080
562
SUSCEPTIBLE
number-susceptible
17
1
11

MONITOR
1178
517
1253
562
RECOVERED
number-recovered
17
1
11

PLOT
2122
899
2901
1132
CRITICAL CASES OF COVID-19 / HOSPITALISATIONS (RED)
DAYS SINCE FIRST INFECTION
NUMBER OF PEOPLE
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -2674135 true "" "plot count turtles with [state = \"critical\"]"

MONITOR
1362
517
1418
562
DEATHS
number-dead
17
1
11

MONITOR
918
517
993
562
DAY NUMBER
TICKS
17
1
11

MONITOR
1086
517
1173
562
ACTIVE CASES
number-infected
17
1
11

MONITOR
1259
517
1358
562
CRITICAL CASES
number-critical
17
1
11

MONITOR
1315
571
1418
616
DEATHS PER MILLE
number-dead / Initial_Population * 1000
3
1
11

MONITOR
1246
571
1311
616
% IMMUNE
100 * ((number-inoculated + number-recovered) / count turtles)
2
1
11

MONITOR
1074
571
1164
616
% SUSCEPTIBLE
100 * (number-susceptible) / count turtles
2
1
11

MONITOR
1168
571
1242
616
% INFECTED
100 * (number-infected) / count turtles
2
1
11

MONITOR
917
571
1070
616
DATE
time:show time \"yyyy, MMMM d, EEEE\"
17
1
11

PLOT
2124
658
2903
888
ACTIVE CASES OF COVID-19 (ORANGE)
DAYS SINCE FIRST INFECTION
NUMBER OF PEOPLE
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -955883 true "" "plot count turtles with [state = \"infected\" or state = \"critical\"]"

PLOT
2917
900
3694
1132
CUMULATIVE DEATHS (BLACK)
DAYS SINCE FIRST INFECTION
NUMBER OF PEOPLE
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (Initial_Population - count turtles)"

PLOT
2916
659
3694
889
SUSCEPTIBLE (BLUE) AND RECOVERED/IMMUNE (GREEN)
DAYS SINCE FIRST INFECTION
NUMBER OF PEOPLE
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -13840069 true "" "plot count turtles with [state = \"recovered\" or state = \"inoculated\"]"
"pen-1" 1.0 0 -13791810 true "" "plot count turtles with [state = \"susceptible\"]"

TEXTBOX
2853
615
2967
643
MORE OUTPUT
16
0.0
1

TEXTBOX
155
845
375
863
Immunity from previous infections holds
11
0.0
1

TEXTBOX
657
1050
825
1068
No prior immunity to this virus
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
