type color = Red | Black
type 'a rbt = Node of color * 'a * 'a rbt * 'a rbt | Leaf

let rec balance (base2505, base2636) =
  (fun base2506 ->
    (fun base2514 ->
      (fun base2533 ->
        match base2505 with
        | Node (base2564, base2565, base2566, base2567) -> (
            match base2564 with
            | Red -> base2636 (Node (base2564, base2565, base2566, base2567))
            | Black -> (
                match base2566 with
                | Node (base2568, base2569, base2570, base2571) -> (
                    match base2568 with
                    | Red -> (
                        match base2570 with
                        | Node (base2572, base2573, base2574, base2575) -> (
                            match base2572 with
                            | Red ->
                                base2636
                                  (Node
                                     ( Red,
                                       base2569,
                                       Node (Black, base2573, base2574, base2575),
                                       Node (Black, base2565, base2571, base2567)
                                     ))
                            | Black ->
                                base2533
                                  ( base2571,
                                    base2567,
                                    (fun ( base2576,
                                           base2577,
                                           base2578,
                                           base2579,
                                           base2637 ) ->
                                      base2637
                                        (Node
                                           ( Red,
                                             base2576,
                                             Node
                                               ( Black,
                                                 base2569,
                                                 Node
                                                   ( base2572,
                                                     base2573,
                                                     base2574,
                                                     base2575 ),
                                                 base2577 ),
                                             Node
                                               ( Black,
                                                 base2565,
                                                 base2578,
                                                 base2579 ) ))),
                                    (fun ( base2580,
                                           base2581,
                                           base2582,
                                           base2583,
                                           base2584,
                                           base2638 ) ->
                                      base2638
                                        (Node
                                           ( Red,
                                             base2581,
                                             Node
                                               ( Black,
                                                 base2565,
                                                 Node
                                                   ( base2568,
                                                     base2569,
                                                     base2570,
                                                     base2571 ),
                                                 base2582 ),
                                             Node
                                               ( Black,
                                                 base2580,
                                                 base2583,
                                                 base2584 ) ))),
                                    (fun ( base2585,
                                           base2586,
                                           base2587,
                                           base2588,
                                           base2589,
                                           base2639 ) ->
                                      base2639
                                        (Node
                                           ( Red,
                                             base2585,
                                             Node
                                               ( Black,
                                                 base2565,
                                                 Node
                                                   ( base2568,
                                                     base2569,
                                                     base2570,
                                                     base2571 ),
                                                 base2586 ),
                                             Node
                                               ( Black,
                                                 base2587,
                                                 base2588,
                                                 base2589 ) ))),
                                    (fun base2640 ->
                                      base2640
                                        (Node
                                           ( base2564,
                                             base2565,
                                             base2566,
                                             base2567 ))),
                                    base2636 ))
                        | Leaf ->
                            base2533
                              ( base2571,
                                base2567,
                                (fun ( base2590,
                                       base2591,
                                       base2592,
                                       base2593,
                                       base2641 ) ->
                                  base2641
                                    (Node
                                       ( Red,
                                         base2590,
                                         Node (Black, base2569, Leaf, base2591),
                                         Node
                                           (Black, base2565, base2592, base2593)
                                       ))),
                                (fun ( base2594,
                                       base2595,
                                       base2596,
                                       base2597,
                                       base2598,
                                       base2642 ) ->
                                  base2642
                                    (Node
                                       ( Red,
                                         base2595,
                                         Node
                                           ( Black,
                                             base2565,
                                             Node
                                               ( base2568,
                                                 base2569,
                                                 base2570,
                                                 base2571 ),
                                             base2596 ),
                                         Node
                                           (Black, base2594, base2597, base2598)
                                       ))),
                                (fun ( base2599,
                                       base2600,
                                       base2601,
                                       base2602,
                                       base2603,
                                       base2643 ) ->
                                  base2643
                                    (Node
                                       ( Red,
                                         base2599,
                                         Node
                                           ( Black,
                                             base2565,
                                             Node
                                               ( base2568,
                                                 base2569,
                                                 base2570,
                                                 base2571 ),
                                             base2600 ),
                                         Node
                                           (Black, base2601, base2602, base2603)
                                       ))),
                                (fun base2644 ->
                                  base2644
                                    (Node
                                       (base2564, base2565, base2566, base2567))),
                                base2636 ))
                    | Black ->
                        base2514
                          ( base2567,
                            (fun ( base2604,
                                   base2605,
                                   base2606,
                                   base2607,
                                   base2608,
                                   base2645 ) ->
                              base2645
                                (Node
                                   ( Red,
                                     base2605,
                                     Node
                                       ( Black,
                                         base2565,
                                         Node
                                           ( base2568,
                                             base2569,
                                             base2570,
                                             base2571 ),
                                         base2606 ),
                                     Node (Black, base2604, base2607, base2608)
                                   ))),
                            (fun ( base2609,
                                   base2610,
                                   base2611,
                                   base2612,
                                   base2613,
                                   base2646 ) ->
                              base2646
                                (Node
                                   ( Red,
                                     base2609,
                                     Node
                                       ( Black,
                                         base2565,
                                         Node
                                           ( base2568,
                                             base2569,
                                             base2570,
                                             base2571 ),
                                         base2610 ),
                                     Node (Black, base2611, base2612, base2613)
                                   ))),
                            (fun base2647 ->
                              base2647
                                (Node (base2564, base2565, base2566, base2567))),
                            base2636 ))
                | Leaf ->
                    base2514
                      ( base2567,
                        (fun ( base2614,
                               base2615,
                               base2616,
                               base2617,
                               base2618,
                               base2648 ) ->
                          base2648
                            (Node
                               ( Red,
                                 base2615,
                                 Node (Black, base2565, Leaf, base2616),
                                 Node (Black, base2614, base2617, base2618) ))),
                        (fun ( base2619,
                               base2620,
                               base2621,
                               base2622,
                               base2623,
                               base2649 ) ->
                          base2649
                            (Node
                               ( Red,
                                 base2619,
                                 Node (Black, base2565, Leaf, base2620),
                                 Node (Black, base2621, base2622, base2623) ))),
                        (fun base2650 ->
                          base2650
                            (Node (base2564, base2565, base2566, base2567))),
                        base2636 )))
        | Leaf -> base2636 Leaf)
        (fun
          (base2534, base2535, base2536, base2537, base2538, base2539, base2651)
        ->
          match base2534 with
          | Node (base2540, base2541, base2542, base2543) -> (
              match base2540 with
              | Red ->
                  base2536 (base2541, base2542, base2543, base2535, base2651)
              | Black ->
                  base2514
                    ( base2535,
                      (fun ( base2544,
                             base2545,
                             base2546,
                             base2547,
                             base2548,
                             base2652 ) ->
                        base2537
                          ( base2544,
                            base2545,
                            base2546,
                            base2547,
                            base2548,
                            base2652 )),
                      (fun ( base2549,
                             base2550,
                             base2551,
                             base2552,
                             base2553,
                             base2653 ) ->
                        base2538
                          ( base2549,
                            base2550,
                            base2551,
                            base2552,
                            base2553,
                            base2653 )),
                      (fun base2654 -> base2539 base2654),
                      base2651 ))
          | Leaf ->
              base2514
                ( base2535,
                  (fun ( base2554,
                         base2555,
                         base2556,
                         base2557,
                         base2558,
                         base2655 ) ->
                    base2537
                      ( base2554,
                        base2555,
                        base2556,
                        base2557,
                        base2558,
                        base2655 )),
                  (fun ( base2559,
                         base2560,
                         base2561,
                         base2562,
                         base2563,
                         base2656 ) ->
                    base2538
                      ( base2559,
                        base2560,
                        base2561,
                        base2562,
                        base2563,
                        base2656 )),
                  (fun base2657 -> base2539 base2657),
                  base2651 )))
      (fun (base2515, base2516, base2517, base2518, base2658) ->
        match base2515 with
        | Node (base2519, base2520, base2521, base2522) -> (
            match base2519 with
            | Red -> (
                match base2521 with
                | Node (base2523, base2524, base2525, base2526) -> (
                    match base2523 with
                    | Red ->
                        base2516
                          ( base2520,
                            base2524,
                            base2525,
                            base2526,
                            base2522,
                            base2658 )
                    | Black ->
                        base2506
                          ( base2522,
                            (fun (base2527, base2528, base2529, base2659) ->
                              base2517
                                ( base2520,
                                  Node (base2523, base2524, base2525, base2526),
                                  base2527,
                                  base2528,
                                  base2529,
                                  base2659 )),
                            (fun base2660 -> base2518 base2660),
                            base2658 ))
                | Leaf ->
                    base2506
                      ( base2522,
                        (fun (base2530, base2531, base2532, base2661) ->
                          base2517
                            ( base2520,
                              Leaf,
                              base2530,
                              base2531,
                              base2532,
                              base2661 )),
                        (fun base2662 -> base2518 base2662),
                        base2658 ))
            | Black -> base2518 base2658)
        | Leaf -> base2518 base2658))
    (fun (base2507, base2508, base2509, base2663) ->
      match base2507 with
      | Node (base2510, base2511, base2512, base2513) -> (
          match base2510 with
          | Red -> base2508 (base2511, base2512, base2513, base2663)
          | Black -> base2509 base2663)
      | Leaf -> base2509 base2663)

let rec insertAux (base2499, base2500, base2631) =
  match base2500 with
  | Node (base2501, base2502, base2503, base2504) ->
      (fun base2632 ->
        if base2632 then
          insertAux
            ( base2499,
              base2503,
              fun base2633 ->
                balance (Node (base2501, base2502, base2633, base2504), base2631)
            )
        else
          (fun base2634 ->
            if base2634 then
              insertAux
                ( base2499,
                  base2504,
                  fun base2635 ->
                    balance
                      (Node (base2501, base2502, base2503, base2635), base2631)
                )
            else base2631 (Node (base2501, base2502, base2503, base2504)))
            (base2499 > base2502))
        (base2499 < base2502)
  | Leaf -> base2631 (Node (Red, base2499, Leaf, Leaf))

let rec insert (base2493, base2494, base2629) =
  insertAux
    ( base2493,
      base2494,
      fun base2630 ->
        match base2630 with
        | Node (base2495, base2496, base2497, base2498) ->
            base2629 (Node (Black, base2496, base2497, base2498))
        | Leaf -> base2629 Leaf )

let rec rbtAux (base2492, base2625) =
  (fun base2626 ->
    if base2626 then base2625 Leaf
    else
      (fun base2627 ->
        rbtAux (base2627, fun base2628 -> insert (base2492, base2628, base2625)))
        (base2492 - 1))
    (base2492 == 0)

let rec main = rbtAux (100000, fun base2624 -> base2624)
