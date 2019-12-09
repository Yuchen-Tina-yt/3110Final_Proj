open Weapon

let dagger = "
       .---.
       |---|
       |---|
       |---|
   .---^ - ^---.
   :___________:
      |  |//|
      |  |//|
      |  |//|
      |  |//|
      |  |//|
      |  |//|
      |  |.-|
      |.-'**|
       \***/
        \*/
         V"
let sword = "
                    d@b
H@b                ,H@@__________________________________________________
H@@EEEEEEEEEEEEEH@@                                                   //
H@@EEEEEEEEEEEEEH@@                                                  //
H@@EEEEEEEEEEEEEH@@_________________________________________________//
H@P                `H@@
                    T@
"
let gun = "
 +--^----------,--------,-----,--------^-,
 | |||||||||   `--------'     |          O
 `+---------------------------^----------|
   `\_,---------,---------,--------------'
     / XXXXXX /'|       /'
    / XXXXXX /  `\    /'
   / XXXXXX /`-------'
  / XXXXXX /
 / XXXXXX /
(________(                
 `------' "
let ice_ball = "
  *                                                               *
           *                                                  *
                 ______  _____  _____  ______
                /      \/    /  \    \/      \   *
     *         /  \    / ___/    \___ \    /  \
              /    \  / /            \ \  /    \           *
             _\____ \ \ \     /\     / / / ____/_
            /   __ \ \ \ \    \/*   / / / / __   \
           /   /  \ \ \ \ \        / / / / /  \   \      *
     *    /___/    \ \ \ \ \______/ / / / /    \___\
                    \ \ \ \        / / / /
 *      /\    __     \ \ \ \______/ / / /     __    /\
       /  \   \_\    /  \ \        / /  \    /_/   /  \     *
      /   /         /   /  \      /  \   \         \   \
     _\  /         /   /    \    /    \   \         \  /_
    /  \ \--------/   /      \__/      \   \--------/ /  \
   /    \------------/       /  \       \------------/    \
  < ------------------------<    >------------------------ >
   \    /------------\       \__/       /------------\    /
    \__/ /--------\ * \      /  \      /   /--------\ \__/
      /  \         \   \    /    \    /   /         /  \
      \   \    __   \   \  /      \  /   /   __    /   /
       \  /   /_/    \  / / ______ \ \  /    \_\   \  /
 *      \/           / / / /      \ \ \ \           \/     *
           ___      / / / / ______ \ \ \ \       ___
          \   \    / / / / /      \ \ \ \ \    /   /
           \   \__/ / / / /        \ \ \ \ \__/   /  *
            \_ ____/ / / /    /\    \ \ \ \____ _/
              /     / / /     \/     \ \ \     \
              \    /  \ \___      ___/ /  \    /
               \  /    \    \    /    / *  \  /        *
    *           \______/\____\  /____/\______/
           *                              *"
let fire_ball = "

    (  .      )
           )           (              )
                 .  '   .   '  .  '  .
        (    , )       (.   )  (   ',    )
         .' ) ( . )    ,  ( ,     )   ( .
      ). , ( .   (  ) ( , ')  .' (  ,    )
     (_,) . ), ) _) _,')  (, ) '. )  ,. (' )
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 `                 __ _          
                / _(_)         
                | |_ _ _ __ ___ 
                |  _| | '__/ _ \
                | | | | | |  __/
                |_| |_|_|  \___|"
let plasma_ball = "
                               ________________
                          ____/ (  (    )   )  \___
                         /( (  (  )   _    ))  )   )\
                       ((     (   )(    )  )   (   )  )
                     ((/  ( _(   )   (   _) ) (  () )  )
                    ( (  ( (_)   ((    (   )  .((_ ) .  )_
                   ( (  )    (      (  )    )   ) . ) (   )
                  (  (   (  (   ) (  _  ( _) ).  ) . ) ) ( )
                  ( (  (   ) (  )   (  ))     ) _)(   )  )  )
                 ( (  ( \ ) (    (_  ( ) ( )  )   ) )  )) ( )
                  (  (   (  (   (_ ( ) ( _    )  ) (  )  )   )
                 ( (  ( (  (  )     (_  )  ) )  _)   ) _( ( )
                  ((  (   )(    (     _    )   _) _(_ (  (_ )
                   (_((__(_(__(( ( ( |  ) ) ) )_))__))_)___)
                   ((__)        \\||lll|l||///          \_))
                            (   /(/ (  )  ) )\   )
                          (    ( ( ( | | ) ) )\   )
                           (   /(| / ( )) ) ) )) )
                         (     ( ((((_(|)_)))))     )
                          (      ||\(|(|)|/||     )
                        (        |(||(||)||||        )
                          (     //|/l|||)|\\ \     )
                        (/ / //  /|//||||\\  \ \  \ _)
"

let dagger = Weapon.make_weapon "Dagger" 50 in 
let sword = Weapon.make_weapon "Sword" 100 in
let gun = Weapon.make_weapon "Gun" 150 in  
let fireball = Weapon.make_weapon "Fireball" 200 in 
let iceball = Weapon.make_weapon "IceBall" 250 in 
let plasmaball = Weapon.make_weapon "Plasmaball" 300 in 
let arr = [(dagger, 11);(sword, 10); (gun, 7); (fireball, 5); 
           (iceball, 3); (plasmaball, 1)]


let get_dice_design (weapon :  Weapon.t) : unit = 
  match weapon with
  | Weapon.sword -> ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.on_green] sword
  | 1 -> ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.on_green] gun
  | 2 -> ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.on_green] board_2
  | 3 -> ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.on_green] board_3
  | 4 -> ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.on_green] board_4
  | 5 -> ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.on_green] board_5
  | 6 -> ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.on_green] board_6
  | _ -> failwith ""  

