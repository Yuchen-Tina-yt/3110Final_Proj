open ANSITerminal

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
       \\***/
        \\*/
         V"

let sword = "
                      d@b
H@b                 ,H@@______________________________________________________
H@@EEEEEEEEEEEEEEEEEEEH@@                                                    /
H@@EEEEEEEEEEEEEEEEEEEH@@                                                   /
H@@EEEEEEEEEEEEEEEEEEEH@@___________________________________________________
H@P                  `H@@
                      T@
"
let gun = "
 +--^----------,--------,-----,--------^-,
 | |||||||||   `--------'     |          O
 `+---------------------------^----------|
   `\\_,---------,---------,--------------'
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
                /      \\/    /  \\    \\/      \\   *
     *         /  \\    / ___/    \\___ \    /  \\
              /    \\  / /            \\ \\  /    \\           *
             _\\____ \\ \\ \\     /\\     / / / ____/_
            /   __ \\ \\ \\ \\    \\/*   / / / / __   \\
           /   /  \\ \\ \\ \\ \\        / / / / /  \\   \\      *
     *    /___/    \\ \\ \\ \\ \\______/ / / / /    \\___\\
                    \\ \\ \\ \\        / / / /
 *      /\\    __     \\ \\ \\ \\______/ / / /     __    /\\
       /  \\   \\_\\    /  \\ \\        / /  \\    /_/   /  \\     *
      /   /         /   /  \\      /  \\   \\         \\   \\
     _\\  /         /   /    \\    /    \\   \         \\  /_
    /  \\ \\--------/   /      \\__/      \\   \\--------/ /  \\
   /    \\------------/       /  \\       \\------------/    \\
  < ------------------------<    >------------------------ >
   \\    /------------\\       \\__/       /------------\\    /
    \\__/ /--------\\ * \\      /  \\      /   /--------\\ \\__/
      /  \\         \\   \\    /    \\    /   /         /  \\
      \\   \\    __   \\   \\  /      \\  /   /   __    /   /
       \\  /   /_/    \\  / / ______ \\ \\  /    \\_\\   \\  /
 *      \\/           / / / /      \\ \\ \\ \\           \\/     *
           ___      / / / / ______ \\ \\ \\ \\       ___
          \\   \\    / / / / /      \\ \\ \\ \\ \\    /   /
           \\   \\__/ / / / /        \\ \\ \\ \\ \\__/   /  *
            \\_ ____/ / / /    /\\    \\ \\ \\ \\____ _/
              /     / / /     \\/     \\ \\ \\     \\
              \\    /  \\ \\___      ___/ /  \\    /
               \\  /    \\    \\    /    / *  \\  /        *
    *           \\______/\\____\\  /____/\\______/
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
                |_| |_|_|  \\___|"

let plasma_ball = "
                               ________________
                          ____/ (  (    )   )  \\___
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
                   ((__)        \\||lll|l||///          \\_))
                            (   /(/ (  )  ) )\\   )
                          (    ( ( ( | | ) ) )\\   )
                           (   /(| / ( )) ) ) )) )
                         (     ( ((((_(|)_)))))     )
                          (      ||\\(|(|)|/||     )
                        (        |(||(||)||||        )
                          (     //|/l|||)|\\ \\     )
                        (/ / //  /|//||||\\  \\ \\  \\ _)
"

let bow = "

          4$$-.                       
           4   '.                                        
           4    ^.                                       
           4     $                                       
           4     'b                                      
           4      'b.                                    
           4        $                                    
           4        $r                                   
           4        $F                                   
-$b========4========$b====*P=-                           
           4       *$$F                                  
           4        $$'                                 
           4       .$F                                   
           4       dP                                    
           4      F                                      
           4     @                                       
           4    .                                        
           J.                                            
          '$$
          "

let axe = "
                                           _.gd8888888bp._
                                        .g88888888888888888p.
                                      .d8888P''       ''Y8888b.
                                      'Y8P'               'Y8P'
                                         `.               ,'
                                           \     .-.     /
                                            \   (___)   /
 .------------------._______________________:__________j
/                   |                      |           |`-.,_
\###################|######################|###########|,-'`
 `------------------'                       :    ___   l
                                            /   (   )   \
                                   fsc     /     `-'     \
                                         ,'               `.
                                      .d8b.               .d8b.
                                      'Y8888p..       ,.d8888P'
                                        'Y88888888888888888P'
                                           ''YY8888888PP''
                                           "

let bomb = "
                 .               
                 .               
                 .       :       
                 :      .        
        :..   :  : :  .          
           ..  ; :: .            
              ... .. :..         
             ::: :...            
         ::.:.:...;; .....       
      :..     .;.. :;     ..     
            . :. .  ;.           
             .: ;;: ;.           
            :; .BRRRV;           
               YB BMMMBR         
              ;BVIMMMMMt         
        .=YRBBBMMMMMMMB          
      =RMMMMMMMMMMMMMM;          
    ;BMMR=VMMMMMMMMMMMV.         
   tMMR::VMMMMMMMMMMMMMB:        
  tMMt ;BMMMMMMMMMMMMMMMB.       
 ;MMY ;MMMMMMMMMMMMMMMMMMV       
 XMB .BMMMMMMMMMMMMMMMMMMM:      
 BMI +MMMMMMMMMMMMMMMMMMMMi      
.MM= XMMMMMMMMMMMMMMMMMMMMY      
 BMt YMMMMMMMMMMMMMMMMMMMMi      
 VMB +MMMMMMMMMMMMMMMMMMMM:      
 ;MM+ BMMMMMMMMMMMMMMMMMMR       
  tMBVBMMMMMMMMMMMMMMMMMB.       
   tMMMMMMMMMMMMMMMMMMMB:        
    ;BMMMMMMMMMMMMMMMMY          
      +BMMMMMMMMMMMBY:           
        :+YRBBBRVt;
        "

let saw = "
      ______________________________ ______________________
    ,/                              |                      \
  ,/                                |  __              __   }
 /_.............................____| (  `--.______.--'  )  /
                                     \ `-._          _.-'  /
                                      `-.  `--.__.--'   .-'
                                         `-._       _.-'
                                             `-----'
                                             "



let get_weapon_design (weapon : Weapon.t) =
  let a = Weapon.get_weapon_type weapon in  
  match a with
  | "Dagger" -> ANSITerminal.print_string [ANSITerminal.red] dagger
  | "Sword" -> ANSITerminal.print_string [ANSITerminal.blue] sword
  | "Gun" -> ANSITerminal.print_string [ANSITerminal.cyan] gun
  | "Iceball" -> ANSITerminal.print_string [ANSITerminal.green] ice_ball
  | "Fireball" -> ANSITerminal.print_string [ANSITerminal.magenta] fire_ball
  | "Plasmaball" -> ANSITerminal.print_string [ANSITerminal.yellow] plasma_ball
  | "Bow" -> ANSITerminal.print_string [ANSITerminal.red] bow
  | "Axe" -> ANSITerminal.print_string [ANSITerminal.blue] axe
  | "Bomb" -> ANSITerminal.print_string [ANSITerminal.cyan] bomb
  | "Saw" -> ANSITerminal.print_string [ANSITerminal.green] saw
  | _ -> failwith "Weapon name not found" 