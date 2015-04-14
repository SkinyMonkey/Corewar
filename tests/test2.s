.name    "Octobre Rouge V4.2"
.comment  "And the winner is ..."

    st  r1,19
    ld  %0,r15
    fork  %:xxx
    zjmp  %:torp
xxx:    live  %42
    fork  %:torp
    zjmp  %:xxx
  
zork:    live  %42
    fork  %:zork2
    st  r1,13
    ld  %0,r15    
zork_:    live  %42
    zjmp  %-5
    
zork2:    live  %42
    fork  %:zork3
    st  r1,13
    ld  %0,r15
zork2_:    live  %42
    zjmp  %-5

zork3:    live  %42
    fork  %:zork4
    st  r1,13
    ld  %0,r15
