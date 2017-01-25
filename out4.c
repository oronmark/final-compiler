
    #include <stdio.h>
    #include <stdlib.h>
    
    /* change to 0 for no debug info to be printed: */
    #define DO_SHOW 1
    
    #include "cisc.h"
    
    int main()
    {
      START_MACHINE;
    
      JUMP(CONTINUE);

 	 #define SOB_VOID 1
	 #define SOB_NIL 2
	 #define SOB_FALSE 3
	 #define SOB_TRUE 5
   
    #include "char.lib"
    #include "io.lib"
    #include "math.lib"
    #include "string.lib"
    #include "system.lib"

    
not yet implemented
CMP(R0,IMM(SOB_FALSE));
JUMP_EQ(L_if3_dif_2);
not yet implemented
JUMP_EQ(L_if3_exit_1);
L_if3_dif_2:
not yet implemented
L_if3_exit_1:

not yet implemented
CMP(R0,IMM(SOB_FALSE));
JUMP_EQ(L_if3_dif_4);
not yet implemented
JUMP_EQ(L_if3_exit_3);
L_if3_dif_4:
not yet implemented
L_if3_exit_3:

    STOP_MACHINE;

    return 0;
    }