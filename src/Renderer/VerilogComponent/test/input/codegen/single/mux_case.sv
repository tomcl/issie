//-----------------------------------------------------
// Design Name : mux_using_case
// File Name   : mux_using_case.v
// Function    : 2:1 Mux using Case
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
module  mux_using_case(
 din_0      , // Mux first input
 din_1      , // Mux Second input
 sel           , // Select input
 mux_out   // Mux output
 );
 //-----------Input Ports---------------
 input bit din_0, din_1, sel ;
 //-----------Output Ports---------------
 output bit mux_out;
 
 //-------------Code Starts Here---------
 always_comb
  begin
    case(sel ) 
        1'b0 : mux_out = din_0;
        1'b1 : mux_out = din_1;
    endcase 
  end
  
endmodule //End Of Module mux
