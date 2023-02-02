//-----------------------------------------------------
// Design Name : mux_using_if
// File Name   : mux_using_if.v
// Function    : 2:1 Mux using If
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
module  mux_using_if(
 din_0      , // Mux first input
 din_1      , // Mux Second input
 sel        , // Select input
 mux_out      // Mux output
 );
 //-----------Input Ports---------------
 input bit din_0, din_1, sel ;
 //-----------Output Ports---------------
 output bit mux_out;
 //------------Internal Variables--------
 //-------------Code Starts Here---------
 always_comb
  begin
    if (sel == 1'b0) begin
        mux_out = din_0;
    end else begin
        mux_out = din_1 ;
    end
  end
 
endmodule //End Of Module mux
