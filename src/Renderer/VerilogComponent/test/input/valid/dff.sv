//-----------------------------------------------------
// Design Name : dff_sync_reset
// File Name   : dff_sync_reset.v
// Function    : D flip-flop sync reset
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
module dff_sync_reset (
data   , // Data Input
clk    , // Clock Input
reset  , // Reset input
q        // Q output
);
//-----------Input Ports---------------
input bit data, clk, reset ; 

//-----------Output Ports---------------
output bit q;

//------------Internal Variables--------

//-------------Code Starts Here---------
always_ff @ ( posedge clk)
if (~reset) begin
  q <= 1'b0;
end  else begin
  q <= data;
end

endmodule //End Of Module dff_sync_reset
