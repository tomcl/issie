//-----------------------------------------------------
// Design Name : tff_sync_reset
// File Name   : tff_sync_reset.v
// Function    : T flip-flop sync reset
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
module tff_sync_reset (
data  , // Data Input
clk   , // Clock Input
reset , // Reset input
q       // Q output
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
end else if (data) begin
  q <= ~q;
end

endmodule //End Of Module tff_async_reset
