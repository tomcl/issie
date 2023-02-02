//-----------------------------------------------------
// Design Name : up_counter_load
// File Name   : up_counter_load.v
// Function    : Up counter with load
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
module up_counter_load    (
out      ,  // Output of the counter
data     ,  // Parallel load for the counter
load     ,  // Parallel load enable
enable   ,  // Enable counting
clk      ,  // clock input
reset       // reset input
);
//----------Output Ports--------------
output bit [7:0] out;
//------------Input Ports-------------- 
input bit [7:0] data;
input bit load, enable, clk, reset;
//------------Internal Variables--------
//-------------Code Starts Here-------
always_ff @(posedge clk)
if (reset) begin
  out <= 8'b0 ;
end else if (load) begin
  out <= data;
end else if (enable) begin
  out <= out + 8'b1;
end
    
endmodule  