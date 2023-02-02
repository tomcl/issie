//-----------------------------------------------------
// Design Name : up_counter
// File Name   : up_counter.v
// Function    : Up counter
// Coder      : Deepak
//-----------------------------------------------------
module up_counter    (
out     ,  // Output of the counter
enable  ,  // enable for counter
clk     ,  // clock Input
reset      // reset Input
);
//----------Output Ports--------------
    output bit [7:0] out;
//------------Input Ports--------------
     input bit enable, clk, reset;
//------------Internal Variables--------
    bit [7:0] out;
//-------------Code Starts Here-------
always_ff @(posedge clk)
if (reset) begin
  out <= 8'b0 ;
end else if (enable) begin
  out <= out + 8'b1;
end


endmodule 
