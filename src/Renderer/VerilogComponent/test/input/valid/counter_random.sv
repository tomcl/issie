//-----------------------------------------------------
// Design Name : lfsr
// File Name   : lfsr.v
// Function    : Linear feedback shift register
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
module lfsr    (
out             ,  // Output of the counter
enable          ,  // Enable  for counter
clk             ,  // clock input
reset              // reset input
);

//----------Output Ports--------------
output bit [7:0] out;
//------------Input Ports--------------
input bit enable, clk, reset;
//------------Internal Variables--------
bit        linear_feedback;

//-------------Code Starts Here-------
//assign linear_feedback = !(out[7] ^ out[3]);
assign linear_feedback = ~(out[7] ^ out[3]);

always_ff @(posedge clk)
if (reset) begin // active high reset
  out <= 8'b0 ;
end else if (enable) begin
  out <= {out[6],out[5],
          out[4],out[3],
          out[2],out[1],
          out[0], linear_feedback};
end 

endmodule // End Of Module counter
