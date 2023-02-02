//-----------------------------------------------------
// Design Name : gray_counter
// File Name   : gray_counter.v
// Function    : 8 bit gray counterS
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
module gray_counter (
  out    , // counter out
  enable , // enable for counter
  clk    , // clock
  rst      // active hight reset
  );
  
  //------------Input Ports--------------
  input bit clk, rst, enable; 
  //----------Output Ports----------------
  output bit [ 7:0] out;
  //------------Internal Variables--------
  // wire [7:0] out; before it was assign wire ...
  bit [7:0] count;
  //-------------Code Starts Here---------
  always_ff @ (posedge clk) 
  if (rst) 
    count <= 8'b0; 
  else if (enable) 
    count <= count + 8'b1; 
    
  assign out = { count[7], (count[7] ^ count[6]),(count[6] ^ 
               count[5]),(count[5] ^ count[4]), (count[4] ^ 
               count[3]),(count[3] ^ count[2]), (count[2] ^ 
               count[1]),(count[1] ^ count[0]) };
    
endmodule 
