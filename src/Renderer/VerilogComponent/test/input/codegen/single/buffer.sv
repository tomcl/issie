module wires(
  // Write your IO Port Declarations here
  input bit [2:0] in,
  output bit [2:0] out
); 
  // Write your Assignments here  
  always_comb begin  
    out=in;
  end
 
endmodule