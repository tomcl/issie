//remove Not ones
module jbasicgates(yOR,yAND,a,b);
  output bit yOR,yAND;
  input bit a,b;
  
  assign yOR = a | b;
  assign yAND = a & b;
  // assign yXOR = a ^ b;
  // assign yNOR = ~(a | b);
  // assign yNAND = ~(a & b);
  // assign yXNOR = ~(a ^ b);
  
endmodule