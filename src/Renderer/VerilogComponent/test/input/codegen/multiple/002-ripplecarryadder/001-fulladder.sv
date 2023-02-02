module jfulladder(y,carryout,a,b,carryin);
  output bit y,carryout;
  input bit a,b,carryin;
  
  assign y = a ^ b ^ carryin;
  assign carryout = ( a & b ) | ( a & carryin ) | ( b & carryin );
  
endmodule