// This is a 4 bit serial adder, output will be ready after 4 clock cycles
// Assume a,b will change with the clock
// Assume carryin will NOT change with the clock i.e. it will remain constant over the 4 clock cycles
// The outputs y & carryout will keep changing at posedge to indicate the current state of calculation
module jserialaddlab(a, b , clk, reset, sum, carry, cout, count, so);
  input bit clk, reset, a, b;
  output bit sum, carry;
  bit cin;
  output bit [3:0] so;
  output bit [2:0] count;
  output bit cout;
  bit cut;
  
  assign sum = a^b^cin;
  assign carry = (count == 3'd4) ? 1'b0 : cut;
  assign cut = (a&b) | (a&cin) | (b&cin);
  
  always_ff@(posedge clk)
  begin
    if(reset)
      cin <= 1'b0;
    else
      cin <= carry;
  end

  always_ff@(posedge clk)
  begin
    if(reset)
      cout <= 1'b0;
    else
      cout <= cut;
  end
  
  always_ff@(posedge clk)
  begin
    if(reset)
      so <= 1'b0;
    else
      so <= {sum, so[3:1]};
  end
  
  always_ff@(posedge clk)
  begin
    if(reset)
      count <= 1'b0;
    else
      count <= (count ==3'd4) ? 3'd1 : count+3'd1;
  end
  
endmodule
