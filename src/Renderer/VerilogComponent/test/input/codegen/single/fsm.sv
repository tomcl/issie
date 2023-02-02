// Identifies the sequence 11101
// includes a overlap bit sequence as well
module jfsmMealyWithOverlap(dataout, clk, reset, datain);
  output bit dataout;
  input bit clk, reset, datain;
  
  bit [2:0] cs, ns;
  
  bit [2:0] a = 3'b000;
  bit [2:0] b = 3'b001;
  bit [2:0] c = 3'b010;
  bit [2:0] d = 3'b011;
  bit [2:0] e = 3'b100;
  bit [2:0] f = 3'b101;
  
  always_ff @(posedge clk)
  begin
    if(reset)
      cs <= a;
    else
      cs <= ns;
  end

  always_comb
  begin
    case(cs)
    3'b000:
      begin
        if(datain)
          ns = b;
        else
          ns = a;
      end
    3'b001:
      begin
        if(datain)
          ns = c;
        else
          ns = b;
      end
    3'b010:
      begin
        if(datain)
          ns = d;
        else
          ns = a;
      end
    3'b011:
      begin
        if(datain)
          ns = d;
        else
          ns = e;
      end
    3'b101:
      begin
        if(datain)
          ns = b; // This has to be ns <= a; if we have to consider with overlap
        else
          ns = a;
      end
    default: ns=a;
    endcase      
  end
  
  // This will assign the correct status to the dataout bit
  always_comb
  begin
    if ( cs == e && datain == 1'b1 )
      dataout = 1'b1;
    else
      dataout = 1'b0;
  end    
endmodule