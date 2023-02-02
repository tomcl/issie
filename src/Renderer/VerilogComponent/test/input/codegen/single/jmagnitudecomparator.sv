// This takes two 4 bit numbers and compares them
// The outputs are single bit
//    AEQB i.e. A equals B
//    AGTB i.e. A greater than B
//    ALTB i.e. A less than B
//
//
module jmagnitudeComparator(AEQB, AGTB, ALTB, A, B);
  output bit AEQB, AGTB, ALTB;
  input bit [3:0] A, B;

  always_comb
  begin
    if( A == B )
      begin
        AEQB = 1'b1;
        AGTB = 1'b0;
        ALTB = 1'b0;
      end
    else if ( A > B )
      begin
        AEQB = 1'b0;
        AGTB = 1'b1;
        ALTB = 1'b0;
      end
    else
      begin
        AEQB = 1'b0;
        AGTB = 1'b0;
        ALTB = 1'b1;
      end
  end
endmodule