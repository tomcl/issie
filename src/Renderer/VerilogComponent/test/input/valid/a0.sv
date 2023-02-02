//-----------------------------------------------------
// Design Name : encoder_using_case
// File Name   : encoder_using_case.v
// Function    : Encoder using Case
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
module encoder_using_case(
binary_out , //  4 bit binary Output
encoder_in , //  16-bit Input
enable       //  Enable for the encoder
);
output bit [3:0] binary_out  ;
input  bit enable ; 
input bit [15:0] encoder_in ; 
     
//reg [3:0] binary_out ;
      
 always_comb
// begin
//   binary_out = 4'd0;
//   if (enable) begin
     case (encoder_in) 
       16'h0002 : binary_out = 4'd1; 
     endcase
//   end
// end

endmodule