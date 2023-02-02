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
begin
  binary_out = 4'd0;
  if (enable) begin
    case (encoder_in) 
      16'h0002 : binary_out = 4'd1; 
      16'h0004 : binary_out = 4'd2; 
      16'h0008 : binary_out = 4'd3; 
      16'h0010 : binary_out = 4'd4;
      16'h0020 : binary_out = 4'd5; 
      16'h0040 : binary_out = 4'd6; 
      16'h0080 : binary_out = 4'd7; 
      16'h0100 : binary_out = 4'd8;
      16'h0200 : binary_out = 4'd9;
      16'h0400 : binary_out = 4'd10; 
      16'h0800 : binary_out = 4'd11; 
      16'h1000 : binary_out = 4'd12; 
      16'h2000 : binary_out = 4'd13; 
      16'h4000 : binary_out = 4'd14; 
      16'h8000 : binary_out = 4'd15; 
   endcase
  end
end


endmodule