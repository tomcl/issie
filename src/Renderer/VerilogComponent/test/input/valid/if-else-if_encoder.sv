//-----------------------------------------------------
// Design Name : encoder_using_if
// File Name   : encoder_using_if.v
// Function    : Encoder using If
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
module encoder_using_if(
binary_out , //  4 bit binary output
encoder_in , //  16-bit input
enable       //  Enable for the encoder
); 
//-----------Output Ports---------------
output bit [3:0] binary_out  ;
//-----------Input Ports---------------
input  bit enable ; 
input bit [15:0] encoder_in ; 
//------------Internal Variables--------
//reg [3:0] binary_out ;  
//-------------Code Start-----------------
always_comb
 begin 
   binary_out = 4'd0; 
   if (enable) begin
     if (encoder_in == 16'h0002) begin
      binary_out = 4'd1;
     end  if (encoder_in == 16'h0004) begin 
      binary_out = 4'd2; 
     end  if (encoder_in == 16'h0008) begin 
      binary_out = 4'd3; 
     end  if (encoder_in == 16'h0010) begin 
      binary_out = 4'd4; 
     end  if (encoder_in == 16'h0020) begin 
      binary_out = 4'd5; 
     end  if (encoder_in == 16'h0040) begin 
      binary_out = 4'd6; 
     end  if (encoder_in == 16'h0080) begin 
      binary_out = 4'd7; 
     end  if (encoder_in == 16'h0100) begin 
      binary_out = 4'd8; 
     end  if (encoder_in == 16'h0200) begin 
      binary_out = 4'd9; 
     end if (encoder_in == 16'h0400) begin 
      binary_out = 4'd10; 
     end  if (encoder_in == 16'h0800) begin 
      binary_out = 4'd11; 
     end  if (encoder_in == 16'h1000) begin
      binary_out = 4'd12; 
     end  if (encoder_in == 16'h2000) begin 
      binary_out = 4'd13;
     end  if (encoder_in == 16'h4000) begin 
      binary_out = 4'd14; 
     end if (encoder_in == 16'h8000) begin 
      binary_out = 4'd15; 
     end
  end
end
      
endmodule