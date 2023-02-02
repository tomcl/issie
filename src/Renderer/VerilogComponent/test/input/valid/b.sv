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
//    binary_out = 4'd0; 
//    if (enable) begin
//      if (encoder_in == 16'h0002)
       binary_out = 4'd1;
//   end
  end
      
endmodule