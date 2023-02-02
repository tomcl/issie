//source: https://github.com/snbk001/Verilog-Design-Examples/blob/main/4%20bit%20counter/counter4bit.v
module counter4bit(
                   reset,
                   load,
                   enable,
                   clk,
                   count_out,
                   count
                   );

input bit reset, load, enable, clk;
output bit [3:0]count_out;
output bit [3:0]count;
bit [3:0] S0 = 4'd8;
bit [3:0] S1 = 4'd7; 
bit [3:0] S2 = 4'd11;
bit [3:0] S3 = 4'd4;
bit [3:0] S4 = 4'd9; 
bit [3:0] S5 = 4'd2;
bit [3:0] S6 = 4'd5;
bit [3:0] S7 = 4'd12;
bit [3:0] S8 = 4'd6;
bit [3:0] S9 = 4'd3;
bit [3:0] S10 = 4'd15;
bit [3:0] S11 = 4'd1;
bit [3:0] S12= 4'd14;
bit [3:0] S13 = 4'd13;

always_ff @ (posedge clk)
   begin
      if (reset)
         count_out <= S0;
      else
         count_out <= count;
   end

always_ff @ (posedge clk)
   begin
      if (load && enable)
         begin
            case(count_out)
               4'd8       :  count <= S1 ;
               4'd7       :  count <= S2 ;
               4'd11       :  count <= S3 ;
               4'd4       :  count <= S4 ;
               4'd9       :  count <= S5 ;
               4'd2       :  count <= S6 ;
               4'd5       :  count <= S7 ;
               4'd12       :  count <= S8 ;
               4'd6       :  count <= S9 ;
               4'd3       :  count <= S10;
               4'd15      :  count <= S11;
               4'd1      :  count <= S12;
               4'd14      :  count <= S13;
               4'd13      :  count <= S0 ;
               default  :  count <= S0 ; 
            endcase
         end
      else if (load == 1'd0)
         count <= S0;
      else if (enable == 1'd0)
         count <= count;
   end            

endmodule