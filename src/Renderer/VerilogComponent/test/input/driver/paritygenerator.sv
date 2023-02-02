module top_module;
  bit [3:0] DIN;
  bit [3:0] DIN_array [9:0];
  bit [4:0] DOUT;
  bit [4:0] DOUT_array [9:0];
  bit [0:0] parity;
  bit [0:0] parity_array [9:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(24) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"DOUT\", \"Values\": [");
    for(i_=0;i_<9; i_=i_+1) begin $write("%d, ", DOUT_array[i_]); end
    $display("%d]}, ", DOUT_array[9]);
    $write("{\"Label\": \"parity\", \"Values\": [");
    for(i_=0;i_<9; i_=i_+1) begin $write("%d, ", parity_array[i_]); end
    $display("%d]}", parity_array[9]);
    $write("]");
    $finish(0);
  end
  initial begin
      DIN_array[0] = 4'd7;
      DIN_array[1] = 4'd15;
      DIN_array[2] = 4'd3;
      DIN_array[3] = 4'd1;
      DIN_array[4] = 4'd0;
      DIN_array[5] = 4'd5;
      DIN_array[6] = 4'd2;
      DIN_array[7] = 4'd8;
      DIN_array[8] = 4'd10;
      DIN_array[9] = 4'd13;
    for(j_=0; j_<10; j_=j_+1) begin
        DIN=DIN_array[j_];
      #0.5;
        DOUT_array[j_]=DOUT;
        parity_array[j_]=parity;
      @(negedge clk);
end
  end
  jparityGenerator dut (.DIN(DIN), .DOUT(DOUT), .parity(parity));
endmodule