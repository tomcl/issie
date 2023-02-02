module top_module;
  bit [0:0] reset;
  bit [0:0] reset_array [26:0];
  bit [3:0] count;
  bit [3:0] count_array [26:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(58) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"count\", \"Values\": [");
    for(i_=0;i_<26; i_=i_+1) begin $write("%d, ", count_array[i_]); end
    $display("%d]}", count_array[26]);
    $write("]");
    $finish(0);
  end
  initial begin
      reset_array[0] = 1'd0;
      reset_array[1] = 1'd0;
      reset_array[2] = 1'd0;
      reset_array[3] = 1'd0;
      reset_array[4] = 1'd0;
      reset_array[5] = 1'd0;
      reset_array[6] = 1'd0;
      reset_array[7] = 1'd0;
      reset_array[8] = 1'd1;
      reset_array[9] = 1'd0;
      reset_array[10] = 1'd0;
      reset_array[11] = 1'd0;
      reset_array[12] = 1'd0;
      reset_array[13] = 1'd0;
      reset_array[14] = 1'd0;
      reset_array[15] = 1'd0;
      reset_array[16] = 1'd0;
      reset_array[17] = 1'd0;
      reset_array[18] = 1'd0;
      reset_array[19] = 1'd0;
      reset_array[20] = 1'd0;
      reset_array[21] = 1'd0;
      reset_array[22] = 1'd0;
      reset_array[23] = 1'd0;
      reset_array[24] = 1'd0;
      reset_array[25] = 1'd0;
      reset_array[26] = 1'd0;
    for(j_=0; j_<27; j_=j_+1) begin
        reset=reset_array[j_];
      #0.5;
        count_array[j_]=count;
      @(negedge clk);
end
  end
  synchronouscounter dut (.reset(reset), .count(count), .clk(clk));
endmodule