module top_module;
  bit [0:0] reset;
  bit [0:0] reset_array [22:0];
  bit [2:0] OUTPUT;
  bit [2:0] OUTPUT_array [22:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(50) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"OUTPUT\", \"Values\": [");
    for(i_=0;i_<22; i_=i_+1) begin $write("%d, ", OUTPUT_array[i_]); end
    $display("%d]}", OUTPUT_array[22]);
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
      reset_array[8] = 1'd0;
      reset_array[9] = 1'd1;
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
    for(j_=0; j_<23; j_=j_+1) begin
        reset=reset_array[j_];
      #0.5;
        OUTPUT_array[j_]=OUTPUT;
      @(negedge clk);
end
  end
  jarbitraryCounter dut (.reset(reset), .OUTPUT(OUTPUT), .clk(clk));
endmodule