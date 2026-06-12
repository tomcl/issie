module top_module;
  bit [0:0] addr;
  bit [0:0] addr_array [5:0];
  bit [2:0] output1;
  bit [2:0] output1_array [5:0];
  bit [2:0] output2;
  bit [2:0] output2_array [5:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(16) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"output1\", \"Values\": [");
    for(i_=0;i_<5; i_=i_+1) begin $write("%d, ", output1_array[i_]); end
    $display("%d]}, ", output1_array[5]);
    $write("{\"Label\": \"output2\", \"Values\": [");
    for(i_=0;i_<5; i_=i_+1) begin $write("%d, ", output2_array[i_]); end
    $display("%d]}", output2_array[5]);
    $write("]");
    $finish(0);
  end
  initial begin
      addr_array[0] = 1'd0;
      addr_array[1] = 1'd0;
      addr_array[2] = 1'd1;
      addr_array[3] = 1'd1;
      addr_array[4] = 1'd0;
      addr_array[5] = 1'd1;
    for(j_=0; j_<6; j_=j_+1) begin
        addr=addr_array[j_];
      #0.5;
        output1_array[j_]=output1;
        output2_array[j_]=output2;
      @(negedge clk);
end
  end
  array_test dut (.addr(addr), .output1(output1), .output2(output2));
endmodule