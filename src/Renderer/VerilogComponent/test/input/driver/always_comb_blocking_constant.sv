module top_module;
  bit [0:0] in;
  bit [0:0] in_array [5:0];
  bit [3:0] out;
  bit [3:0] out_array [5:0];
  bit [0:0] out2;
  bit [0:0] out2_array [5:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(16) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"out\", \"Values\": [");
    for(i_=0;i_<5; i_=i_+1) begin $write("%d, ", out_array[i_]); end
    $display("%d]}, ", out_array[5]);
    $write("{\"Label\": \"out2\", \"Values\": [");
    for(i_=0;i_<5; i_=i_+1) begin $write("%d, ", out2_array[i_]); end
    $display("%d]}", out2_array[5]);
    $write("]");
    $finish(0);
  end
  initial begin
      in_array[0] = 1'd0;
      in_array[1] = 1'd0;
      in_array[2] = 1'd0;
      in_array[3] = 1'd1;
      in_array[4] = 1'd1;
      in_array[5] = 1'd1;
    for(j_=0; j_<6; j_=j_+1) begin
        in=in_array[j_];
      #0.5;
        out_array[j_]=out;
        out2_array[j_]=out2;
      @(negedge clk);
end
  end
  always_comb_blocking_constant dut (.in(in), .out(out), .out2(out2));
endmodule