module top_module;
  bit [1:0] in1;
  bit [1:0] in1_array [5:0];
  bit [2:0] in2;
  bit [2:0] in2_array [5:0];
  bit [4:0] out;
  bit [4:0] out_array [5:0];
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
    $display("%d]}", out_array[5]);
    $write("]");
    $finish(0);
  end
  initial begin
      in1_array[0] = 2'd3;
      in1_array[1] = 2'd1;
      in1_array[2] = 2'd2;
      in1_array[3] = 2'd1;
      in1_array[4] = 2'd0;
      in1_array[5] = 2'd3;
      in2_array[0] = 3'd0;
      in2_array[1] = 3'd2;
      in2_array[2] = 3'd4;
      in2_array[3] = 3'd7;
      in2_array[4] = 3'd5;
      in2_array[5] = 3'd3;
    for(j_=0; j_<6; j_=j_+1) begin
        in1=in1_array[j_];
        in2=in2_array[j_];
      #0.5;
        out_array[j_]=out;
      @(negedge clk);
end
  end
  always_ff_nonblocking_expression dut (.in1(in1), .in2(in2), .out(out), .clk(clk));
endmodule