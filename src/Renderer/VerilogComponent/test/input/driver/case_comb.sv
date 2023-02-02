module top_module;
  bit [1:0] in;
  bit [1:0] in_array [3:0];
  bit [1:0] out;
  bit [1:0] out_array [3:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(12) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"out\", \"Values\": [");
    for(i_=0;i_<3; i_=i_+1) begin $write("%d, ", out_array[i_]); end
    $display("%d]}", out_array[3]);
    $write("]");
    $finish(0);
  end
  initial begin
      in_array[0] = 2'd1;
      in_array[1] = 2'd2;
      in_array[2] = 2'd3;
      in_array[3] = 2'd0;
    for(j_=0; j_<4; j_=j_+1) begin
        in=in_array[j_];
      #0.5;
        out_array[j_]=out;
      @(negedge clk);
end
  end
  case_comb dut (.in(in), .out(out));
endmodule