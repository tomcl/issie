module top_module;
  bit [0:0] in;
  bit [0:0] in_array [5:0];
  bit [1:0] out;
  bit [1:0] out_array [5:0];
  bit [0:0] dummy;
  bit [0:0] dummy_array [5:0];
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
    $write("{\"Label\": \"dummy\", \"Values\": [");
    for(i_=0;i_<5; i_=i_+1) begin $write("%d, ", dummy_array[i_]); end
    $display("%d]}", dummy_array[5]);
    $write("]");
    $finish(0);
  end
  initial begin
      in_array[0] = 1'd0;
      in_array[1] = 1'd0;
      in_array[2] = 1'd0;
      in_array[3] = 1'd0;
      in_array[4] = 1'd0;
      in_array[5] = 1'd0;
    for(j_=0; j_<6; j_=j_+1) begin
        in=in_array[j_];
      #0.5;
        out_array[j_]=out;
        dummy_array[j_]=dummy;
      @(negedge clk);
end
  end
  if_comb dut (.in(in), .out(out), .dummy(dummy));
endmodule