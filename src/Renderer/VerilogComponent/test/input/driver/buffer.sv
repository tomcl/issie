module top_module;
  bit [2:0] in;
  bit [2:0] in_array [9:0];
  bit [2:0] out;
  bit [2:0] out_array [9:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(24) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"out\", \"Values\": [");
    for(i_=0;i_<9; i_=i_+1) begin $write("%d, ", out_array[i_]); end
    $display("%d]}", out_array[9]);
    $write("]");
    $finish(0);
  end
  initial begin
      in_array[0] = 3'd7;
      in_array[1] = 3'd1;
      in_array[2] = 3'd3;
      in_array[3] = 3'd1;
      in_array[4] = 3'd0;
      in_array[5] = 3'd1;
      in_array[6] = 3'd2;
      in_array[7] = 3'd3;
      in_array[8] = 3'd4;
      in_array[9] = 3'd5;
    for(j_=0; j_<10; j_=j_+1) begin
        in=in_array[j_];
      #0.5;
        out_array[j_]=out;
      @(negedge clk);
end
  end
  wires dut (.in(in), .out(out));
endmodule