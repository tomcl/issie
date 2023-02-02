module top_module;
  bit [0:0] d;
  bit [0:0] d_array [13:0];
  bit [0:0] q;
  bit [0:0] q_array [13:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(32) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"q\", \"Values\": [");
    for(i_=0;i_<13; i_=i_+1) begin $write("%d, ", q_array[i_]); end
    $display("%d]}", q_array[13]);
    $write("]");
    $finish(0);
  end
  initial begin
      d_array[0] = 1'd0;
      d_array[1] = 1'd0;
      d_array[2] = 1'd1;
      d_array[3] = 1'd0;
      d_array[4] = 1'd1;
      d_array[5] = 1'd1;
      d_array[6] = 1'd1;
      d_array[7] = 1'd1;
      d_array[8] = 1'd0;
      d_array[9] = 1'd1;
      d_array[10] = 1'd0;
      d_array[11] = 1'd1;
      d_array[12] = 1'd0;
      d_array[13] = 1'd0;
    for(j_=0; j_<14; j_=j_+1) begin
        d=d_array[j_];
      #0.5;
        q_array[j_]=q;
      @(negedge clk);
end
  end
  flop dut (.d(d), .q(q), .clk(clk));
endmodule