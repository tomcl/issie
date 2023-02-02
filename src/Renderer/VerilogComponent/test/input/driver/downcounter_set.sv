module top_module;
  bit [0:0] s;
  bit [0:0] s_array [22:0];
  bit [3:0] q;
  bit [3:0] q_array [22:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(50) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"q\", \"Values\": [");
    for(i_=0;i_<22; i_=i_+1) begin $write("%d, ", q_array[i_]); end
    $display("%d]}", q_array[22]);
    $write("]");
    $finish(0);
  end
  initial begin
      s_array[0] = 1'd0;
      s_array[1] = 1'd0;
      s_array[2] = 1'd0;
      s_array[3] = 1'd1;
      s_array[4] = 1'd0;
      s_array[5] = 1'd0;
      s_array[6] = 1'd0;
      s_array[7] = 1'd0;
      s_array[8] = 1'd0;
      s_array[9] = 1'd0;
      s_array[10] = 1'd0;
      s_array[11] = 1'd0;
      s_array[12] = 1'd0;
      s_array[13] = 1'd0;
      s_array[14] = 1'd0;
      s_array[15] = 1'd0;
      s_array[16] = 1'd0;
      s_array[17] = 1'd0;
      s_array[18] = 1'd1;
      s_array[19] = 1'd1;
      s_array[20] = 1'd1;
      s_array[21] = 1'd0;
      s_array[22] = 1'd0;
    for(j_=0; j_<23; j_=j_+1) begin
        s=s_array[j_];
      #0.5;
        q_array[j_]=q;
      @(negedge clk);
end
  end
  counter dut (.s(s), .q(q), .clk(clk));
endmodule