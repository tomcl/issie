module top_module;
  bit [0:0] rst;
  bit [0:0] rst_array [9:0];
  bit [1:0] sr;
  bit [1:0] sr_array [9:0];
  bit [0:0] q;
  bit [0:0] q_array [9:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(24) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"q\", \"Values\": [");
    for(i_=0;i_<9; i_=i_+1) begin $write("%d, ", q_array[i_]); end
    $display("%d]}", q_array[9]);
    $write("]");
    $finish(0);
  end
  initial begin
      rst_array[0] = 1'd0;
      rst_array[1] = 1'd0;
      rst_array[2] = 1'd0;
      rst_array[3] = 1'd0;
      rst_array[4] = 1'd1;
      rst_array[5] = 1'd0;
      rst_array[6] = 1'd0;
      rst_array[7] = 1'd0;
      rst_array[8] = 1'd1;
      rst_array[9] = 1'd1;
      sr_array[0] = 2'd0;
      sr_array[1] = 2'd1;
      sr_array[2] = 2'd2;
      sr_array[3] = 2'd0;
      sr_array[4] = 2'd3;
      sr_array[5] = 2'd3;
      sr_array[6] = 2'd1;
      sr_array[7] = 2'd2;
      sr_array[8] = 2'd3;
      sr_array[9] = 2'd0;
    for(j_=0; j_<10; j_=j_+1) begin
        rst=rst_array[j_];
        sr=sr_array[j_];
      #0.5;
        q_array[j_]=q;
      @(negedge clk);
end
  end
  jsrflipflop dut (.rst(rst), .sr(sr), .q(q), .clk(clk));
endmodule