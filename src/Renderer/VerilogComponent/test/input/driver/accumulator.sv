module top_module;
  bit [0:0] clr;
  bit [0:0] clr_array [17:0];
  bit [3:0] d;
  bit [3:0] d_array [17:0];
  bit [3:0] q;
  bit [3:0] q_array [17:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(40) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"q\", \"Values\": [");
    for(i_=0;i_<17; i_=i_+1) begin $write("%d, ", q_array[i_]); end
    $display("%d]}", q_array[17]);
    $write("]");
    $finish(0);
  end
  initial begin
      clr_array[0] = 1'd0;
      clr_array[1] = 1'd1;
      clr_array[2] = 1'd0;
      clr_array[3] = 1'd0;
      clr_array[4] = 1'd0;
      clr_array[5] = 1'd0;
      clr_array[6] = 1'd0;
      clr_array[7] = 1'd0;
      clr_array[8] = 1'd0;
      clr_array[9] = 1'd1;
      clr_array[10] = 1'd1;
      clr_array[11] = 1'd0;
      clr_array[12] = 1'd0;
      clr_array[13] = 1'd0;
      clr_array[14] = 1'd0;
      clr_array[15] = 1'd1;
      clr_array[16] = 1'd0;
      clr_array[17] = 1'd0;
      d_array[0] = 4'd3;
      d_array[1] = 4'd2;
      d_array[2] = 4'd1;
      d_array[3] = 4'd0;
      d_array[4] = 4'd1;
      d_array[5] = 4'd2;
      d_array[6] = 4'd3;
      d_array[7] = 4'd4;
      d_array[8] = 4'd5;
      d_array[9] = 4'd6;
      d_array[10] = 4'd7;
      d_array[11] = 4'd8;
      d_array[12] = 4'd5;
      d_array[13] = 4'd4;
      d_array[14] = 4'd6;
      d_array[15] = 4'd3;
      d_array[16] = 4'd2;
      d_array[17] = 4'd1;
    for(j_=0; j_<18; j_=j_+1) begin
        clr=clr_array[j_];
        d=d_array[j_];
      #0.5;
        q_array[j_]=q;
      @(negedge clk);
end
  end
  accum dut (.clr(clr), .d(d), .q(q), .clk(clk));
endmodule