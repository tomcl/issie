module top_module;
  bit [0:0] clr;
  bit [0:0] clr_array [18:0];
  bit [0:0] up_down;
  bit [0:0] up_down_array [18:0];
  bit [3:0] q;
  bit [3:0] q_array [18:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(42) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"q\", \"Values\": [");
    for(i_=0;i_<18; i_=i_+1) begin $write("%d, ", q_array[i_]); end
    $display("%d]}", q_array[18]);
    $write("]");
    $finish(0);
  end
  initial begin
      clr_array[0] = 1'd0;
      clr_array[1] = 1'd0;
      clr_array[2] = 1'd0;
      clr_array[3] = 1'd1;
      clr_array[4] = 1'd0;
      clr_array[5] = 1'd0;
      clr_array[6] = 1'd0;
      clr_array[7] = 1'd0;
      clr_array[8] = 1'd0;
      clr_array[9] = 1'd0;
      clr_array[10] = 1'd0;
      clr_array[11] = 1'd1;
      clr_array[12] = 1'd1;
      clr_array[13] = 1'd0;
      clr_array[14] = 1'd0;
      clr_array[15] = 1'd0;
      clr_array[16] = 1'd0;
      clr_array[17] = 1'd0;
      clr_array[18] = 1'd0;
      up_down_array[0] = 1'd1;
      up_down_array[1] = 1'd1;
      up_down_array[2] = 1'd1;
      up_down_array[3] = 1'd1;
      up_down_array[4] = 1'd1;
      up_down_array[5] = 1'd1;
      up_down_array[6] = 1'd1;
      up_down_array[7] = 1'd0;
      up_down_array[8] = 1'd0;
      up_down_array[9] = 1'd0;
      up_down_array[10] = 1'd0;
      up_down_array[11] = 1'd0;
      up_down_array[12] = 1'd0;
      up_down_array[13] = 1'd0;
      up_down_array[14] = 1'd1;
      up_down_array[15] = 1'd0;
      up_down_array[16] = 1'd1;
      up_down_array[17] = 1'd1;
      up_down_array[18] = 1'd0;
    for(j_=0; j_<19; j_=j_+1) begin
        clr=clr_array[j_];
        up_down=up_down_array[j_];
      #0.5;
        q_array[j_]=q;
      @(negedge clk);
end
  end
  counter dut (.clr(clr), .up_down(up_down), .q(q), .clk(clk));
endmodule