module top_module;
  bit [0:0] t;
  bit [0:0] t_array [13:0];
  bit [0:0] i;
  bit [0:0] i_array [13:0];
  bit [0:0] o;
  bit [0:0] o_array [13:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(32) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"o\", \"Values\": [");
    for(i_=0;i_<13; i_=i_+1) begin $write("%d, ", o_array[i_]); end
    $display("%d]}", o_array[13]);
    $write("]");
    $finish(0);
  end
  initial begin
      t_array[0] = 1'd0;
      t_array[1] = 1'd1;
      t_array[2] = 1'd0;
      t_array[3] = 1'd1;
      t_array[4] = 1'd1;
      t_array[5] = 1'd1;
      t_array[6] = 1'd1;
      t_array[7] = 1'd0;
      t_array[8] = 1'd0;
      t_array[9] = 1'd1;
      t_array[10] = 1'd0;
      t_array[11] = 1'd1;
      t_array[12] = 1'd0;
      t_array[13] = 1'd1;
      i_array[0] = 1'd1;
      i_array[1] = 1'd0;
      i_array[2] = 1'd0;
      i_array[3] = 1'd1;
      i_array[4] = 1'd0;
      i_array[5] = 1'd1;
      i_array[6] = 1'd1;
      i_array[7] = 1'd0;
      i_array[8] = 1'd0;
      i_array[9] = 1'd1;
      i_array[10] = 1'd1;
      i_array[11] = 1'd1;
      i_array[12] = 1'd1;
      i_array[13] = 1'd0;
    for(j_=0; j_<14; j_=j_+1) begin
        t=t_array[j_];
        i=i_array[j_];
      #0.5;
        o_array[j_]=o;
      @(negedge clk);
end
  end
  three_st dut (.t(t), .i(i), .o(o));
endmodule