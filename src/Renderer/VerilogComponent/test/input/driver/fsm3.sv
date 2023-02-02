module top_module;
  bit [0:0] reset;
  bit [0:0] reset_array [20:0];
  bit [0:0] x1;
  bit [0:0] x1_array [20:0];
  bit [0:0] outp;
  bit [0:0] outp_array [20:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(46) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"outp\", \"Values\": [");
    for(i_=0;i_<20; i_=i_+1) begin $write("%d, ", outp_array[i_]); end
    $display("%d]}", outp_array[20]);
    $write("]");
    $finish(0);
  end
  initial begin
      reset_array[0] = 1'd0;
      reset_array[1] = 1'd1;
      reset_array[2] = 1'd0;
      reset_array[3] = 1'd0;
      reset_array[4] = 1'd0;
      reset_array[5] = 1'd0;
      reset_array[6] = 1'd0;
      reset_array[7] = 1'd0;
      reset_array[8] = 1'd0;
      reset_array[9] = 1'd0;
      reset_array[10] = 1'd0;
      reset_array[11] = 1'd0;
      reset_array[12] = 1'd0;
      reset_array[13] = 1'd0;
      reset_array[14] = 1'd0;
      reset_array[15] = 1'd0;
      reset_array[16] = 1'd1;
      reset_array[17] = 1'd0;
      reset_array[18] = 1'd0;
      reset_array[19] = 1'd0;
      reset_array[20] = 1'd0;
      x1_array[0] = 1'd0;
      x1_array[1] = 1'd1;
      x1_array[2] = 1'd1;
      x1_array[3] = 1'd0;
      x1_array[4] = 1'd1;
      x1_array[5] = 1'd1;
      x1_array[6] = 1'd0;
      x1_array[7] = 1'd1;
      x1_array[8] = 1'd0;
      x1_array[9] = 1'd0;
      x1_array[10] = 1'd0;
      x1_array[11] = 1'd0;
      x1_array[12] = 1'd0;
      x1_array[13] = 1'd0;
      x1_array[14] = 1'd0;
      x1_array[15] = 1'd0;
      x1_array[16] = 1'd0;
      x1_array[17] = 1'd0;
      x1_array[18] = 1'd0;
      x1_array[19] = 1'd0;
      x1_array[20] = 1'd1;
    for(j_=0; j_<21; j_=j_+1) begin
        reset=reset_array[j_];
        x1=x1_array[j_];
      #0.5;
        outp_array[j_]=outp;
      @(negedge clk);
end
  end
  fsm dut (.reset(reset), .x1(x1), .outp(outp), .clk(clk));
endmodule