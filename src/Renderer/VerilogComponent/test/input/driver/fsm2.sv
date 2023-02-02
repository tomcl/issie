module top_module;
  bit [0:0] reset;
  bit [0:0] reset_array [18:0];
  bit [0:0] datain;
  bit [0:0] datain_array [18:0];
  bit [0:0] dataout;
  bit [0:0] dataout_array [18:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(42) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"dataout\", \"Values\": [");
    for(i_=0;i_<18; i_=i_+1) begin $write("%d, ", dataout_array[i_]); end
    $display("%d]}", dataout_array[18]);
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
      reset_array[16] = 1'd0;
      reset_array[17] = 1'd1;
      reset_array[18] = 1'd0;
      datain_array[0] = 1'd1;
      datain_array[1] = 1'd1;
      datain_array[2] = 1'd1;
      datain_array[3] = 1'd0;
      datain_array[4] = 1'd1;
      datain_array[5] = 1'd1;
      datain_array[6] = 1'd1;
      datain_array[7] = 1'd0;
      datain_array[8] = 1'd1;
      datain_array[9] = 1'd1;
      datain_array[10] = 1'd1;
      datain_array[11] = 1'd0;
      datain_array[12] = 1'd0;
      datain_array[13] = 1'd1;
      datain_array[14] = 1'd1;
      datain_array[15] = 1'd1;
      datain_array[16] = 1'd0;
      datain_array[17] = 1'd1;
      datain_array[18] = 1'd1;
    for(j_=0; j_<19; j_=j_+1) begin
        reset=reset_array[j_];
        datain=datain_array[j_];
      #0.5;
        dataout_array[j_]=dataout;
      @(negedge clk);
end
  end
  jfsmMooreWithOverlap dut (.reset(reset), .datain(datain), .dataout(dataout), .clk(clk));
endmodule