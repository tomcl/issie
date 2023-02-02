module top_module;
  bit [0:0] reset;
  bit [0:0] reset_array [9:0];
  bit [1:0] MODE;
  bit [1:0] MODE_array [9:0];
  bit [3:0] DATAIN;
  bit [3:0] DATAIN_array [9:0];
  bit [3:0] DATAOUT;
  bit [3:0] DATAOUT_array [9:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(24) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"DATAOUT\", \"Values\": [");
    for(i_=0;i_<9; i_=i_+1) begin $write("%d, ", DATAOUT_array[i_]); end
    $display("%d]}", DATAOUT_array[9]);
    $write("]");
    $finish(0);
  end
  initial begin
      reset_array[0] = 1'd0;
      reset_array[1] = 1'd0;
      reset_array[2] = 1'd0;
      reset_array[3] = 1'd0;
      reset_array[4] = 1'd0;
      reset_array[5] = 1'd1;
      reset_array[6] = 1'd0;
      reset_array[7] = 1'd0;
      reset_array[8] = 1'd0;
      reset_array[9] = 1'd0;
      MODE_array[0] = 2'd3;
      MODE_array[1] = 2'd1;
      MODE_array[2] = 2'd1;
      MODE_array[3] = 2'd1;
      MODE_array[4] = 2'd1;
      MODE_array[5] = 2'd1;
      MODE_array[6] = 2'd3;
      MODE_array[7] = 2'd1;
      MODE_array[8] = 2'd1;
      MODE_array[9] = 2'd1;
      DATAIN_array[0] = 4'd10;
      DATAIN_array[1] = 4'd1;
      DATAIN_array[2] = 4'd3;
      DATAIN_array[3] = 4'd1;
      DATAIN_array[4] = 4'd0;
      DATAIN_array[5] = 4'd1;
      DATAIN_array[6] = 4'd5;
      DATAIN_array[7] = 4'd3;
      DATAIN_array[8] = 4'd4;
      DATAIN_array[9] = 4'd5;
    for(j_=0; j_<10; j_=j_+1) begin
        reset=reset_array[j_];
        MODE=MODE_array[j_];
        DATAIN=DATAIN_array[j_];
      #0.5;
        DATAOUT_array[j_]=DATAOUT;
      @(negedge clk);
end
  end
  juniversalShiftRegister dut (.reset(reset), .MODE(MODE), .DATAIN(DATAIN), .DATAOUT(DATAOUT), .clk(clk));
endmodule