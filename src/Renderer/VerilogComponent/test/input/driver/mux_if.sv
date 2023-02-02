module top_module;
  bit [0:0] din_0;
  bit [0:0] din_0_array [7:0];
  bit [0:0] din_1;
  bit [0:0] din_1_array [7:0];
  bit [0:0] sel;
  bit [0:0] sel_array [7:0];
  bit [0:0] mux_out;
  bit [0:0] mux_out_array [7:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(20) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"mux_out\", \"Values\": [");
    for(i_=0;i_<7; i_=i_+1) begin $write("%d, ", mux_out_array[i_]); end
    $display("%d]}", mux_out_array[7]);
    $write("]");
    $finish(0);
  end
  initial begin
      din_0_array[0] = 1'd0;
      din_0_array[1] = 1'd0;
      din_0_array[2] = 1'd0;
      din_0_array[3] = 1'd0;
      din_0_array[4] = 1'd1;
      din_0_array[5] = 1'd1;
      din_0_array[6] = 1'd1;
      din_0_array[7] = 1'd1;
      din_1_array[0] = 1'd0;
      din_1_array[1] = 1'd0;
      din_1_array[2] = 1'd1;
      din_1_array[3] = 1'd1;
      din_1_array[4] = 1'd0;
      din_1_array[5] = 1'd0;
      din_1_array[6] = 1'd1;
      din_1_array[7] = 1'd1;
      sel_array[0] = 1'd0;
      sel_array[1] = 1'd1;
      sel_array[2] = 1'd0;
      sel_array[3] = 1'd1;
      sel_array[4] = 1'd0;
      sel_array[5] = 1'd1;
      sel_array[6] = 1'd0;
      sel_array[7] = 1'd1;
    for(j_=0; j_<8; j_=j_+1) begin
        din_0=din_0_array[j_];
        din_1=din_1_array[j_];
        sel=sel_array[j_];
      #0.5;
        mux_out_array[j_]=mux_out;
      @(negedge clk);
end
  end
  mux_using_if dut (.din_0(din_0), .din_1(din_1), .sel(sel), .mux_out(mux_out));
endmodule