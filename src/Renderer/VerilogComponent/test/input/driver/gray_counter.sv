module top_module;
  bit [0:0] rst;
  bit [0:0] rst_array [15:0];
  bit [0:0] enable;
  bit [0:0] enable_array [15:0];
  bit [7:0] out;
  bit [7:0] out_array [15:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(36) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"out\", \"Values\": [");
    for(i_=0;i_<15; i_=i_+1) begin $write("%d, ", out_array[i_]); end
    $display("%d]}", out_array[15]);
    $write("]");
    $finish(0);
  end
  initial begin
      rst_array[0] = 1'd0;
      rst_array[1] = 1'd1;
      rst_array[2] = 1'd0;
      rst_array[3] = 1'd0;
      rst_array[4] = 1'd0;
      rst_array[5] = 1'd0;
      rst_array[6] = 1'd0;
      rst_array[7] = 1'd0;
      rst_array[8] = 1'd0;
      rst_array[9] = 1'd0;
      rst_array[10] = 1'd0;
      rst_array[11] = 1'd0;
      rst_array[12] = 1'd0;
      rst_array[13] = 1'd1;
      rst_array[14] = 1'd0;
      rst_array[15] = 1'd0;
      enable_array[0] = 1'd1;
      enable_array[1] = 1'd0;
      enable_array[2] = 1'd1;
      enable_array[3] = 1'd1;
      enable_array[4] = 1'd1;
      enable_array[5] = 1'd1;
      enable_array[6] = 1'd1;
      enable_array[7] = 1'd1;
      enable_array[8] = 1'd1;
      enable_array[9] = 1'd1;
      enable_array[10] = 1'd0;
      enable_array[11] = 1'd0;
      enable_array[12] = 1'd1;
      enable_array[13] = 1'd1;
      enable_array[14] = 1'd1;
      enable_array[15] = 1'd1;
    for(j_=0; j_<16; j_=j_+1) begin
        rst=rst_array[j_];
        enable=enable_array[j_];
      #0.5;
        out_array[j_]=out;
      @(negedge clk);
end
  end
  gray_counter dut (.rst(rst), .enable(enable), .out(out), .clk(clk));
endmodule