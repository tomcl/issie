module top_module;
  bit [0:0] enable;
  bit [0:0] enable_array [19:0];
  bit [15:0] encoder_in;
  bit [15:0] encoder_in_array [19:0];
  bit [3:0] binary_out;
  bit [3:0] binary_out_array [19:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(44) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"binary_out\", \"Values\": [");
    for(i_=0;i_<19; i_=i_+1) begin $write("%d, ", binary_out_array[i_]); end
    $display("%d]}", binary_out_array[19]);
    $write("]");
    $finish(0);
  end
  initial begin
      enable_array[0] = 1'd0;
      enable_array[1] = 1'd1;
      enable_array[2] = 1'd1;
      enable_array[3] = 1'd1;
      enable_array[4] = 1'd1;
      enable_array[5] = 1'd1;
      enable_array[6] = 1'd1;
      enable_array[7] = 1'd1;
      enable_array[8] = 1'd1;
      enable_array[9] = 1'd0;
      enable_array[10] = 1'd0;
      enable_array[11] = 1'd1;
      enable_array[12] = 1'd1;
      enable_array[13] = 1'd1;
      enable_array[14] = 1'd1;
      enable_array[15] = 1'd1;
      enable_array[16] = 1'd1;
      enable_array[17] = 1'd1;
      enable_array[18] = 1'd1;
      enable_array[19] = 1'd1;
      encoder_in_array[0] = 16'd1;
      encoder_in_array[1] = 16'd2;
      encoder_in_array[2] = 16'd32768;
      encoder_in_array[3] = 16'd4;
      encoder_in_array[4] = 16'd16384;
      encoder_in_array[5] = 16'd8;
      encoder_in_array[6] = 16'd8192;
      encoder_in_array[7] = 16'd16;
      encoder_in_array[8] = 16'd4096;
      encoder_in_array[9] = 16'd32;
      encoder_in_array[10] = 16'd2048;
      encoder_in_array[11] = 16'd64;
      encoder_in_array[12] = 16'd1024;
      encoder_in_array[13] = 16'd128;
      encoder_in_array[14] = 16'd512;
      encoder_in_array[15] = 16'd256;
      encoder_in_array[16] = 16'd3;
      encoder_in_array[17] = 16'd64;
      encoder_in_array[18] = 16'd4096;
      encoder_in_array[19] = 16'd5000;
    for(j_=0; j_<20; j_=j_+1) begin
        enable=enable_array[j_];
        encoder_in=encoder_in_array[j_];
      #0.5;
        binary_out_array[j_]=binary_out;
      @(negedge clk);
end
  end
  encoder_using_case dut (.enable(enable), .encoder_in(encoder_in), .binary_out(binary_out));
endmodule