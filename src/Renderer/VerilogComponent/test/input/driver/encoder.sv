module top_module;
  bit [7:0] sel;
  bit [7:0] sel_array [15:0];
  bit [2:0] code;
  bit [2:0] code_array [15:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(36) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"code\", \"Values\": [");
    for(i_=0;i_<15; i_=i_+1) begin $write("%d, ", code_array[i_]); end
    $display("%d]}", code_array[15]);
    $write("]");
    $finish(0);
  end
  initial begin
      sel_array[0] = 8'd0;
      sel_array[1] = 8'd1;
      sel_array[2] = 8'd2;
      sel_array[3] = 8'd4;
      sel_array[4] = 8'd8;
      sel_array[5] = 8'd16;
      sel_array[6] = 8'd32;
      sel_array[7] = 8'd64;
      sel_array[8] = 8'd128;
      sel_array[9] = 8'd3;
      sel_array[10] = 8'd5;
      sel_array[11] = 8'd10;
      sel_array[12] = 8'd48;
      sel_array[13] = 8'd129;
      sel_array[14] = 8'd80;
      sel_array[15] = 8'd132;
    for(j_=0; j_<16; j_=j_+1) begin
        sel=sel_array[j_];
      #0.5;
        code_array[j_]=code;
      @(negedge clk);
end
  end
  priority_encoder dut (.sel(sel), .code(code));
endmodule