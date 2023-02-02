module top_module;
  bit [0:0] en;
  bit [0:0] en_array [9:0];
  bit [3:0] in;
  bit [3:0] in_array [9:0];
  bit [15:0] out;
  bit [15:0] out_array [9:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(24) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"out\", \"Values\": [");
    for(i_=0;i_<9; i_=i_+1) begin $write("%d, ", out_array[i_]); end
    $display("%d]}", out_array[9]);
    $write("]");
    $finish(0);
  end
  initial begin
      en_array[0] = 1'd0;
      en_array[1] = 1'd1;
      en_array[2] = 1'd1;
      en_array[3] = 1'd1;
      en_array[4] = 1'd0;
      en_array[5] = 1'd1;
      en_array[6] = 1'd1;
      en_array[7] = 1'd1;
      en_array[8] = 1'd1;
      en_array[9] = 1'd1;
      in_array[0] = 4'd2;
      in_array[1] = 4'd1;
      in_array[2] = 4'd0;
      in_array[3] = 4'd1;
      in_array[4] = 4'd2;
      in_array[5] = 4'd3;
      in_array[6] = 4'd4;
      in_array[7] = 4'd5;
      in_array[8] = 4'd6;
      in_array[9] = 4'd7;
    for(j_=0; j_<10; j_=j_+1) begin
        en=en_array[j_];
        in=in_array[j_];
      #0.5;
        out_array[j_]=out;
      @(negedge clk);
end
  end
  decoder dut (.en(en), .in(in), .out(out));
endmodule