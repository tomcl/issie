module top_module;
  bit [7:0] a;
  bit [7:0] a_array [16:0];
  bit [3:0] b;
  bit [3:0] b_array [16:0];
  bit [11:0] res;
  bit [11:0] res_array [16:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(38) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"res\", \"Values\": [");
    for(i_=0;i_<16; i_=i_+1) begin $write("%d, ", res_array[i_]); end
    $display("%d]}", res_array[16]);
    $write("]");
    $finish(0);
  end
  initial begin
      a_array[0] = 8'd0;
      a_array[1] = 8'd1;
      a_array[2] = 8'd3;
      a_array[3] = 8'd4;
      a_array[4] = 8'd10;
      a_array[5] = 8'd12;
      a_array[6] = 8'd30;
      a_array[7] = 8'd35;
      a_array[8] = 8'd42;
      a_array[9] = 8'd57;
      a_array[10] = 8'd23;
      a_array[11] = 8'd100;
      a_array[12] = 8'd111;
      a_array[13] = 8'd127;
      a_array[14] = 8'd13;
      a_array[15] = 8'd60;
      a_array[16] = 8'd0;
      b_array[0] = 4'd1;
      b_array[1] = 4'd0;
      b_array[2] = 4'd2;
      b_array[3] = 4'd3;
      b_array[4] = 4'd4;
      b_array[5] = 4'd5;
      b_array[6] = 4'd6;
      b_array[7] = 4'd7;
      b_array[8] = 4'd8;
      b_array[9] = 4'd9;
      b_array[10] = 4'd10;
      b_array[11] = 4'd11;
      b_array[12] = 4'd12;
      b_array[13] = 4'd13;
      b_array[14] = 4'd14;
      b_array[15] = 4'd15;
      b_array[16] = 4'd0;
    for(j_=0; j_<17; j_=j_+1) begin
        a=a_array[j_];
        b=b_array[j_];
      #0.5;
        res_array[j_]=res;
      @(negedge clk);
end
  end
  mult dut (.a(a), .b(b), .res(res));
endmodule