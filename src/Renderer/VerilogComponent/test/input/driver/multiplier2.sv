module top_module;
  bit [17:0] a;
  bit [17:0] a_array [21:0];
  bit [17:0] b;
  bit [17:0] b_array [21:0];
  bit [35:0] mult;
  bit [35:0] mult_array [21:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(48) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"mult\", \"Values\": [");
    for(i_=0;i_<21; i_=i_+1) begin $write("%d, ", mult_array[i_]); end
    $display("%d]}", mult_array[21]);
    $write("]");
    $finish(0);
  end
  initial begin
      a_array[0] = 18'd0;
      a_array[1] = 18'd1;
      a_array[2] = 18'd3;
      a_array[3] = 18'd4;
      a_array[4] = 18'd10;
      a_array[5] = 18'd12;
      a_array[6] = 18'd30;
      a_array[7] = 18'd35;
      a_array[8] = 18'd42;
      a_array[9] = 18'd57;
      a_array[10] = 18'd23;
      a_array[11] = 18'd100;
      a_array[12] = 18'd111;
      a_array[13] = 18'd127;
      a_array[14] = 18'd13;
      a_array[15] = 18'd60;
      a_array[16] = 18'd0;
      a_array[17] = 18'd1000;
      a_array[18] = 18'd1300;
      a_array[19] = 18'd1255;
      a_array[20] = 18'd103;
      a_array[21] = 18'd0;
      b_array[0] = 18'd1;
      b_array[1] = 18'd0;
      b_array[2] = 18'd2;
      b_array[3] = 18'd3;
      b_array[4] = 18'd4;
      b_array[5] = 18'd5;
      b_array[6] = 18'd6;
      b_array[7] = 18'd7;
      b_array[8] = 18'd8;
      b_array[9] = 18'd9;
      b_array[10] = 18'd10;
      b_array[11] = 18'd11;
      b_array[12] = 18'd12;
      b_array[13] = 18'd13;
      b_array[14] = 18'd14;
      b_array[15] = 18'd15;
      b_array[16] = 18'd0;
      b_array[17] = 18'd2345;
      b_array[18] = 18'd4321;
      b_array[19] = 18'd5420;
      b_array[20] = 18'd43250;
      b_array[21] = 18'd123;
    for(j_=0; j_<22; j_=j_+1) begin
        a=a_array[j_];
        b=b_array[j_];
      #0.5;
        mult_array[j_]=mult;
      @(negedge clk);
end
  end
  mult dut (.a(a), .b(b), .mult(mult), .clk(clk));
endmodule