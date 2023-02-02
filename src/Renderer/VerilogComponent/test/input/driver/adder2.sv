module top_module;
  bit [7:0] a;
  bit [7:0] a_array [13:0];
  bit [7:0] b;
  bit [7:0] b_array [13:0];
  bit [7:0] sum;
  bit [7:0] sum_array [13:0];
  bit [0:0] co;
  bit [0:0] co_array [13:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(32) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"sum\", \"Values\": [");
    for(i_=0;i_<13; i_=i_+1) begin $write("%d, ", sum_array[i_]); end
    $display("%d]}, ", sum_array[13]);
    $write("{\"Label\": \"co\", \"Values\": [");
    for(i_=0;i_<13; i_=i_+1) begin $write("%d, ", co_array[i_]); end
    $display("%d]}", co_array[13]);
    $write("]");
    $finish(0);
  end
  initial begin
      a_array[0] = 8'd12;
      a_array[1] = 8'd34;
      a_array[2] = 8'd127;
      a_array[3] = 8'd0;
      a_array[4] = 8'd100;
      a_array[5] = 8'd105;
      a_array[6] = 8'd75;
      a_array[7] = 8'd64;
      a_array[8] = 8'd87;
      a_array[9] = 8'd93;
      a_array[10] = 8'd51;
      a_array[11] = 8'd13;
      a_array[12] = 8'd25;
      a_array[13] = 8'd37;
      b_array[0] = 8'd0;
      b_array[1] = 8'd40;
      b_array[2] = 8'd5;
      b_array[3] = 8'd100;
      b_array[4] = 8'd35;
      b_array[5] = 8'd72;
      b_array[6] = 8'd84;
      b_array[7] = 8'd91;
      b_array[8] = 8'd101;
      b_array[9] = 8'd111;
      b_array[10] = 8'd117;
      b_array[11] = 8'd34;
      b_array[12] = 8'd41;
      b_array[13] = 8'd45;
    for(j_=0; j_<14; j_=j_+1) begin
        a=a_array[j_];
        b=b_array[j_];
      #0.5;
        sum_array[j_]=sum;
        co_array[j_]=co;
      @(negedge clk);
end
  end
  adder dut (.a(a), .b(b), .sum(sum), .co(co));
endmodule