module top_module;
  bit [3:0] a;
  bit [3:0] a_array [15:0];
  bit [3:0] b;
  bit [3:0] b_array [15:0];
  bit [0:0] cin;
  bit [0:0] cin_array [15:0];
  bit [3:0] sum;
  bit [3:0] sum_array [15:0];
  bit [0:0] cout;
  bit [0:0] cout_array [15:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(36) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"sum\", \"Values\": [");
    for(i_=0;i_<15; i_=i_+1) begin $write("%d, ", sum_array[i_]); end
    $display("%d]}, ", sum_array[15]);
    $write("{\"Label\": \"cout\", \"Values\": [");
    for(i_=0;i_<15; i_=i_+1) begin $write("%d, ", cout_array[i_]); end
    $display("%d]}", cout_array[15]);
    $write("]");
    $finish(0);
  end
  initial begin
      a_array[0] = 4'd1;
      a_array[1] = 4'd0;
      a_array[2] = 4'd1;
      a_array[3] = 4'd3;
      a_array[4] = 4'd4;
      a_array[5] = 4'd5;
      a_array[6] = 4'd6;
      a_array[7] = 4'd7;
      a_array[8] = 4'd8;
      a_array[9] = 4'd9;
      a_array[10] = 4'd10;
      a_array[11] = 4'd11;
      a_array[12] = 4'd12;
      a_array[13] = 4'd13;
      a_array[14] = 4'd14;
      a_array[15] = 4'd15;
      b_array[0] = 4'd13;
      b_array[1] = 4'd0;
      b_array[2] = 4'd1;
      b_array[3] = 4'd2;
      b_array[4] = 4'd3;
      b_array[5] = 4'd12;
      b_array[6] = 4'd15;
      b_array[7] = 4'd7;
      b_array[8] = 4'd5;
      b_array[9] = 4'd6;
      b_array[10] = 4'd4;
      b_array[11] = 4'd3;
      b_array[12] = 4'd2;
      b_array[13] = 4'd8;
      b_array[14] = 4'd10;
      b_array[15] = 4'd11;
      cin_array[0] = 1'd0;
      cin_array[1] = 1'd0;
      cin_array[2] = 1'd0;
      cin_array[3] = 1'd0;
      cin_array[4] = 1'd1;
      cin_array[5] = 1'd0;
      cin_array[6] = 1'd0;
      cin_array[7] = 1'd1;
      cin_array[8] = 1'd0;
      cin_array[9] = 1'd0;
      cin_array[10] = 1'd0;
      cin_array[11] = 1'd0;
      cin_array[12] = 1'd0;
      cin_array[13] = 1'd0;
      cin_array[14] = 1'd0;
      cin_array[15] = 1'd1;
    for(j_=0; j_<16; j_=j_+1) begin
        a=a_array[j_];
        b=b_array[j_];
        cin=cin_array[j_];
      #0.5;
        sum_array[j_]=sum;
        cout_array[j_]=cout;
      @(negedge clk);
end
  end
  fulladder dut (.a(a), .b(b), .cin(cin), .sum(sum), .cout(cout));
endmodule