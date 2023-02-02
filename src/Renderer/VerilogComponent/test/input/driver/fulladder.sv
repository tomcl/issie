module top_module;
  bit [0:0] a;
  bit [0:0] a_array [7:0];
  bit [0:0] b;
  bit [0:0] b_array [7:0];
  bit [0:0] cin;
  bit [0:0] cin_array [7:0];
  bit [0:0] sum;
  bit [0:0] sum_array [7:0];
  bit [0:0] cout;
  bit [0:0] cout_array [7:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(20) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"sum\", \"Values\": [");
    for(i_=0;i_<7; i_=i_+1) begin $write("%d, ", sum_array[i_]); end
    $display("%d]}, ", sum_array[7]);
    $write("{\"Label\": \"cout\", \"Values\": [");
    for(i_=0;i_<7; i_=i_+1) begin $write("%d, ", cout_array[i_]); end
    $display("%d]}", cout_array[7]);
    $write("]");
    $finish(0);
  end
  initial begin
      a_array[0] = 1'd0;
      a_array[1] = 1'd0;
      a_array[2] = 1'd0;
      a_array[3] = 1'd0;
      a_array[4] = 1'd1;
      a_array[5] = 1'd1;
      a_array[6] = 1'd1;
      a_array[7] = 1'd1;
      b_array[0] = 1'd0;
      b_array[1] = 1'd1;
      b_array[2] = 1'd0;
      b_array[3] = 1'd1;
      b_array[4] = 1'd0;
      b_array[5] = 1'd1;
      b_array[6] = 1'd0;
      b_array[7] = 1'd1;
      cin_array[0] = 1'd0;
      cin_array[1] = 1'd0;
      cin_array[2] = 1'd1;
      cin_array[3] = 1'd1;
      cin_array[4] = 1'd0;
      cin_array[5] = 1'd0;
      cin_array[6] = 1'd1;
      cin_array[7] = 1'd1;
    for(j_=0; j_<8; j_=j_+1) begin
        a=a_array[j_];
        b=b_array[j_];
        cin=cin_array[j_];
      #0.5;
        sum_array[j_]=sum;
        cout_array[j_]=cout;
      @(negedge clk);
end
  end
  fa dut (.a(a), .b(b), .cin(cin), .sum(sum), .cout(cout));
endmodule