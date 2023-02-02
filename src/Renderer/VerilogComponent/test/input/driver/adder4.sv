module top_module;
  bit [0:0] reset;
  bit [0:0] reset_array [14:0];
  bit [0:0] a;
  bit [0:0] a_array [14:0];
  bit [0:0] b;
  bit [0:0] b_array [14:0];
  bit [0:0] sum;
  bit [0:0] sum_array [14:0];
  bit [0:0] carry;
  bit [0:0] carry_array [14:0];
  bit [3:0] so;
  bit [3:0] so_array [14:0];
  bit [2:0] count;
  bit [2:0] count_array [14:0];
  bit [0:0] cout;
  bit [0:0] cout_array [14:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(34) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"sum\", \"Values\": [");
    for(i_=0;i_<14; i_=i_+1) begin $write("%d, ", sum_array[i_]); end
    $display("%d]}, ", sum_array[14]);
    $write("{\"Label\": \"carry\", \"Values\": [");
    for(i_=0;i_<14; i_=i_+1) begin $write("%d, ", carry_array[i_]); end
    $display("%d]}, ", carry_array[14]);
    $write("{\"Label\": \"so\", \"Values\": [");
    for(i_=0;i_<14; i_=i_+1) begin $write("%d, ", so_array[i_]); end
    $display("%d]}, ", so_array[14]);
    $write("{\"Label\": \"count\", \"Values\": [");
    for(i_=0;i_<14; i_=i_+1) begin $write("%d, ", count_array[i_]); end
    $display("%d]}, ", count_array[14]);
    $write("{\"Label\": \"cout\", \"Values\": [");
    for(i_=0;i_<14; i_=i_+1) begin $write("%d, ", cout_array[i_]); end
    $display("%d]}", cout_array[14]);
    $write("]");
    $finish(0);
  end
  initial begin
      reset_array[0] = 1'd0;
      reset_array[1] = 1'd1;
      reset_array[2] = 1'd0;
      reset_array[3] = 1'd0;
      reset_array[4] = 1'd0;
      reset_array[5] = 1'd0;
      reset_array[6] = 1'd0;
      reset_array[7] = 1'd0;
      reset_array[8] = 1'd0;
      reset_array[9] = 1'd0;
      reset_array[10] = 1'd0;
      reset_array[11] = 1'd0;
      reset_array[12] = 1'd0;
      reset_array[13] = 1'd1;
      reset_array[14] = 1'd0;
      a_array[0] = 1'd1;
      a_array[1] = 1'd0;
      a_array[2] = 1'd1;
      a_array[3] = 1'd1;
      a_array[4] = 1'd0;
      a_array[5] = 1'd0;
      a_array[6] = 1'd1;
      a_array[7] = 1'd0;
      a_array[8] = 1'd1;
      a_array[9] = 1'd0;
      a_array[10] = 1'd1;
      a_array[11] = 1'd1;
      a_array[12] = 1'd0;
      a_array[13] = 1'd1;
      a_array[14] = 1'd0;
      b_array[0] = 1'd1;
      b_array[1] = 1'd0;
      b_array[2] = 1'd0;
      b_array[3] = 1'd0;
      b_array[4] = 1'd0;
      b_array[5] = 1'd1;
      b_array[6] = 1'd0;
      b_array[7] = 1'd0;
      b_array[8] = 1'd1;
      b_array[9] = 1'd1;
      b_array[10] = 1'd0;
      b_array[11] = 1'd0;
      b_array[12] = 1'd0;
      b_array[13] = 1'd1;
      b_array[14] = 1'd1;
    for(j_=0; j_<15; j_=j_+1) begin
        reset=reset_array[j_];
        a=a_array[j_];
        b=b_array[j_];
      #0.5;
        sum_array[j_]=sum;
        carry_array[j_]=carry;
        so_array[j_]=so;
        count_array[j_]=count;
        cout_array[j_]=cout;
      @(negedge clk);
end
  end
  jserialaddlab dut (.reset(reset), .a(a), .b(b), .sum(sum), .carry(carry), .so(so), .count(count), .cout(cout), .clk(clk));
endmodule