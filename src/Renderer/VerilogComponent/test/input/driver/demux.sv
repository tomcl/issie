module top_module;
  bit [0:0] f;
  bit [0:0] f_array [14:0];
  bit [1:0] sel;
  bit [1:0] sel_array [14:0];
  bit [0:0] a;
  bit [0:0] a_array [14:0];
  bit [0:0] b;
  bit [0:0] b_array [14:0];
  bit [0:0] c;
  bit [0:0] c_array [14:0];
  bit [0:0] d;
  bit [0:0] d_array [14:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(34) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"a\", \"Values\": [");
    for(i_=0;i_<14; i_=i_+1) begin $write("%d, ", a_array[i_]); end
    $display("%d]}, ", a_array[14]);
    $write("{\"Label\": \"b\", \"Values\": [");
    for(i_=0;i_<14; i_=i_+1) begin $write("%d, ", b_array[i_]); end
    $display("%d]}, ", b_array[14]);
    $write("{\"Label\": \"c\", \"Values\": [");
    for(i_=0;i_<14; i_=i_+1) begin $write("%d, ", c_array[i_]); end
    $display("%d]}, ", c_array[14]);
    $write("{\"Label\": \"d\", \"Values\": [");
    for(i_=0;i_<14; i_=i_+1) begin $write("%d, ", d_array[i_]); end
    $display("%d]}", d_array[14]);
    $write("]");
    $finish(0);
  end
  initial begin
      f_array[0] = 1'd1;
      f_array[1] = 1'd0;
      f_array[2] = 1'd1;
      f_array[3] = 1'd1;
      f_array[4] = 1'd1;
      f_array[5] = 1'd1;
      f_array[6] = 1'd1;
      f_array[7] = 1'd1;
      f_array[8] = 1'd1;
      f_array[9] = 1'd1;
      f_array[10] = 1'd0;
      f_array[11] = 1'd0;
      f_array[12] = 1'd0;
      f_array[13] = 1'd1;
      f_array[14] = 1'd1;
      sel_array[0] = 2'd2;
      sel_array[1] = 2'd1;
      sel_array[2] = 2'd0;
      sel_array[3] = 2'd1;
      sel_array[4] = 2'd2;
      sel_array[5] = 2'd3;
      sel_array[6] = 2'd2;
      sel_array[7] = 2'd3;
      sel_array[8] = 2'd1;
      sel_array[9] = 2'd0;
      sel_array[10] = 2'd1;
      sel_array[11] = 2'd2;
      sel_array[12] = 2'd3;
      sel_array[13] = 2'd3;
      sel_array[14] = 2'd1;
    for(j_=0; j_<15; j_=j_+1) begin
        f=f_array[j_];
        sel=sel_array[j_];
      #0.5;
        a_array[j_]=a;
        b_array[j_]=b;
        c_array[j_]=c;
        d_array[j_]=d;
      @(negedge clk);
end
  end
  demux dut (.f(f), .sel(sel), .a(a), .b(b), .c(c), .d(d));
endmodule