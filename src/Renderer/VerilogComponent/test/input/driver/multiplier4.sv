module top_module;
  bit [15:0] ar;
  bit [15:0] ar_array [7:0];
  bit [15:0] ai;
  bit [15:0] ai_array [7:0];
  bit [17:0] br;
  bit [17:0] br_array [7:0];
  bit [17:0] bi;
  bit [17:0] bi_array [7:0];
  bit [34:0] pr;
  bit [34:0] pr_array [7:0];
  bit [34:0] pi;
  bit [34:0] pi_array [7:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(20) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"pr\", \"Values\": [");
    for(i_=0;i_<7; i_=i_+1) begin $write("%d, ", pr_array[i_]); end
    $display("%d]}, ", pr_array[7]);
    $write("{\"Label\": \"pi\", \"Values\": [");
    for(i_=0;i_<7; i_=i_+1) begin $write("%d, ", pi_array[i_]); end
    $display("%d]}", pi_array[7]);
    $write("]");
    $finish(0);
  end
  initial begin
      ar_array[0] = 16'd1;
      ar_array[1] = 16'd5;
      ar_array[2] = 16'd210;
      ar_array[3] = 16'd3000;
      ar_array[4] = 16'd43;
      ar_array[5] = 16'd256;
      ar_array[6] = 16'd7654;
      ar_array[7] = 16'd23;
      ai_array[0] = 16'd1;
      ai_array[1] = 16'd5;
      ai_array[2] = 16'd210;
      ai_array[3] = 16'd300;
      ai_array[4] = 16'd43;
      ai_array[5] = 16'd256;
      ai_array[6] = 16'd764;
      ai_array[7] = 16'd23;
      br_array[0] = 18'd1;
      br_array[1] = 18'd5;
      br_array[2] = 18'd210;
      br_array[3] = 18'd3000;
      br_array[4] = 18'd43;
      br_array[5] = 18'd256;
      br_array[6] = 18'd7654;
      br_array[7] = 18'd0;
      bi_array[0] = 18'd1;
      bi_array[1] = 18'd5;
      bi_array[2] = 18'd210;
      bi_array[3] = 18'd1000;
      bi_array[4] = 18'd43;
      bi_array[5] = 18'd256;
      bi_array[6] = 18'd7654;
      bi_array[7] = 18'd0;
    for(j_=0; j_<8; j_=j_+1) begin
        ar=ar_array[j_];
        ai=ai_array[j_];
        br=br_array[j_];
        bi=bi_array[j_];
      #0.5;
        pr_array[j_]=pr;
        pi_array[j_]=pi;
      @(negedge clk);
end
  end
  cmult dut (.ar(ar), .ai(ai), .br(br), .bi(bi), .pr(pr), .pi(pi), .clk(clk));
endmodule