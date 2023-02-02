 // source: https://docs.xilinx.com/r/en-US/ug901-vivado-synthesis/Complex-Multiplier-Examples

// Complex Multiplier (pr+i.pi) = (ar+i.ai)*(br+i.bi)

// file: cmult.v

module cmult

(

input bit clk,

input bit [15:0]      ar, ai,

input bit [17:0]      br, bi,

output bit [34:0] pr, pi

);

bit [15:0] ai_d, ai_dd, ai_ddd, ai_dddd   ;

bit [15:0] ar_d, ar_dd, ar_ddd, ar_dddd   ;

bit [17:0] bi_d, bi_dd, bi_ddd, br_d, br_dd, br_ddd ;
bit [16:0]  addcommon     ;

bit [18:0]  addr, addi     ;

bit [34:0] mult0, multr, multi, pr_int, pi_int  ;

bit [34:0] common, commonr1, commonr2   ;

always_ff @(posedge clk)

begin

ar_d   <= ar;

ar_dd  <= ar_d;

ai_d   <= ai;

ai_dd  <= ai_d;

br_d   <= br;

br_dd  <= br_d;

br_ddd <= br_dd;

bi_d   <= bi;

bi_dd  <= bi_d;

bi_ddd <= bi_dd;

end

// Common factor (ar ai) x bi, shared for the calculations of the real and imaginary final products

//

always_ff @(posedge clk)

begin

addcommon <= ar_d - ai_d;

mult0     <= addcommon * bi_dd;

common    <= mult0;

end

// Real product

//

always_ff @(posedge clk)

begin

ar_ddd   <= ar_dd;

ar_dddd  <= ar_ddd;

addr     <= br_ddd - bi_ddd;

multr    <= addr * ar_dddd;

commonr1 <= common;

pr_int   <= multr + commonr1;

end

// Imaginary product

//

always_ff @(posedge clk)

begin

ai_ddd   <= ai_dd;

ai_dddd  <= ai_ddd;

addi     <= br_ddd + bi_ddd;

multi    <= addi * ai_dddd;

commonr2 <= common;

pi_int   <= multi + commonr2;

end

assign pr = pr_int;

assign pi = pi_int;

endmodule // cmult 