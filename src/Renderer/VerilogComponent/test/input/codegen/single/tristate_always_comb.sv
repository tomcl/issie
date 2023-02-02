        module three_st (t, i, o);
        input  bit t, i;
        output bit o;
        always_comb
        begin
           if (~t)
              o = i; 
           else
              o = 1'b0;
        end
        endmodule