// Trivial Nexys 2 module that does nothing but turn on LED0
module led(output led0);
// synthesis attribute LOC led0 J14
assign led0 = 1;
endmodule
