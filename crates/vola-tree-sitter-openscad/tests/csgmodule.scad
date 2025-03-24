module body() {
    color("Blue") sphere(10);
}

module holeA() rotate([0,90,0]) body();

test = true;

if (test) {
    holeA();
}else{
    body();
}
