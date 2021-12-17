//var targetArea = [20,30,-10,-5];
var targetArea = [169, 206, -108, -68]

function step(probe) {
    probe.p.x += probe.v.x;
    probe.p.y += probe.v.y;
    if (probe.v.x > 0) {
        --probe.v.x;
    } else if (probe.v.x < 0) {
        ++probe.v.x;
    }
    --probe.v.y;
}

function withinTarget(p) {
    return p.x >= targetArea[0] && p.x <= targetArea[1] && p.y >= targetArea[2] && p.y <= targetArea[3];
}

function Vector(x, y) {
    this.x = x;
    this.y = y;
}

function Probe(v) {
    this.p = new Vector(0,0);
    this.v = v;
}

function trace(probe) {

    let maxy = probe.p.y;

    for(let i=0; i<2000;i++) {
        if (withinTarget(probe.p)) {
            return { withinTarget: true, maxy }
        }
    
        if (probe.p.y < targetArea[2]) {
            break;
        }

        step(probe);

        if (probe.p.y > maxy) {
            maxy = probe.p.y;
        }
    }

    return { withinTarget: false };
}

let maxy = -100000;
let testcount= 0;
let numHits = 0;

for(let j=-800; j<800; ++j) {
    for(let i=-800; i<800; i++) {
        const probe = new Probe(new Vector(i,j))
        const result = trace(probe);
        if (result.withinTarget) {
            ++numHits;
            if (result.maxy > maxy) {
                console.log("new maxy reached", maxy, result.maxy, i, j, numHits)
                maxy = result.maxy    
            }            
        }
        testcount++;
    }
}

console.log("done", testcount)
console.log("part 1, maxy = ", maxy) // 5778
console.log("part 2, num hits = ", numHits) // 2576


