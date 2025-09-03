const fs = require("fs");

function javascript(n) {
    for (let i = 1; i < n; i++) {
        let line = " ".repeat(n - i);
        if (i === 1) {
            line += "*";
        } else {
            line += "*" + " ".repeat(2*i - 3) + "*";
        }
        console.log(line);
    }
    console.log("*".repeat(2*n - 1));
}

const n = parseInt(fs.readFileSync("input.txt", "utf8").trim());
javascript(n);
