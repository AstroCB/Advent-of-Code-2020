// Doing this in JavaScript is cheating but I really don't want to do heavy
// string processing in C++
const fs = require("fs");

const input = "input.txt";
const output = "processed.txt";

const data = fs.readFileSync(input);
const rules = data.toString().split("\n");

if (fs.existsSync(output)) fs.unlinkSync(output);

rules.forEach(line => {
    if (line.trim().length === 0) return;

    const [_, primary, secondaryDesc] = line.match(/(.+) bags contain (.+)/);
    const secondaries = secondaryDesc.split(", ").map(desc => {
        if (desc == "no other bags.") return;

        const [_, num, color] = desc.match(/(\d+) (.+) bags?/);
        return { num, color };
    }).filter(s => s);

    if (secondaries.length > 0) {
        fs.appendFileSync(output, `${primary}\n`);
        secondaries.forEach(({ num, color }) => {
            fs.appendFileSync(output, `${num}\n`);
            fs.appendFileSync(output, `${color}\n`);
        });
        fs.appendFileSync(output, "\n");
    }
});