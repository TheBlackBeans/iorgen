"use strict";

/**
 * @param {{foo: number, bar: number}} struct a struct 1 instance
 * @param {number} n a number
 * @param {Array.<{foo: number, bar: number}>} structList a list a struct 1
 * @param {Array.<{name: string, pos: {x: number, y: number, z: number}}>} triangle a triangle
 * @param {{'first char': string, 'second char': string, 'third char': string}} structChars a struct of chars
 * @returns {void}
 */
function structs(struct, n, structList, triangle, structChars) {
    /* TODO Look at them structs. */
}

function main(stdin) {
    let line = 0;

    const words = stdin[line++].split(" ");
    const struct = {
        foo: Number(words[0]),
        bar: Number(words[1])
    };
    const n = Number(stdin[line++]);
    const structList = [];
    for (let i = 0; i < n; i++) {
        const words1 = stdin[line++].split(" ");
        const j = {
            foo: Number(words1[0]),
            bar: Number(words1[1])
        };
        structList.push(j);
    }
    const triangle = [];
    for (let i = 0; i < 3; i++) {
        const j = {};
        j.name = stdin[line++];
        const words1 = stdin[line++].split(" ");
        j.pos = {
            x: Number(words1[0]),
            y: Number(words1[1]),
            z: Number(words1[2])
        };
        triangle.push(j);
    }
    const words1 = stdin[line++].split(" ");
    const structChars = {
        firstChar: words1[0],
        secondChar: words1[1],
        thirdChar: words1[2]
    };
    structs(struct, n, structList, triangle, structChars);
}

let stdin = "";
process.stdin.on("data", data => stdin += data.toString())
             .on("end", () => main(stdin.split("\n")));
