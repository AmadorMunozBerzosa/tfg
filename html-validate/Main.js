const himalaya = require('himalaya');
const {readFile, writeFile, unlink } = require("node:fs/promises");
const {parse} = require('node:path');
const { execFile } = require('node:child_process');
const { stderr } = require('node:process');


const transform = async file => {
    let html, json;

    try {
        html = await readFile(file, {encoding: "utf-8"});
    } catch(error) {
        console.error(`Error: Could not read the input file \"${file}\"`);
        return;
    }

    try {
        json = await himalaya.parse(html, {... himalaya.parseDefaults, includePositions: true});
    } catch(error) {
        console.error("Error: The file doesn't contain valid HTML");
        return;
    }

    const outputFile = `${parse(file).name}.json`;
    
    try {
        await writeFile(outputFile, JSON.stringify(json));
    } catch(error) {
        console.error("Error: Couldn't write the output file");
    }
        
    execFile( __dirname + "/../build/exec/cli", [outputFile], async (error,stdout,stderr) => {
        try {
            if (error) {
                console.error(`Execution error: ${error}`);
                return;
            }
            console.log(stdout);
            if(stderr !== "") {
                console.error(stderr);    
            }
        }
        finally {
            await unlink(outputFile);
        }
    })
}

const [,,...args] = process.argv;

if (args.length === 0) {
    console.error("Error: Not enough arguments. Expecting the path of the HTML file");
} else if (args.length > 1) {
    console.error("Error: Too many arguments. Expecting 1");
} else {
    transform(args[0]);
}