import { readdirSync, readFileSync, statSync } from "node:fs";

import { join } from "node:path";

const __dirname = new URL(".", import.meta.url).pathname;

function getFixtureFileNames(): string[] {
  const fixtureDir = __dirname;
  const fixtureFiles = readdirSync(fixtureDir);
  const fixtureRegex = /[0-9-_]+\.dat/;
  const fixtureFileNames = fixtureFiles.filter((file) => {
    const filePath = join(fixtureDir, file);
    const isFile = statSync(filePath).isFile();
    const isFixtureFile = fixtureRegex.test(file);
    return isFile && isFixtureFile;
  });
  return fixtureFileNames;
}

function asciiToHexValues(asciiArray: Buffer): Buffer {
  // Split the buffer into sets of 2 bytes
  const hexValues = Buffer.alloc(asciiArray.length / 2);
  let count = 0;

  for (let i = 0; i < asciiArray.length / 2; i += 2) {
    const byte = Buffer.from([asciiArray[i], asciiArray[i + 1]]);
    if (byte.length !== 2) {
      throw new Error(`Invalid byte length: ${byte.length}`);
    }
    const hexValue = `0x${byte.toString("utf8")}`;

    hexValues[count] = parseInt(hexValue, 16);
    count++;
  }

  // Join the hex values into a single buffer
  const hexBuffer = hexValues;

  return hexBuffer;
}

/**
 * Loads all fixture files from the fixtures directory.
 * @returns {Buffer[]} An array of Buffers containing the contents of the fixture files.
 */
export function loadFixtureFiles(): Buffer[] {
  const fixtureFileNames = getFixtureFileNames();
  const fixtures: Buffer[] = [];
  for (const fixtureFileName of fixtureFileNames) {
    const fixtureFilePath = join(__dirname, fixtureFileName);
    const fileBuffer = readFileSync(fixtureFilePath);
    fixtures.push(asciiToHexValues(fileBuffer));
  }
  return fixtures;
}
