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
    fixtures.push(fileBuffer);
  }
  return fixtures;
}
