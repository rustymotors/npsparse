import { loadFixtureFiles } from "./fixtures/loader.ts";
import { ParseTreeNode, Parser, asciiToHexValues, validateBuffer } from "../lib/index.ts";
import { suite, test, beforeEach } from "node:test";
import { strict as assert } from "node:assert";

suite("Tests", () => {
  let fixtures: Buffer[] = [];

  beforeEach(() => {
    fixtures = loadFixtureFiles();
  });

  suite("Loader sanity check", () => {
    test("Fixture files loaded", () => {
      assert.ok(fixtures.length > 0, "No fixture files loaded");
      for (const fixture of fixtures) {
        for (let i = 0; i < fixture.length; i++) {
          assert.ok(
            fixture[i] >= 0 && fixture[i] <= 255,
            "Fixture file has invalid hex content"
          );
        }
      }
    });
  });

  suite("Parsers", () => {
    test('empty buffer', () => {
      const fixture = fixtures[3];
      const parser = new Parser();
      const valid = validateBuffer('');
      assert.notEqual(valid, null, "Buffer is not empty");
     
    });

    test("parseWord", () => {
      const fixture = fixtures[0];
      const parser = new Parser();

      // Truncate the buffer to to be an even number of bytes
      const truncatedBuffer = fixture.slice(0, fixture.length - (fixture.length % 2));

      parser.parse(truncatedBuffer);

        const validationError = parser.getError();
        assert.equal(validationError, null, "Validation error is not null");

      const parseTree = parser.getParseTree();
      assert.ok(parseTree !== null, "Parse tree is null");
      const word = parseTree.getNode(0);
      assert.notEqual(word, null, "Parsed word is null");
        assert.ok(word instanceof ParseTreeNode, "Parsed word is not a ParseTreeNode");
      assert.equal(word.value, 1281, "Parsed word does not match expected value");
    });
  });
});
