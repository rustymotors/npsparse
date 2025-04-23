import { loadFixtureFiles } from "./fixtures/loader.ts";
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
        assert.ok(fixture.length > 0, "Fixture file is empty");
        for (let i = 0; i < fixture.length; i++) {
          assert.ok(
            fixture[i] >= 0 && fixture[i] <= 255,
            "Fixture file has invalid hex content"
          );
        }
      }
    });

    suite("Parse fixture files", () => {
      test("parseWord", () => {
        for (const fixture of fixtures) {
          const word = fixture.readUInt16BE(0);
          assert.ok(
            word >= 0 && word <= 65535,
            "Unable to parse word from fixture file"
          );
        }
      });
    });
  });
});
