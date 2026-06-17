import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";

const source = readFileSync("common-api/src/main/java/cn/lili/controller/common/UploadController.java", "utf8");

test("upload directory path normalizes browser placeholder values", () => {
  assert.match(source, /normalizeDirectoryPath\(directoryPath\)/);
  assert.match(source, /"undefined"\.equalsIgnoreCase\(directoryPath\)/);
  assert.match(source, /"null"\.equalsIgnoreCase\(directoryPath\)/);
  assert.match(source, /return "default";/);
});
