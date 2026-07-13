import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";

const source = readFileSync("framework/src/main/java/cn/lili/modules/file/serviceimpl/FileServiceImpl.java", "utf8");

test("store-owned file resource listing supports ownerName filtering", () => {
  const method = source.match(/public IPage<File> customerPageOwner[\s\S]*?return this\.page/);
  assert.ok(method, "customerPageOwner should exist");
  assert.match(method[0], /getOwnerName\(\)/);
  assert.match(method[0], /File::getOwnerName/);
});
