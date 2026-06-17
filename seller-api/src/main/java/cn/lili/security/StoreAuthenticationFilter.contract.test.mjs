import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";

const source = readFileSync("seller-api/src/main/java/cn/lili/security/StoreAuthenticationFilter.java", "utf8");

test("store auth filter stops after rejecting an invalid token", () => {
  assert.match(
    source,
    /else if \(StrUtil\.isNotBlank\(jwt\)\) \{\s*return;\s*\}/,
    "filter should not continue into controllers after writing a 403 response",
  );
});

test("store auth filter writes 403 for malformed or expired tokens", () => {
  assert.match(
    source,
    /catch \(Exception e\) \{[\s\S]*?log\.error\("user analysis exception:", e\);[\s\S]*?\}\s*outputInvalidToken\(response\);/,
    "token parse failures should send a 403 response instead of returning an empty 200",
  );
  assert.match(source, /private void outputInvalidToken\(HttpServletResponse response\)/);
  assert.match(source, /ResponseUtil\.output\(response, 403, ResponseUtil\.resultMap\(false, 403, "登录已失效，请重新登录"\)\)/);
});
