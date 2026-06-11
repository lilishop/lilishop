# All-in-One Lite Acceptance Test Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 验证 `lilishop-all-in-one:lite` 镜像在只依赖 MySQL + Redis 的情况下，用户可以通过浏览器完整使用买家 PC、H5、商家端、管理端基础功能，并用数据库、Redis、日志和必要的定时任务证据闭环。

**Architecture:** 以新镜像启动一个干净容器，先做服务级健康检查，再用浏览器执行端到端用户路径。所有会改数据的动作都使用可识别测试数据或可回滚配置，执行前后记录 DB/Redis/API/日志证据，避免只凭页面“看起来正常”下结论。

**Tech Stack:** Docker, Spring Boot lilishop-all, MariaDB/MySQL, Redis, Nginx, buyer/seller/manager PC web, uniapp H5, Browser automation, curl, SQL, redis-cli.

---

## 测试边界和审批点

本计划默认验证“本地一镜像完整体验可用”，不是验证生产级 ES/RocketMQ/XXLJob 等价能力。

默认不做真实支付、真实短信、真实物流、真实第三方登录、真实微信小程序/公众号回调、线上 OSS 上传覆盖。

默认允许执行以下低风险写操作：
- 新增或修改一条带明确前缀的测试数据，例如 `CODEx_TEST_YYYYMMDDHHMM`。
- 买家端登录、加入购物车、提交未支付订单，如流程可用则最后取消或保留为测试订单。
- 商家端对测试商品做上架/下架、价格或库存小范围编辑，并在结束时回滚。
- 管理端修改一项可回滚系统配置，并在结束时改回原值。
- 为触发定时任务，可以创建或调整一条测试订单/充值/优惠券相关数据，必须在记录原值后执行，并在报告中说明是否回滚。

需要你审批或补充的点：
- 是否允许我创建测试订单。
- 是否允许我在商家端创建一个测试商品。
- 是否允许我临时修改系统设置并回滚。
- 是否需要覆盖 IM 页面。

## Task 1: 镜像和容器启动验收

**Files:**
- No code changes.

**Step 1: 确认当前镜像版本**

Run:
```bash
docker image ls lilishop-all-in-one:lite --format '{{.Repository}}:{{.Tag}} {{.ID}} {{.Size}}'
```

Expected:
- 输出镜像 ID 和大小。
- 记录到最终报告。

**Step 2: 干净重启容器**

Run:
```bash
docker rm -f lilishop-all-lite-uat 2>/dev/null || true
docker run -d --name lilishop-all-lite-uat \
  -p 18082:80 \
  -p 18083:18081 \
  -p 13307:3306 \
  -p 16380:6379 \
  lilishop-all-in-one:lite
```

Expected:
- 容器启动成功。

**Step 3: 等待健康状态**

Run:
```bash
docker inspect --format '{{.State.Status}} {{if .State.Health}}{{.State.Health.Status}}{{else}}no-health{{end}}' lilishop-all-lite-uat
docker exec lilishop-all-lite-uat supervisorctl status
```

Expected:
- 容器 `running healthy`。
- 仅有 `mariadb`、`redis`、`lilishop`、`nginx`、`h5` 为 RUNNING。
- 不应出现 ES、RocketMQ、XXLJob Admin 进程。

**Step 4: 验证运行配置**

Run:
```bash
docker exec lilishop-all-lite-uat sh -lc 'sed -n "1,220p" /opt/lilishop/runtime/application-runtime.yml'
```

Expected:
- `spring.data.elasticsearch.repositories.enabled: false`
- `lili.data.elasticsearch.enabled: false`
- `xxl.job.enabled: false`
- Redis 指向 `127.0.0.1:6379`
- MySQL 指向 `127.0.0.1:3306/lilishop`
- `lili.system.isDemoSite: false`

## Task 2: 基础 API 和静态资源验收

**Files:**
- No code changes.

**Step 1: 验证核心入口 HTTP 状态**

Run:
```bash
curl -i 'http://127.0.0.1:18082/'
curl -i 'http://127.0.0.1:18082/h5/'
curl -i 'http://127.0.0.1:18082/manager/'
curl -i 'http://127.0.0.1:18082/seller/'
curl -i 'http://127.0.0.1:18082/api/api/common/common/site'
```

Expected:
- PC、H5、管理端、商家端 HTML 均返回 200。
- `common/site` 返回 `code=200`。

**Step 2: 检查前端配置地址**

Run:
```bash
curl -sS 'http://127.0.0.1:18082/config.local.js'
curl -sS 'http://127.0.0.1:18082/h5/config.local.js'
curl -sS 'http://127.0.0.1:18082/manager/config.local.js'
curl -sS 'http://127.0.0.1:18082/seller/config.local.js'
```

Expected:
- 所有 API base 指向当前容器入口或相对路径。
- 不应硬编码到线上 `pickmall.cn` API。

## Task 3: 测试账号和测试数据盘点

**Files:**
- No code changes.

**Step 1: 从数据库确认账号**

Run:
```bash
docker exec lilishop-all-lite-uat mysql -uroot -plilishop -D lilishop -e "
SELECT username, nick_name, disabled FROM li_member LIMIT 10;
SELECT username, nick_name, delete_flag FROM li_admin_user LIMIT 10;
SELECT member_name, store_name, store_disable FROM li_store LIMIT 10;
"
```

Expected:
- 找到可登录买家账号、管理账号、商家账号。
- 如果密码不可确认，继续从 SQL 初始化文件或已有说明中定位默认密码；不能猜测。

**Step 2: 查找可用于商品筛选的样本**

Run:
```bash
docker exec lilishop-all-lite-uat mysql -uroot -plilishop -D lilishop -e "
SELECT gs.id sku_id, gs.goods_id, gs.goods_name, gs.price, gs.brand_id, gs.category_path, g.params
FROM li_goods_sku gs
JOIN li_goods g ON gs.goods_id = g.id
WHERE gs.market_enable='UPPER'
  AND gs.auth_flag='PASS'
  AND gs.delete_flag=0
  AND g.params IS NOT NULL
  AND g.params <> '[]'
LIMIT 20;
"
```

Expected:
- 记录至少 1 个有 `params` 的 SKU，用于浏览器参数筛选验收。

## Task 4: 买家 PC 端浏览器验收

**Files:**
- No code changes.

**Step 1: 打开 PC 首页**

Use Browser:
- Navigate to `http://127.0.0.1:18082/`
- 等待首页渲染。

Expected:
- 页面无空白。
- 控制台无阻断性错误。
- 首页基础区块、导航、商品入口可见。

**Step 2: 商品列表基础搜索**

Use Browser:
- 进入商品列表页。
- 搜索一个数据库中存在的关键字，例如从 Task 3 选出的商品名称片段。

Expected:
- 列表出现匹配商品。
- 页面请求 `/api/buyer/goods/goods/es` 返回 `code=200`。

**Step 3: 参数筛选回归**

Use Browser:
- 在商品列表选择一个参数筛选项，例如 `内存=16G`。
- 如果 UI 无法直接暴露该项，则用 URL 参数进入等效列表：`/goodsList?prop=内存_16G`，并同步用 API 取证。

Run:
```bash
curl -sS 'http://127.0.0.1:18082/api/buyer/goods/goods/es?pageNumber=1&pageSize=10&prop=%E5%86%85%E5%AD%98_16G'
curl -sS 'http://127.0.0.1:18082/api/buyer/goods/goods/es/related?pageNumber=1&pageSize=10'
```

Expected:
- 参数筛选后结果数量收窄。
- 返回商品 `attrList` 包含所选参数。
- `related.paramOptions` 非空。

**Step 4: 商品详情**

Use Browser:
- 点击商品进入详情页。

Expected:
- 商品名称、价格、规格、图片、详情区域渲染。
- 详情接口返回 200。

**Step 5: 买家登录和购物车**

Use Browser:
- 使用 Task 3 确认的买家账号登录。
- 在商品详情选择规格并加入购物车。
- 进入购物车。

Expected:
- 登录后 token/cookie/localStorage 生效。
- 购物车出现刚加入商品。

Run:
```bash
docker exec lilishop-all-lite-uat redis-cli -a lilishop --no-auth-warning DBSIZE
docker exec lilishop-all-lite-uat mysql -uroot -plilishop -D lilishop -e "
SELECT * FROM li_member_cart ORDER BY create_time DESC LIMIT 5;
"
```

Expected:
- Redis 有会话/缓存类 key 增长或存在。
- 购物车 DB 记录能对应当前商品和会员。

## Task 5: H5 浏览器验收

**Files:**
- No code changes.

**Step 1: 打开 H5 首页**

Use Browser:
- Navigate to `http://127.0.0.1:18082/h5/`
- 使用移动端 viewport。

Expected:
- 不再出现 `getStorageSync is not yet implemented` 这类阻断错误。
- 首页可渲染、可滚动。

**Step 2: H5 商品列表和详情**

Use Browser:
- 进入 H5 商品列表。
- 搜索同一个商品关键字。
- 进入商品详情。

Expected:
- 列表和详情接口返回 200。
- 页面无白屏，无阻断性控制台错误。

**Step 3: H5 登录和购物车**

Use Browser:
- 使用买家账号登录。
- 加入购物车。

Expected:
- 登录态可保持。
- 购物车商品可见。
- DB 中有对应购物车记录。

## Task 6: 管理端浏览器验收

**Files:**
- No code changes.

**Step 1: 管理端登录**

Use Browser:
- Navigate to `http://127.0.0.1:18082/manager/`
- 使用 Task 3 确认的管理账号登录。

Expected:
- 登录成功进入管理端。
- 菜单加载，不白屏。

**Step 2: 商品管理搜索**

Use Browser:
- 进入商品管理列表。
- 搜索 Task 3 选出的商品名称或 SKU。

Expected:
- 能搜索到商品。
- 证明之前“管理后台商品搜不到数据”的问题在当前环境不复现。

**Step 3: 系统设置可写性验证**

Use Browser:
- 读取一个低风险配置字段原值，例如站点名称或备案文本。
- 修改为 `CODEx_TEST_YYYYMMDDHHMM`。
- 保存后刷新确认生效。
- 立即改回原值。

Run:
```bash
docker exec lilishop-all-lite-uat mysql -uroot -plilishop -D lilishop -e "
SELECT id, setting_value FROM li_setting WHERE id='BASE_SETTING';
"
```

Expected:
- `isDemoSite=false` 下保存不被演示站拦截。
- 保存成功且可回滚。

## Task 7: 商家端浏览器验收

**Files:**
- No code changes.

**Step 1: 商家端登录**

Use Browser:
- Navigate to `http://127.0.0.1:18082/seller/`
- 使用 Task 3 确认的商家账号登录。

Expected:
- 登录成功进入商家后台。
- 菜单加载，不白屏。

**Step 2: 商家商品列表和搜索**

Use Browser:
- 进入商品列表。
- 搜索当前店铺已有商品。

Expected:
- 商品列表可加载。
- 搜索结果与 DB 中店铺商品对应。

**Step 3: 商家低风险写操作**

Use Browser:
- 优先对测试商品执行库存或价格微调。
- 如无测试商品，经审批后创建 `CODEx_TEST_YYYYMMDDHHMM` 商品。

Expected:
- 保存成功。
- 买家端搜索可见对应变更。
- 若操作已有商品，结束前回滚原值。

## Task 8: MQ 本地替换业务触发验证

**Files:**
- No code changes unless execution时发现真实缺陷。

**Step 1: 验证发送入口静态覆盖**

Run:
```bash
rg -n "rocketMQTemplate\\.|asyncSend\\(|syncSend\\(|sendOneWay\\(" framework/src/main/java consumer/src/main/java lilishop-all/src/main/java -g '*.java'
rg -n "@RocketMQMessageListener|implements RocketMQListener" consumer/src/main/java -g '*.java'
```

Expected:
- 当前发送方式均被 `LocalRocketMqTemplate` 覆盖。
- consumer 侧 topic 均被 `LocalRocketMqDispatcher` 覆盖。

**Step 2: 用真实业务动作触发本地 MQ**

Use Browser:
- 买家查看商品详情，触发足迹或商品浏览类消息。
- 买家提交购物车/订单，如审批允许。
- 商家修改商品，如审批允许。

Run:
```bash
docker exec lilishop-all-lite-uat sh -lc 'tail -n 300 /data/logs/lilishop.log'
docker exec lilishop-all-lite-uat mysql -uroot -plilishop -D lilishop -e "
SELECT * FROM li_member_footprint ORDER BY create_time DESC LIMIT 10;
SELECT id, goods_name, update_time FROM li_goods ORDER BY update_time DESC LIMIT 10;
"
```

Expected:
- 本地 MQ 不报 `No route info`、broker 连接、producer 初始化类错误。
- 对应业务副作用落库或缓存变化能被证明。

## Task 9: 定时任务验证

**Files:**
- No code changes unless执行时确认必须增加可测试入口。

**Step 1: 本地 scheduler 入口验证**

Run:
```bash
docker exec lilishop-all-lite-uat sh -lc 'grep -E "每分钟任务执行|每小时任务执行|每日任务执行" /data/logs/lilishop.log | tail -n 20'
```

Expected:
- 至少出现 `每分钟任务执行`。
- 如果测试时间覆盖整点或零点，记录每小时/每日任务日志。

**Step 2: 手动构造可观察的分钟级任务样本**

Execution note:
- 先读取 `CancelOrderTaskExecute`、`RechargeOrderTaskExecute` 的实际筛选条件。
- 若审批允许，创建或调整一条 `CODEx_TEST` 未支付订单/充值记录，让它满足分钟任务处理条件。
- 等待下一次本地 scheduler 执行。

Run:
```bash
docker exec lilishop-all-lite-uat sh -lc 'grep -E "每分钟任务执行|取消|订单|充值" /data/logs/lilishop.log | tail -n 100'
docker exec lilishop-all-lite-uat mysql -uroot -plilishop -D lilishop -e "
SELECT sn, order_status, pay_status, cancel_reason, update_time FROM li_order ORDER BY update_time DESC LIMIT 10;
"
```

Expected:
- 分钟任务由本地 scheduler 触发。
- 测试订单状态按原业务规则变化，或明确记录该任务无符合条件数据。

## Task 10: Redis 和缓存验证

**Files:**
- No code changes.

**Step 1: 登录前后 Redis 对比**

Run:
```bash
docker exec lilishop-all-lite-uat redis-cli -a lilishop --no-auth-warning DBSIZE
docker exec lilishop-all-lite-uat redis-cli -a lilishop --no-auth-warning --scan | head -n 50
```

Expected:
- Redis 可访问。
- 登录、站点设置、商品缓存、验证码/session/token 等 key 可观察。

**Step 2: 配置修改后的缓存一致性**

Use Browser:
- 配合 Task 6 的系统设置修改和回滚。

Run:
```bash
curl -sS 'http://127.0.0.1:18082/api/api/common/common/site'
docker exec lilishop-all-lite-uat redis-cli -a lilishop --no-auth-warning --scan | grep -i setting | head
```

Expected:
- API 读取结果与 DB/页面一致。
- 如存在缓存，保存后缓存不会导致旧值一直返回。

## Task 11: 最终报告

**Files:**
- Create: `docs/verification/2026-06-11-allinone-lite-acceptance-report.md`

**Step 1: 汇总通过项**

Report must include:
- 镜像 ID、容器 ID、端口、健康状态。
- 浏览器页面截图或明确页面状态。
- 关键 API 请求和响应摘要。
- DB/Redis 验证 SQL 和结果摘要。
- 定时任务日志证据。
- MQ 本地替代验证证据。

**Step 2: 汇总失败项和边界**

Report must include:
- 失败功能、复现步骤、接口/日志/DB 证据。
- 是否已修复，若未修复给出原因和后续任务。
- 明确说明哪些能力没有覆盖，例如真实支付、短信、三方登录。

**Step 3: 判断是否达到“可完整使用”**

Expected:
- 如果 C 端 PC/H5、商家端、管理端、搜索筛选、购物车/订单基础链路、Redis、MySQL、本地定时任务、本地 MQ smoke 都通过，则标记为可本地完整体验。
- 若任一关键链路失败，不标记完成，继续进入修复任务。

