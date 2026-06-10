# all-in-one Lite MySQL Redis Compatibility Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在 all-in-one 精简模式只保留 MySQL/Redis 时，恢复 C 端商品搜索、原 XXLJob 定时任务、RocketMQ 发送/消费链路的本地兼容能力。

**Architecture:** 保留现有业务接口和调用方，新增精简模式实现。ES 搜索接口在 `lili.data.elasticsearch.enabled=false` 时从 MySQL 查询 `li_goods_sku/li_goods` 并映射为 `EsGoodsIndex`；XXLJob 禁用时用 Spring `@Scheduled` 调用同一个 `TimedTaskJobHandler`；RocketMQ 禁用时用本地 Spring 事件总线模拟 `topic:tag` 分发到现有 listener，避免大面积修改业务服务。

**Tech Stack:** Spring Boot 3.5、MyBatis-Plus、Spring Events、Spring Scheduling、JUnit/Maven、Docker all-in-one。

---

### Task 1: MySQL-backed C-end goods search

**Files:**
- Modify: `framework/src/main/java/cn/lili/modules/search/serviceimpl/EsGoodsSearchServiceNoop.java`
- Test: `framework/src/test/java/cn/lili/modules/search/serviceimpl/EsGoodsSearchServiceNoopTest.java`

**Steps:**
1. Write tests for keyword/category/brand/price filters and SKU id lookup against mocked `GoodsSkuService` results.
2. Verify the tests fail because current no-op returns empty results.
3. Implement MySQL fallback by querying `GoodsSkuService.getGoodsSkuByList(GoodsSearchParams)` and mapping `GoodsSku` to `EsGoodsIndex`.
4. Aggregate simple selector data from matched records.
5. Run targeted test and Maven package.

### Task 2: Local XXLJob fallback scheduler

**Files:**
- Create: `consumer/src/main/java/cn/lili/timetask/config/LocalTimedTaskScheduler.java`
- Test: `consumer/src/test/java/cn/lili/timetask/config/LocalTimedTaskSchedulerTest.java`

**Steps:**
1. Write tests that scheduler invokes minute/hour/day handler methods when enabled.
2. Implement `@Scheduled` adapter under `xxl.job.enabled=false` and `lili.lite.local-schedule.enabled=true`.
3. Keep all real task execution inside existing `TimedTaskJobHandler`.
4. Run targeted test and Maven package.

### Task 3: Local RocketMQ fallback dispatch

**Files:**
- Replace/modify: `lilishop-all/src/main/java/cn/lili/config/LiteMessageQueueConfig.java`
- Create: `framework/src/main/java/cn/lili/rocketmq/local/LocalRocketMqMessage.java`
- Create: `framework/src/main/java/cn/lili/rocketmq/local/LocalRocketMqTemplate.java`
- Create: `consumer/src/main/java/cn/lili/rocketmq/local/LocalRocketMqDispatcher.java`
- Test: `consumer/src/test/java/cn/lili/rocketmq/local/LocalRocketMqDispatcherTest.java`

**Steps:**
1. Write tests proving `topic:tag` dispatch reaches existing listener with a `MessageExt` containing body and tag.
2. Implement `LocalRocketMqTemplate` overriding common `asyncSend/send` paths to publish `LocalRocketMqMessage`.
3. Implement dispatcher routing configured topics to existing listener beans.
4. Keep unsupported delayed delivery logged and executed immediately unless a later Redis delayed queue is added.
5. Run targeted test and Maven package.

### Task 4: Runtime verification

**Files:**
- Existing Docker all-in-one files only if new env vars are needed.

**Steps:**
1. Build `lilishop-all-in-one:lite`.
2. Start clean `lilishop-all-lite-local`.
3. Verify health, supervisor process list, manager login, C-end search endpoint, logs without ES/RocketMQ/XXL admin connection attempts.
4. Commit per repo.
