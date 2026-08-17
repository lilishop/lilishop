# Lilishop 技术栈

> 本文档以本仓库源码与 `pom.xml` 为准，交叉对照 `lilishop`、`lilishop-ui`、`lilishop-uniapp`、`docker` 四个仓库的 `README.md`。  
> README 已声明：首页不再维护不完整的系统架构图和技术栈表。  
> 访谈确认的现存短板见第 5 节；定性结论见第 5.9～5.11 节。

## 1. 仓库边界

完整系统由四个源码仓库组成，本仓库只覆盖后端：

| 仓库 | 职责 | 本仓库是否包含 |
| --- | --- | --- |
| `lilishop` | 后端源码（本仓库） | 是 |
| `lilishop-ui` | PC / 商家端 / 运营端 / IM Web | 否 |
| `lilishop-uniapp` | H5 / 小程序 / APP | 否 |
| `docker` | 部署、SQL、镜像与启动脚本 | 否 |

官方站点与文档：<https://pickmall.cn>、<https://docs.pickmall.cn>。

## 2. 本仓库模块

父工程 `lili-shop-parent`，版本 `${revision}` = `4.3`，Java 21。

| 模块 | 端口 | 启动类职责 | 服务对象 |
| --- | --- | --- | --- |
| `framework` | — | 领域模型、基础设施、业务服务（被 API 模块依赖） | 全端共用 |
| `manager-api` | 8887 | 平台管理 + 店铺管理 API（`/manager/**`、`/store/**`） | 平台运营方、入驻商家 |
| `buyer-api` | 8888 | 买家 API（`/buyer/**`） | C 端消费者 |
| `im-api` | 8885 | IM / WebSocket | 客服与即时通讯（支撑能力，不是使命级用户） |

店铺端没有独立 Maven 模块，商家接口与平台接口同在 `manager-api`，通过安全配置隔离。

`common-api` 已下线。前端约定：

| 端 | 公共接口前缀 |
| --- | --- |
| 买家 PC / H5 | `{buyerUrl}/buyer/common/*` |
| 商家端 | `{sellerUrl}/store/common/*` |
| 平台端 | `{managerUrl}/manager/common/*` |

禁止再使用 `/common/common/*`。

## 3. 已核实的后端技术栈

以下版本来自根 `pom.xml` 与 `framework/pom.xml`，不是 README 徽章的转述。

### 3.1 运行时与框架

| 组件 | 本仓库实际版本 | 备注 |
| --- | --- | --- |
| Java | 21 | `java.version` |
| Spring Boot | 4.0.5 | `spring-boot-starter-parent` |
| Spring WebMVC | Boot 4 starter | `spring-boot-starter-webmvc` |
| Spring Security | Boot 4 starter | 管理端 / 买家端鉴权 |
| Spring Validation / Cache / WebSocket / Quartz / Actuator / JDBC | Boot 4 starter | 校验、缓存、IM、定时任务、健康检查 |
| Spring Integration Redis | 随 Boot | 与 Redis 集成 |
| Spring Cloud | **本仓库无依赖** | README 宣传「SpringCloud 微服务商城」；访谈定性：可选演进，不得假装已经引入 |

### 3.2 数据、搜索、消息与任务

| 组件 | 本仓库实际状态 | 备注 |
| --- | --- | --- |
| MySQL | 驱动 `mysql-connector-j` 8.3.0 | 常规部署与项目技术栈以 MySQL 为准；`application.yml` 使用 Druid |
| Druid | 1.2.16 | 连接池 |
| MyBatis-Plus | 3.5.15 | `mybatis-plus-spring-boot4-starter` |
| Redis | Spring Data Redis + Lettuce | 缓存、会话；并承担消息队列 |
| Redisson | 3.52.0 | 分布式锁 / 高级 Redis |
| 商品搜索 | MySQL 实时组装 | 源码注释明确「Elasticsearch 已移除」；`pom.xml` 无 ES 依赖 |
| 异步消息 | Redis List / ZSet（`LiliMessageTemplate`） | 类名与 Topic 仍沿用 RocketMQ 术语；`pom.xml` 无 RocketMQ 依赖 |
| 定时任务 | Spring `@Scheduled` + Quartz starter | 内嵌在 `manager-api`；`pom.xml` 无 XXL-Job 依赖 |
| MQTT | Eclipse Paho 1.2.5 | IM / 直播消息通道 |

`application.yml` 仍保留 `rocketmq:` 配置段，属于命名遗留，不代表运行时依赖 RocketMQ Broker。

### 3.3 安全、文档与工具

| 组件 | 版本 |
| --- | --- |
| JWT (jjwt) | 0.13.0 |
| Jasypt | 3.0.4 |
| Lombok | 1.18.30（父 POM `lombok-version`） |
| Hutool | 5.8.41 |
| Fastjson2 | 2.0.59 |
| Springdoc OpenAPI | 3.0.3 |
| OWASP HTML Sanitizer | 20211018.2 |
| Apache POI | 5.2.3 |
| ZXing | 3.5.0 |

API 文档标题版本在 `config/application.yml` 中为 `4.2.2`，`im-api` 为 `1.0.0`，与父 POM `4.3` 不一致。

### 3.4 支付、存储、短信、物流、直播

| 能力 | 组件 |
| --- | --- |
| 微信支付 | `wechatpay-java` 0.2.17 |
| 支付宝 | `alipay-sdk-java` 4.22.32.ALL |
| 对象存储 | 阿里云 OSS 3.14.0、MinIO 8.5.17、华为 OBS 3.25.5、腾讯 COS 5.6.97 |
| 短信 | 阿里云短信 SDK 2.0.9 |
| 物流 | 快递 100 `1.0.11`、快递鸟等插件 |
| 直播 / 云能力 | 腾讯云 SDK 3.1.693（README 已完成「腾讯云-云直播」） |

### 3.5 前端与部署（跨仓，本仓库不持有源码）

| 层 | 实际约定 | 本仓库状态 |
| --- | --- | --- |
| 运营端 / 商家端 / PC / IM Web | Vue 3.5.x + Element Plus 2.14.x（`lilishop-ui`，Node >= 16） | 不在本仓 |
| 运营端 i18n | `vue-i18n` 9.x | 不在本仓 |
| H5 / 小程序 / APP | uni-app Vue3，`lilishop-uniapp` 版本 `4.5.3`，`uview-plus` | 不在本仓 |
| 常规部署 | Docker / docker-compose，中间件默认 MySQL + Redis | 脚本在 `docker` 仓 |
| All-In-One lite | 单容器体验：应用 + 前端 + H5 + MariaDB + Redis + 本地存储 | 不是生产架构 |
| All-In-One 完整打包 | 还可含 Elasticsearch、RocketMQ、XXL-Job；Kibana / Logstash / Dashboard 可选 | 体验打包，不回退本仓已移除能力 |
| 数据库脚本 | `docker` 仓 `init/mysql` | 本仓 `DB/` 仅作说明入口 |

## 4. README 与源码的已知偏差

这些偏差属于访谈确认的文档漂移短板，必须在后续迭代中逐条对齐。

| 陈述位置 | README / 对外说法 | 本仓库 / 四仓源码 |
| --- | --- | --- |
| `lilishop` 徽章 | Spring Boot 3.5.6 | 父 POM 为 Spring Boot **4.0.5** |
| `lilishop` 正文 | 「后端基于 Spring Boot 4（Java 21）」 | 与源码一致 |
| `lilishop-ui` 正文 | 「后端基于 SpringBoot3」 | 与本仓 Boot 4 不一致 |
| 核心特性 | SpringCloud 微服务商城 | **无** `spring-cloud` 依赖；当前是多模块 Spring Boot |
| 架构章节 | 不再维护架构图与技术栈表 | 对外认知继续依赖过期徽章 |
| 搜索 | 历史文档与 All-In-One 仍打 ES | 源码已移除 Elasticsearch，搜索走 MySQL |
| 消息 | 配置段 / 类名仍写 RocketMQ；All-In-One 仍打 Broker | 运行时为 Redis 队列 |
| 任务 | All-In-One 含 XXL-Job | 本仓为 `@Scheduled` + Quartz |
| 版本号 | swagger `4.2.2`、IM `1.0.0`、徽章混用 | 父工程 `4.3` |
| 数据库 | All-In-One 内置 MariaDB | 常规部署与技术栈仍以 MySQL 为准 |
| 公共 API | 旧文档可能仍写 `/common/common/*` | `common-api` 已下线 |
| 部署 | Docker 一键部署 | 脚本在独立 `docker` 仓，本仓无法单独复现 |

## 5. 现存短板（访谈确认，全部正式记入）

### 5.1 架构 / 技术栈文档与源码脱节

- README 徽章写 Spring Boot 3.5.6，正文写 Boot 4，`lilishop-ui` 仍写 SpringBoot3。
- 对外徽章、`application.yml` swagger 版本、父 POM revision 三套数字并存。
- README 明确停止维护不完整架构图，但没有用「以源码为准」的替代清单（本文档是补位）。

### 5.2 宣传 Spring Cloud，本仓未引入

- 四个 README 的核心特性都写「SpringCloud 微服务商城」。
- 本仓无 `spring-cloud` 依赖，是多模块 Spring Boot 多进程部署。
- 访谈定性：微服务为可选演进，不得假装已经是 Spring Cloud。

### 5.3 搜索 / 消息 / 任务形态分裂

- 后端：ES 已移除；消息走 Redis 队列；任务走 `@Scheduled` / Quartz。
- `docker` All-In-One 完整打包仍含 Elasticsearch、RocketMQ、XXL-Job。
- `lilishop-ui` README 按 vue3 分支描述「只依赖 MySQL + Redis」，与 All-In-One 完整形态不一致。
- 二次开发者会同时看到「不用装 ES」和「镜像里有 ES」。

### 5.4 四仓分离缺少统一跨仓契约

- 接口契约、鉴权头、错误码、静态资源域名、WebSocket / IM 地址分散在四仓。
- 主题色等后续功能必然同时改后端与至少一个前端仓；没有跨仓变更清单时，三端会交付不一致。
- 本仓库无法单独验证「全端覆盖」使命。

### 5.5 产品版本号漂移

- 父 POM `4.3`、`config/application.yml` swagger `4.2.2`、`im-api` `1.0.0`、uniapp `package.json` `4.5.3`。

### 5.6 All-In-One MariaDB 与常规 MySQL 口径易被误读

- 常规 `docker-compose` 与后端 JDBC 以 MySQL 为准。
- All-In-One 内置 MariaDB 只是 MySQL 协议兼容的单容器实现，不代表生产已切库。

### 5.7 lite 体验形态与完整中间件形态边界未写清

- lite：单容器内应用、前端、H5、MariaDB、Redis、本地存储；搜索 / 消息 / 任务用 MySQL / Redis / 本地调度兼容。
- 完整打包：另含 ES / RocketMQ / XXL-Job。
- 两者都不是生产高可用架构。

### 5.8 `common-api` 已下线，旧路径仍可能被引用

- `lilishop-ui` README 已声明不要使用 `/common/common/*`。
- 历史文档、示例和二次开发笔记仍可能指向旧路径。

### 5.9 Elasticsearch 定性（访谈）

以后端源码为准：ES 已移除，搜索走 MySQL 实时组装。All-In-One 内的 ES 只是体验打包，不代表技术栈回退，也不作为恢复 ES 的立项依据。

### 5.10 Spring Cloud 定性（访谈）

明确本仓未引入。微服务为可选演进，必须单独立项。宣传语与规范不得写成「已经是 Spring Cloud」。

### 5.11 部署形态处理原则（访谈）

规范必须写清：**lite 体验 ≠ 生产架构**。生产按访问量、稳定性、监控、备份和扩缩容要求，拆分数据库、缓存、搜索（若未来立项）、消息、任务调度、前端静态资源和后端服务。阶段 0 只写清边界，不强制收敛成单一生产参考实现。

## 6. 技术原则（后续迭代必须遵守）

1. **源码优先**：对外版本号、框架名称以本仓 `pom.xml` 与四个仓库的实际依赖为准；README 徽章随后更新，而不是反向驱动源码。
2. **仓边界清晰**：后端契约变更必须同时给出 `lilishop-ui` / `lilishop-uniapp` 的消费说明；部署与 SQL 变更必须落到 `docker` 仓。
3. **能运行即可观测**：Actuator 仅暴露 `health`；新增中间件必须写明本地 / Docker 两种启动方式，并标明 lite / 完整 / 生产分别是否需要。
4. **不回退已移除能力**：Elasticsearch 已移除，搜索与促销索引不得在未立项的情况下重新引入。
5. **Spring Cloud 不默认启用**：本仓保持多模块 Boot；若要做微服务拆分，必须单独立项。
6. **消息与任务名实相符**：对外说明以 Redis 队列 + `@Scheduled` / Quartz 为准；不得把 All-In-One 里的 RocketMQ / XXL-Job 写成后端必选依赖。
7. **数据库口径固定**：技术栈与生产默认 MySQL；MariaDB 只出现在 All-In-One 体验说明里。
8. **旧路径显式作废**：文档与示例不得再出现 `/common/common/*`。

## 7. 与其他规范的关系

- 使命与目标用户见 [mission.md](./mission.md)
- 补齐短板与产品功能的细粒度顺序见 [roadmap.md](./roadmap.md)
