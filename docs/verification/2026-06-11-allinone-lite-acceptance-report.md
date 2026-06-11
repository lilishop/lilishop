# all-in-one lite 验收报告

日期：2026-06-11

分支：`allinone-lite-mysql-redis`

镜像：`lilishop-all-in-one:lite`

镜像 ID：`sha256:89b08d644f0d73f26aa6a3c49515c5d21d52e0c9072feb49f88ed11e6610e505`

验收容器：`lilishop-all-lite-uat`

访问地址：`http://127.0.0.1:18082`

## 覆盖范围说明

本报告记录的是当前已经执行并通过的 all-in-one lite 可用性验收和关键回归，不等同于完整功能测试清单。

完整测试流程仍以 `docs/plans/2026-06-11-allinone-lite-acceptance-test-plan.md` 为准，按端和功能模块独立执行。当前已执行范围如下：

| 模块 | 当前状态 | 说明 |
| --- | --- | --- |
| 镜像启动、容器健康、运行配置 | 已执行 | 单容器 healthy，MySQL/Redis/后端/nginx/H5 运行，ES/XXLJob 外部依赖关闭 |
| 基础 API、静态资源、runtime 配置 | 已执行 | PC/H5/管理端/商家端入口和核心 common API 已验证 |
| 测试账号、商品、订单数据盘点 | 已执行 | 已确认买家、商家、管理账号和可用商品数据 |
| PC 买家商品搜索、参数筛选 | 已执行 | `小米6 + 尺寸_2` 已验证 |
| PC 买家登录、加购、结算、创建订单 | 已执行 | 创建未支付订单成功 |
| H5 首页、搜索、详情、登录、加购、购物车 | 已执行 | H5 history 路由、登录绕过、商品详情、规格弹窗加购、购物车页已验证 |
| 管理端登录、商品列表、商品搜索 | 已执行 | 商品总数和 `小米6` 搜索已验证 |
| 商家端登录、商品列表、商品搜索 | 已执行 | 商品总数和 `测试1` 搜索已验证 |
| IM 本机化和基础页面 | 已执行 | IM 页面和本机 API 请求已验证 |
| MQ 本地替换触发 | 已执行基础项 | 订单创建、商品库存更新等动作触发本地 `SEND_OK`；代码层已确认主要 listener 被本地 dispatcher 覆盖 |
| 定时任务 | 已执行基础项 | `xxl.job.enabled=false` 时本地 `@Scheduled` 接管每分钟/每小时/每日任务；运行日志已确认每分钟任务执行 |
| Redis/cache | 已执行基础项 | 已确认 token、购物车、SKU stock、GOODS、setting、PV/UV 等 key |
| 商品详情、规格切换、收藏/足迹 | 未完整执行 | 仍在完整功能计划范围内 |
| 购物车数量修改、删除、全选/反选 | 未完整执行 | 仍在完整功能计划范围内 |
| 订单取消、售后、支付前置状态 | 未完整执行 | 不做真实支付，取消/售后可继续低风险验证 |
| 商家商品库存编辑回滚 | 已执行 | SKU `1377066518889627648` 库存 995 -> 996 -> 995，接口和 DB 一致 |
| 管理端系统设置修改并回滚 | 已执行 | `BASE_SETTING.siteName` `lilishop` -> `lilishop-uat-setting-check` -> `lilishop` |
| 优惠券、促销、满减、秒杀、拼团 | 未完整执行 | ES/MQ 去除后的促销链路仍需独立覆盖 |
| 会员、店铺、财务、统计、日志模块 | 未完整执行 | 需要按模块拆分继续浏览器/API 验证 |

## 启动状态

- `docker build -f docker/all-in-one/Dockerfile -t lilishop-all-in-one:lite .`：成功。
- `docker run` 单容器启动后 healthcheck：`running healthy`。
- supervisor 进程：`mariadb`、`redis`、`lilishop`、`nginx`、`h5` 全部 `RUNNING`。
- 容器内未运行 Elasticsearch、RocketMQ、XXLJob 进程。
- runtime 配置：
  - `lili.data.elasticsearch.enabled=false`
  - `spring.data.elasticsearch.repositories.enabled=false`
  - `xxl.job.enabled=false`
  - `lili.verification-code.loginBypassEnabled=true`
  - `lili.system.isDemoSite=false`
- 本轮新镜像重新构建成功，构建过程完成 buyer、seller、manager、IM、H5 和后端 jar 打包。

## 数据与配置

- `li_setting.IM_SETTING`：`{"httpUrl":"http://127.0.0.1:18082/im/"}`
- 商品数据：`li_goods` 共 268 条。
- 买家测试账号：`13011111111` 存在。
- 初始订单数：0；验收创建订单后订单数：1。
- Redis：容器内服务正常运行，`DBSIZE=38`。
- Redis key 证据包括：
  - `{CART}_1376417684140326912`
  - `{ACCESS_TOKEN_MEMBER}_...`
  - `{ACCESS_TOKEN_STORE}_...`
  - `{ACCESS_TOKEN_MANAGER}_...`
  - `{SKU_STOCK}_1376850145177501696`
  - `{SKU_STOCK}_1377066518889627648`
  - `{GOODS_SKU}_1377066518889627648`
  - `{GOODS}_1377066518478585856`
  - `{setting}::BASE_SETTING`

## 接口验收

- `/api/common/common/site`：200。
- `/api/common/common/IM`：200，返回本机 IM 地址。
- `/api/buyer/goods/goods/es?keyword=小米6&prop=尺寸_2`：200，返回小米6 SKU 数据。
- 买家链路：
  - 登录 `13011111111 / 111111`：成功。
  - 清空购物车：成功。
  - 加购 SKU `1376850145177501696`：成功。
  - 读取购物车：成功。
  - 选择地址 `1466246586711756802`：成功。
  - 读取结算页：成功。
  - 创建交易：成功。
- H5 链路：
  - `/h5/pages/passport/login`：账号密码登录通过 runtime `LOGIN_CAPTCHA_BYPASS` 直接提交，`/api/buyer/passport/member/userLogin` 返回 200。
  - `/h5/pages/product/goods?id=1376850145177501696&goodsId=1376850145118781440`：详情页返回 `三星Galaxy S21 Ultra`、价格 `7474.00`、规格、库存、店铺和推荐商品。
  - H5 商品规格弹窗加购：`POST /api/buyer/trade/carts` 返回 200。
  - `/h5/pages/tabbar/cart/cartList`：显示 `三星Galaxy S21 Ultra`、数量 1、合计 `¥7474.00`。
  - `/h5/pages/navigation/search/searchPage?keyword=三星`：返回并展示 4 条三星商品。
- 管理端可写设置：
  - 登录 `admin / 123456`：成功。
  - `GET /api/manager/setting/setting/get/BASE_SETTING`：返回 `siteName=lilishop`。
  - `PUT /api/manager/setting/setting/put/BASE_SETTING` 临时写入 `siteName=lilishop-uat-setting-check`：成功。
  - 再次 GET 确认临时值生效：成功。
  - PUT 回滚原值，DB 查询 `li_setting` 确认 `siteName=lilishop`。
- 商家端库存写回滚：
  - 登录 `13011111111 / 111111`：成功。
  - `GET /api/store/goods/goods/sku/list?pageNumber=1&pageSize=1` 选中 SKU `1377066518889627648`，商品 `达利园 桂圆莲子八宝粥 桂圆莲子`，库存 995。
  - `PUT /api/store/goods/goods/update/stocks` 写入库存 996：成功，详情接口返回 996。
  - 再次 PUT 回滚 995：成功，详情接口和 DB `li_goods_sku.quantity` 均为 995。
- 订单结果：
  - 交易号：`T2064760113138237440`
  - 订单号：`O2064760113142431744`
  - 商品：`三星Galaxy S21 Ultra`
  - 数量：1
  - 金额：7474.00
  - 状态：`UNPAID`

## 浏览器验收

- H5：`/h5/` 打开真实首页，显示分类入口与商品流；console 未出现 `[system] API getStorageSync is not yet implemented`。
- H5 history 路由说明：当前 H5 manifest 使用 history 模式，正确入口是 `/h5/pages/...`，不是 `#/pages/...`。已用 history URL 验证详情、登录、购物车和搜索页面。
- H5 详情页：`/h5/pages/product/goods?id=1376850145177501696&goodsId=1376850145118781440` 显示 `三星Galaxy S21 Ultra`、`¥7474.00`、`加入购物车`、`立即购买`。
- H5 加购：规格弹窗显示已选 `银色`、库存 999；调用弹窗加购后 `/api/buyer/trade/carts` 返回 200，购物车页显示商品和合计。
- PC：`/goodsList?keyword=小米6&prop=尺寸_2` 显示 6 条小米6商品，参数筛选可用。
- PC 买家详情页：`/goodsDetail?skuId=1376850145177501696&goodsId=1376850145118781440` 显示商品名、价格 `￥7,474.00`、规格 `银色/黑色`、库存 999。
- PC 买家购物车：通过 API 加购后 `/cart` 页面显示 `三星Galaxy S21 Ultra`，数量改为 2 后页面合计 `￥14,948.00`；API 删除后购物车清空。自动化点击 PC 详情页底部加购和购物车删除按钮时未稳定触发前端事件，已记录为后续 UI 自动化复测项。
- 管理后台：
  - `/manager/` 登录成功。
  - 新镜像复验：`/manager/login` 页面可打开，登录后 `/manager/home` 显示会员、订单、商品、促销、店铺、设置等菜单和统计卡片。
  - `/manager/manager-goods` 初始商品总数 251。
  - 搜索 `小米6` 返回 1 条，商品 ID `1409695620697223170`。
- 商家后台：
  - `/seller/` 登录成功。
  - 新镜像复验：`/seller/login?redirect=%2Fhome` 页面可打开，登录后 `/seller/home` 显示 `Hi,张三`、店铺 `家家乐`、商品/订单/财务/营销/统计/设置等菜单。
  - 新镜像复验：`/seller/goods` 商品列表可打开，显示搜索表单、商品列表、库存列和 `共 186 条`。
  - `/seller/goods` 初始商品总数 186。
  - 搜索 `测试1` 返回 2 条，包含 `测试1`。
- IM：
  - runtime `IM_URL=http://127.0.0.1:18082/im/`
  - runtime `IM_WS_URL=ws://127.0.0.1:18082/lili/webSocket`
  - `/im/?token=...` 显示 `家家乐`、`搜索好友`、`暂无聊天消息`。
  - 业务请求命中本机 `/api/store/member/user` 和 `/api/im/talk/store/list`，没有请求 `store-api.pickmall.cn`。

## 日志验收

- 定时任务：`TimedTaskJobHandler : 每分钟任务执行` 持续输出。
- MQ 替换：订单创建触发 `RocketmqSendCallback : async onSuccess SendResult=SEND_OK`。
- MQ 本地化代码覆盖：
  - `LocalRocketMqTemplate` 覆盖 `asyncSend`、`syncSend`、`sendOneWay`、`doSend`，发送后发布本地 Spring event，并回调 `SEND_OK`。
  - `LocalRocketMqDispatcher` 按 topic 分发到 `GoodsMessageListener`、`OrderMessageListener`、`MemberMessageListener`、`StoreMessageListener`、`AfterSaleMessageListener`、`NoticeMessageListener`、`NoticeSendMessageListener`、`WxChannelsGoodsSyncListener`、`TimeTriggerConsumer`。
  - 进程列表未出现 RocketMQ namesrv/broker 或 XXLJob 进程。
- XXLJob 本地化代码覆盖：
  - `xxl.job.enabled=false` 时 `LocalTimedTaskScheduler` 通过 `@Scheduled` 调用原 `TimedTaskJobHandler` 的每分钟、每小时、每日任务。
  - `XxlJobConfig` 仅在 `xxl.job.enabled=true` 时初始化。
- 未再出现：
  - `PromotionGoodsServiceImpl` 促销索引空指针。
  - `SkuFreightRender` 数组越界。
  - `读取结算页的购物车异常`。
  - `store-api.pickmall.cn` 远端 IM API。
  - `www.baidu.com` 默认 IM 地址。

## 本轮修复点

- 无 ES 模式下 `EsGoodsIndexServiceNoop` 从 MySQL SKU 构造最小商品索引，促销信息为空 map，避免购物车和商品详情链路 NPE。
- `PromotionGoodsServiceImpl#getCurrentGoodsPromotion` 对空索引兜底。
- `GoodsSkuServiceImpl#getGoodsSkuDetail` 对空索引兜底。
- `SkuFreightRender` 对空购物车、空选中项提前返回，避免无效结算数据触发数组越界。
- 登录验证码在 all-in-one runtime 可通过配置跳过，普通配置默认不跳过。
- H5 登录页读取 `VUE_APP_LOGIN_CAPTCHA_BYPASS`，all-in-one runtime 默认跳过滑块，避免 H5 登录仍强制滑块。
- IM runtime 配置改为本机地址，并让 IM 前端读取 `config.local.js`。

## 剩余风险和待补测

- 促销、优惠券、秒杀、拼团、售后、财务、统计仍未做完整模块级验收；当前只验证了它们不会阻断基础商品、购物车、订单链路。
- H5 加购在 Chrome DevTools a11y 点击同名按钮时容易点到“购物车”入口或静态文本，本轮通过页面真实组件方法和网络请求确认业务链路通过，后续可补一轮坐标/触摸级自动化。
- PC 商品详情页“加入购物车”和购物车删除按钮在自动化点击下未稳定触发，但接口和页面渲染链路通过；需要后续单独定位 PC 前端事件或自动化选择器问题。
- 本轮未做真实支付、真实短信、真实第三方登录、真实物流和上传到线上 OSS。
