# Lilishop(PickMall) 开源商城系统

[![GitHub Stars](https://img.shields.io/github/stars/lilishop/lilishop.svg?style=social&logo=github)](https://github.com/lilishop/lilishop)
[![Gitee Stars](https://gitee.com/beijing_hongye_huicheng/lilishop/badge/star.svg?theme=dark)](https://gitee.com/beijing_hongye_huicheng/lilishop)
[![License](https://img.shields.io/badge/license-AGPL--3.0-blue.svg)](./LICENSE)
[![Java](https://img.shields.io/badge/Java-21-orange.svg)](https://openjdk.org/projects/jdk/21/)
[![Spring Boot](https://img.shields.io/badge/Spring%20Boot-3.5.6-brightgreen.svg)](https://spring.io/projects/spring-boot)

<p align="right">
  简体中文 | <a href="./README.en.md">English</a>
</p>


LILISHOP 是基于 Spring Boot / Spring Cloud / Vue / Uniapp 开发的 Java 开源商城系统，支持 B2B2C 多商户商城、小程序商城、微服务商城、直播电商、分销返佣、秒杀活动、Docker 私有化部署。

## 核心特性

- Java商城系统
- 开源商城系统
- Spring Boot 4商城系统
- SpringCloud微服务商城
- B2B2C商城源码
- 多商户商城系统
- 小程序商城源码
- Uniapp商城系统
- Vue商城系统
- Docker商城部署
- 多语言商城

适用于：

- 企业级商城系统
- 私有化商城部署
- Java商城系统二次开发
- 开源商城源码学习
- 多商户电商平台
---

### 1. 项目简介

**Lilishop** 是一款功能完善的B2B2C多商户商城系统，采用前后端分离架构，全端代码开源。
后端基于 **Spring Boot 4**（Java 21）构建，具备高内聚、低耦合的特性，支持分布式部署。
前端覆盖PC、H5、小程序和APP，基于 **Vue** 和 **uni-app** 开发。

-   **官方网站**: <https://pickmall.cn>
-   **官方文档**: <https://docs.pickmall.cn>
-   **Gitee 仓库**: <https://gitee.com/beijing_hongye_huicheng/lilishop>
-   **GitHub 仓库**: <https://github.com/lilishop/lilishop>


---

### 2. 核心特性

-   **全端覆盖**: 一套代码库支持PC、H5、小程序、APP，降低开发和维护成本。
-   **商家入驻**: 支持多商家入驻，构建平台化电商生态。
-   **分布式架构**: 后端API服务化，支持独立部署和弹性伸缩。
-   **前后端分离**: 清晰的职责划分，便于团队协作和独立开发。
-   **容器化支持**: 提供Docker镜像和docker-compose配置，实现一键部署。
-   **功能完善**: 涵盖客户、订单、商品、促销、店铺、运营、统计等完整电商业务模块。

---

### 3. 在线演示

**注意**: 演示站手机验证码统一为 `111111`。演示环境部署于 `master` 分支。

-   **平台管理端**: <https://admin-b2b2c.pickmall.cn>
    -   账号: `admin`
    -   密码: `123456`
-   **店铺管理端**: <https://store-b2b2c.pickmall.cn>
    -   账号: `13011111111`
    -   密码: `111111`
-   **商城PC端**: <https://pc-b2b2c.pickmall.cn>
-   **微信小程序**:
    ![移动端体验二维码](https://static.pickmall.cn/images/h5-qrcode.png)

---

### 4. 快速开始

#### 环境准备与部署
详细的本地部署指南，请参考官方文档：
[**部署文档 -> 环境准备**](https://docs.pickmall.cn/deply/deply.html)

#### 数据库初始化
-   **推荐方式**: 使用项目提供的 `docker-compose` 配置，可自动完成数据库（MySQL、Redis 等）的部署与初始化。
-   **手动方式**: 如果您选择手动部署，SQL脚本位于以下地址。请确保获取与您代码版本一致的SQL文件。
    [**数据库脚本 (Gitee)**](https://gitee.com/beijing_hongye_huicheng/docker/tree/master/init/mysql)

---

### 5. 架构文档边界

本仓库首页不再维护不完整的系统架构图和技术栈表，避免与实际源码、部署形态和分支差异产生偏差。当前仓库主要说明后端源码定位、在线演示、部署入口和验证补充。

完整架构请以官方文档及四个源码仓库为准：

- 后端源码：`lilishop`
- PC / 商家端 / 运营端 / IM Web：`lilishop-ui`
- H5 / 小程序 / APP：`lilishop-uniapp`
- 部署、SQL、镜像与启动脚本：`docker`

---

### 6. 功能列表

[功能清单](https://bdwx2tfwwt.feishu.cn/sheets/FFQKscQKDhUx60to1k9cbMngnYb?from=from_copylink)

---

### 7. 开源与授权

1.  **开源协议**: 本项目遵循 `AGPL-3.0` 开源协议。
2.  **使用范围**: 仅允许用于个人学习、研究和非商业用途。
3.  **禁止行为**: 禁止将本项目的代码和资源用于任何形式的商业销售。
4.  **商业授权**: 如需商业使用，必须获得官方授权。授权为一次性永久授权，并提供持续的版本升级服务。详情请联系官网客服。
5.  **软件著作权**: 本软件受国家计算机软件著作权保护（登记号：2021SR0805085）。

---

### 8. 社区与支持

我们欢迎任何形式的交流与贡献。在提问前，请先查阅 [官方文档](https://docs.pickmall.cn/) ，并参考 [《提问的智慧》](https://github.com/ryanhanwu/How-To-Ask-Questions-The-Smart-Way/blob/master/README-zh_CN.md) 以便高效沟通。

-   **[在线客服](https://work.weixin.qq.com/kfid/kfc4d8dc24a73c15f44)**
-   **微信交流群**:
    ![微信群](https://lilishop-wechat.oss-cn-beijing.aliyuncs.com/wechat.jpg)

---

### 9. 升级计划

以下功能将陆续在后续版本中发布：✅ 已完成 · 🔜 规划中

- ✅ 商品定时上下架
- ✅ 商品分组
- ✅ 会员分组
- ✅ 商品虚拟销量
- ✅ 专用发票（发票上传）
- ✅ 置顶评论
- ✅ 客户等级
- ✅ 购物卡
- ✅ 腾讯云-云直播
- ✅ 统计：营业概况、商品概况、会员概况、积分分析、储值分析、营销概况、分销统计
- ✅ 经营报表（店铺业绩报表、商品同比环比报表、销售订单明细报表、商品销售汇总报表）
- 🔜 主题色管理
- 🔜 店铺结算汇总
- 🔜 支付方式汇总
- 🔜 结算台账
- 🔜 钱包流水
- 🔜 财务汇总
- 🔜 微信小店对接
- 🔜 抖店对接
