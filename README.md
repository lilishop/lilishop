# Lilishop B2B2C 商城系统

[![GitHub Stars](https://img.shields.io/github/stars/hongyehuicheng/lilishop.svg?style=social&logo=github)](https://github.com/hongyehuicheng/lilishop)
[![Gitee Stars](https://gitee.com/beijing_hongye_huicheng/lilishop/badge/star.svg?theme=dark)](https://gitee.com/beijing_hongye_huicheng/lilishop)
[![License](https://img.shields.io/badge/license-AGPL--3.0-blue.svg)](https://www.gnu.org/licenses/agpl-3.0.html)
[![Spring Boot](https://img.shields.io/badge/Spring%20Boot-2.7.18-brightgreen)](https://spring.io/projects/spring-boot)
[![Vue.js](https://img.shields.io/badge/Vue.js-2.x-green)](https://vuejs.org/)
[![uni-app](https://img.shields.io/badge/uni--app-3.x-green)](https://uniapp.dcloud.io/)

---

### 1. 项目简介

**Lilishop** 是一款功能完善的B2B2C多商户商城系统，采用前后端分离架构，全端代码开源。后端基于 **SpringBoot** 构建，具备高内聚、低耦合的特性，支持分布式部署。前端覆盖PC、H5、小程序和APP，基于 **Vue** 和 **uni-app** 开发。

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
-   **功能完善**: 涵盖会员、订单、商品、促销、店铺、运营、统计等完整电商业务模块。

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
-   **移动端 (H5/小程序/APP)**:
    ![移动端体验二维码](https://static.pickmall.cn/images/h5-qrcode.png)

---

### 4. 快速开始

#### 环境准备与部署
详细的本地部署指南，请参考官方文档：
[**部署文档 -> 环境准备**](https://docs.pickmall.cn/deply/deply.html)

#### 数据库初始化
-   **推荐方式**: 使用项目提供的 `docker-compose` 配置，可自动完成数据库（MySQL, Redis, Elasticsearch等）的部署与初始化。
-   **手动方式**: 如果您选择手动部署，SQL脚本位于以下地址。请确保获取与您代码版本一致的SQL文件。
    [**数据库脚本 (Gitee)**](https://gitee.com/beijing_hongye_huicheng/docker/tree/master/init/mysql)

---

### 5. 技术架构

#### 5.1 架构图
![系统架构图](https://lili-system.oss-cn-beijing.aliyuncs.com/docs/%E6%9E%B6%E6%9E%84.png)

#### 5.2 后端技术栈

| 技术            | 选型            | 备注/用途  |
| :-------------- | :-------------- | :--------- |
| 核心框架        | Spring Boot     | 简化应用开发 |
| ORM框架         | Mybatis-Plus    | 数据持久化  |
| 数据库          | MySQL           | 关系型数据存储 |
| 消息队列        | RocketMQ        | 异步任务与解耦 |
| 缓存            | Redis, MongoDB  | 数据缓存与存储 |
| 搜索引擎        | Elasticsearch   | 商品搜索   |
| 安全框架        | Spring Security | 认证与授权  |
| 分库分表        | ShardingSphere  | 数据水平扩展 |
| 定时任务        | XXL-Job         | 分布式任务调度 |
| 认证方案        | JWT             | Token  |

#### 5.3 前端技术栈

**管理端 (平台/商家)**

| 技术       | 选型       | 备注/用途  |
 | :--------- | :--------- | :--------- |
| JS框架     | Vue.js     | 核心框架   |
| UI库       | iView      | 界面组件   |
| 状态管理   | Vuex       | 全局状态管理 |
| 路由       | Vue Router | 页面路由   |
| HTTP客户端 | axios      | API请求    |

**移动端 (H5/小程序/APP)**

| 技术      | 选型    | 备注/用途      |
 | :-------- | :------ | :------------- |
| 核心框架  | uni-app | 跨平台开发框架 |
| UI库      | uViewUI | 丰富的组件库   |
| CSS预处理 | SCSS    | 样式开发       |

### 功能列表

### 6. 功能清单

#### 6.1 平台管理端

| 模块 | 主要功能                                                               |
| :--- | :--------------------------------------------------------------------- |
| 首页 | 数据看板、待办事项                                                     |
| 会员 | 会员管理、会员评价、积分管理、资金流水、充值管理                       |
| 订单 | 商品订单、虚拟订单、售后处理、订单投诉、收款与退款流水                 |
| 商品 | 商品管理、商品审核、分类、品牌、规格、计量单位管理                     |
| 促销 | 优惠券、秒杀、砍价、拼团、积分商品等营销活动                           |
| 店铺 | 店铺管理、入驻审核、结算管理、店铺对账                                 |
| 运营 | 页面装修、分销管理、文章管理、意见反馈、站内信、短信配置               |
| 统计 | 会员、订单、流量、商品销量等多维度统计                                 |
| 设置 | 权限、角色、部门、管理员、系统参数、OSS、支付、物流、敏感词等基础配置 |

#### 6.2 商家端

| 模块 | 主要功能                                                     |
| :--- | :----------------------------------------------------------- |
| 首页 | 店铺看板、待办事项、公告                                     |
| 商品 | 商品发布、商品管理、运费模板、店铺内分类                     |
| 订单 | 订单处理、评价管理、投诉处理、退款/退货申请                  |
| 财务 | 店铺对账、结算管理、发票管理                                 |
| 促销 | 优惠券、满减、秒杀、拼团、分销商品管理                       |
| 统计 | 订单统计、流量分析、商品销量排行                             |
| 设置 | 物流配送、自提点、店铺信息、PC/移动端装修、店员与权限管理    |

---

### 7. 界面展示

#### 移动端
<img src="https://static.pickmall.cn/images/other/app.gif" alt="移动端功能展示" width="300"/>

#### 平台管理端
![管理端功能展示](https://static.pickmall.cn/images/other/manager.gif)

---

### 8. 开源与授权

1.  **开源协议**: 本项目遵循 `AGPL-3.0` 开源协议。
2.  **使用范围**: 仅允许用于个人学习、研究和非商业用途。
3.  **禁止行为**: 禁止将本项目的代码和资源用于任何形式的商业销售。
4.  **商业授权**: 如需商业使用，必须获得官方授权。授权为一次性永久授权，并提供持续的版本升级服务。详情请联系官网客服。
5.  **软件著作权**: 本软件受国家计算机软件著作权保护（登记号：2021SR0805085）。

---

### 9. 社区与支持

我们欢迎任何形式的交流与贡献。在提问前，请先查阅 [官方文档](https://docs.pickmall.cn/) ，并参考 [《提问的智慧》](https://github.com/ryanhanwu/How-To-Ask-Questions-The-Smart-Way/blob/master/README-zh_CN.md) 以便高效沟通。

-   **[在线客服](https://work.weixin.qq.com/kfid/kfc4d8dc24a73c15f44)**
-   **微信交流1群（已满）**
-   **微信交流2群**:
    ![微信群](https://lilishop-wechat.oss-cn-beijing.aliyuncs.com/wechat.jpg)

-   **社区贡献内容**:
    -   清晨敲代码的分析: <https://blog.csdn.net/vaevaevae233/category_12103567.html>
    -   DeepWiki: <https://deepwiki.com/lilishop/lilishop>