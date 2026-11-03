<div align="center">
  <img src="https://lilishop-oss.oss-cn-beijing.aliyuncs.com/4c864e133c2944efad1f7282ac8a3b9e.png" width="80" alt="Lilishop Logo" />
  <h1>Lilishop(PickMall) - Java Open Source Mall System (Spring Boot 4)</h1>

  <p>
    <a href="https://github.com/lilishop/lilishop"><img src="https://img.shields.io/github/stars/lilishop/lilishop.svg?style=social&logo=github" alt="GitHub Stars" /></a>
    <a href="https://gitee.com/beijing_hongye_huicheng/lilishop"><img src="https://gitee.com/beijing_hongye_huicheng/lilishop/badge/star.svg?theme=dark" alt="Gitee Stars" /></a>
    <a href="./LICENSE"><img src="https://img.shields.io/badge/license-Apache--2.0-blue.svg" alt="License" /></a>
    <a href="https://openjdk.org/projects/jdk/21/"><img src="https://img.shields.io/badge/Java-21-orange.svg" alt="Java 21" /></a>
    <a href="https://spring.io/projects/spring-boot"><img src="https://img.shields.io/badge/Spring%20Boot-4.0.5-brightgreen.svg" alt="Spring Boot 4.0.5" /></a>
  </p>

  <p>
    <a href="https://pickmall.cn">🏠 Official Website</a> ·
    <a href="https://bdwx2tfwwt.feishu.cn/wiki/space/7675179368632617952?ccm_open_type=lark_wiki_spaceLink&open_tab_from=wiki_home">📖 Product Documentation</a> ·
    <a href="#2-online-demo">💻 Online Demo</a> ·
    <a href="https://github.com/lilishop/lilishop">💻 GitHub</a>
  </p>

  <p>
    <a href="./README.md">简体中文</a> | <b>English</b>
  </p>
</div>

> ⭐ **If you like this project, please click `Star` in the top-right corner to support us! Your Star is our greatest motivation for continuous open-source iteration!**

**LILISHOP** is an open-source e-commerce system built with Spring Boot, Vue 3, and uni-app. It supports B2B2C multi-vendor marketplaces, WeChat Mini Programs, H5, Android/iOS Apps, live-streaming e-commerce, affiliate distribution, and flash sales.

### ✨ Why Choose Lilishop
- 🟢 **True Apache-2.0 License**: Free for commercial use and private secondary development without licensing concerns.
- 🚀 **Java 21 + Spring Boot 4 Stack**: Built on `Spring Boot 4.0.5 + Java 21 + MyBatis-Plus 3.5` on the backend and `Vue 3.5 + uni-app` on the frontend.
- 🧩 **Full-Platform E-Commerce Suite**: Covers **Platform Admin, Seller Panel, PC Mall, H5, WeChat Mini Program, Android/iOS Apps, and IM Customer Service**, complete with B2B2C multi-vendor settlement, promotions, distribution, live streaming, and business reports.

---

## 1. Overview

This repository (`lilishop`) is the **Java Backend Main Repository** of Lilishop, built with **Spring Boot 4.0.5** and **Java 21**. It features a clean modular architecture separated into `framework`, `buyer-api`, `manager-api`, and `im-api`.

- **Official Website**: <https://pickmall.cn>
- **Documentation**: [Product Documentation](https://bdwx2tfwwt.feishu.cn/wiki/space/7675179368632617952?ccm_open_type=lark_wiki_spaceLink&open_tab_from=wiki_home)
- **Gitee Repository**: <https://gitee.com/beijing_hongye_huicheng>
- **GitHub Main Repository**: <https://github.com/lilishop/lilishop>

---

## 2. Online Demo

> **Note**: The verification code for the demo environment is `111111`.

| Portal | URL | Demo Credentials |
| :--- | :--- | :--- |
| **Platform Admin Panel (`manager`)** | <https://admin-b2b2c.pickmall.cn> | `admin` / `123456` |
| **Seller / Store Panel (`seller`)** | <https://store-b2b2c.pickmall.cn> | `13011111111` / `111111` |
| **PC Buyer Mall (`buyer`)** | <https://pc-b2b2c.pickmall.cn> | SMS code: `111111` |
| **Mobile H5 (`uni-app`)** | <https://m-b2b2c.pickmall.cn> | SMS code: `111111` |

| WeChat Mini Program | Mobile H5 ([Visit Directly](https://m-b2b2c.pickmall.cn)) |
| :---: | :---: |
| <img src="https://static.pickmall.cn/images/h5-qrcode.png" width="180" alt="WeChat Mini Program QR Code" /> | <img src="https://lilishop-wechat.oss-cn-beijing.aliyuncs.com/h5-qrcode.png" width="180" alt="Mobile H5 QR Code" /> |
| Scan with WeChat | Scan or visit <https://m-b2b2c.pickmall.cn> |

### Mobile UI Preview

| Home | Product List | Product Detail | Checkout | User Center |
| :---: | :---: | :---: | :---: | :---: |
| <img src="https://lilishop-wechat.oss-cn-beijing.aliyuncs.com/image.png" width="160" alt="Home" /> | <img src="https://lilishop-wechat.oss-cn-beijing.aliyuncs.com/image3.png" width="160" alt="Product List" /> | <img src="https://lilishop-wechat.oss-cn-beijing.aliyuncs.com/image4.png" width="160" alt="Product Detail" /> | <img src="https://lilishop-wechat.oss-cn-beijing.aliyuncs.com/image5.png" width="160" alt="Checkout" /> | <img src="https://lilishop-wechat.oss-cn-beijing.aliyuncs.com/image2.png" width="160" alt="User Center" /> |

---

## 3. Core Features & Tech Stack

### Core Features
- **Full-Platform Backend**: Powers PC, H5, WeChat Mini Program, Android/iOS Apps, Seller Portal, and Admin Panel.
- **Multi-Vendor B2B2C**: Supports both self-operated and third-party merchants with store auditing, split-order shipping, promotions, and settlement.
- **Decoupled API Services**: Separated into `buyer-api`, `manager-api`, and `im-api` sharing the `framework` domain core for flexible deployment and scaling.
- **Rich Marketing & Order Types**: Flash sales, group buying, coupons, points mall, bargain, two-tier distribution, gift cards, virtual/card-key goods, and appointment services.
- **Multi-Cloud & Payment Ready**: Pluggable object storage (`Aliyun OSS`, `Tencent COS`, `Huawei OBS`, `MinIO`) and integrated `WeChatPay V3` & `Alipay` SDKs.

### Backend Tech Stack
| Category | Technology | Description |
| :--- | :--- | :--- |
| Runtime | `Java 21` | Modern LTS Java runtime |
| Core Framework | `Spring Boot 4.0.5` | WebMVC, AOP, transaction & auto-configuration core |
| Security & Auth | `Spring Security` + `JWT (jjwt 0.13)` | Stateless authentication & role-based access control |
| ORM & Database | `MyBatis-Plus 3.5.15` + `Druid` + `MySQL 8.x` | Data persistence, pagination & connection pooling |
| Cache & Coordination | `Redis` + `Redisson 3.52` | Distributed cache, locks, rate limiting & delayed queues |
| Job Scheduling | `Quartz` | Scheduled tasks for product listing, promotions & reports |
| Messaging & Push | `WebSocket` + `Eclipse Paho MQTT` | Real-time IM chat and notification push |
| API Documentation | `SpringDoc OpenAPI 3 (3.0.3)` | Automated OpenAPI 3 documentation |
| Object Storage | `Aliyun OSS` / `Tencent COS` / `Huawei OBS` / `MinIO` | Pluggable multi-cloud file storage |
| Payments & SDKs | `WeChatPay V3` / `Alipay SDK` / `Kuaidi100` / `Aliyun SMS` | Payment checkout, refunds, logistics tracking & SMS |

---

## 4. Directory Structure

```text
lilishop
├── framework/                # [Core Domain & Infrastructure Module]
│   └── src/main/java/cn/lili/
│       ├── cache/            # Redis / Redisson cache & distributed lock helpers
│       ├── common/           # Exceptions, unified response, security, encryption & utils
│       ├── config/           # Global configurations (thread pools, WebMVC, serialization)
│       ├── mybatis/          # MyBatis-Plus base entities, pagination & audit handlers
│       ├── trigger/          # Delayed task & event triggers
│       └── modules/          # Core e-commerce domain modules
│           ├── goods/        # Products, categories, brands, specs, reviews & groups
│           ├── order/        # Cart, checkout, order fulfillment, split shipping & after-sales
│           ├── member/       # Members, levels, points, addresses & favorites
│           ├── store/        # Merchant onboarding, store settings, freight & settlement
│           ├── promotion/    # Flash sale, group buy, coupons, points mall, bargain & live
│           ├── distribution/ # Two-tier affiliate distribution & commission settlement
│           ├── appointment/  # Appointment & service products
│           ├── payment/      # WeChatPay V3, Alipay & wallet cashier abstraction
│           ├── wallet/       # Member wallet, gift cards, withdrawals & transactions
│           ├── finance/      # Financial reconciliation & invoices
│           ├── statistics/   # Multi-dimensional analytics & business reports
│           ├── page/         # Visual floor decoration for PC & mobile storefronts
│           ├── im/           # Customer service chat sessions & messages
│           ├── permission/   # Admin users, departments, roles & menu permissions
│           └── system/       # System settings, regions, scheduled jobs & cloud plugins
├── buyer-api/                # [Buyer API Service] REST APIs for PC / H5 / Mini Program / App
├── manager-api/              # [Admin & Seller API Service] REST APIs for Admin & Seller portals
├── im-api/                   # [IM API Service] WebSocket & REST APIs for customer service
├── DB/                       # Database initialization SQL scripts
└── update-sql/               # Version upgrade SQL scripts
```

---

## 5. Related Repositories

| Repository | Description | Links |
| :--- | :--- | :--- |
| **`lilishop`** | Backend Services (Spring Boot 4 / Java 21) | [Gitee](https://gitee.com/beijing_hongye_huicheng/lilishop) · [GitHub](https://github.com/lilishop/lilishop) |
| **`lilishop-ui`** | PC Mall / Seller / Admin / IM Web Frontend | [Gitee](https://gitee.com/beijing_hongye_huicheng/lilishop-ui) |
| **`lilishop-uniapp`** | Mobile Buyer Client (H5 / Mini Program / App) | [Gitee](https://gitee.com/beijing_hongye_huicheng/lilishop-uniapp) |
| **`docker`** | Docker Compose, Images & SQL Init Scripts | [Gitee](https://gitee.com/beijing_hongye_huicheng/docker) · [MySQL Scripts](https://gitee.com/beijing_hongye_huicheng/docker/tree/master/init/mysql) |

---

## 6. License

Lilishop is licensed under the [Apache-2.0](./LICENSE) License. Copyright © 2020-2026 **Beijing Hongye Huicheng Technology Co., Ltd.**
