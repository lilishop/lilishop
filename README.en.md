# Lilishop (PickMall) — Java Open Source B2B2C Mall System

[![GitHub Stars](https://img.shields.io/github/stars/lilishop/lilishop.svg?style=social&logo=github)](https://github.com/lilishop/lilishop)
[![Gitee Stars](https://gitee.com/beijing_hongye_huicheng/lilishop/badge/star.svg?theme=dark)](https://gitee.com/beijing_hongye_huicheng/lilishop)
[![License](https://img.shields.io/badge/license-AGPL--3.0-blue.svg)](./LICENSE)
[![Java](https://img.shields.io/badge/Java-21-orange.svg)](https://openjdk.org/projects/jdk/21/)
[![Spring Boot](https://img.shields.io/badge/Spring%20Boot-3.5.6-brightgreen.svg)](https://spring.io/projects/spring-boot)

<p align="right">
  <a href="./README.md">简体中文</a> | English
</p>

LiliShop is a Java open source ecommerce system built with Spring Boot, Spring Cloud, Vue, and Uniapp.

It supports:

- B2B2C multi-vendor marketplace
- Microservice architecture
- WeChat mini program
- Vue ecommerce frontend
- Uniapp mobile app
- Live streaming ecommerce
- Distribution & affiliate system
- Flash sale / coupon / promotion
- Docker deployment
- Private deployment
- Multi-language ecommerce platform

## Keywords

Java Mall System  
Open Source Ecommerce Platform  
Spring Boot Mall System  
Spring Cloud Ecommerce  
B2B2C Marketplace  
Multi Vendor Ecommerce Platform  
Vue Ecommerce System  
Uniapp Mall System  
Microservice Mall System  
Wechat Mini Program Mall  
Docker Ecommerce Deployment

**Lilishop** (also known as PickMall) is a production-grade **Java open source ecommerce system** designed for the **B2B2C multi-vendor marketplace** use case. It is built on **Spring Boot 3** with a modular service architecture on the backend, and **Vue.js** plus **uni-app** on the frontend — one codebase delivers a PC web mall, an H5 site, a WeChat Mini Program, and Android / iOS native apps.

The system ships with the business modules a real marketplace needs out of the box: merchant onboarding, product and SKU management, order lifecycle, after-sales, promotions (flash sale, group buy, coupon, point), affiliate distribution, live-streaming commerce, visual storefront decoration, member tiers, and a multi-language back office. Docker images and `docker-compose` configs are provided for one-command private deployment to your own servers or a Kubernetes cluster.

**Typical use cases**: enterprise commerce platform, on-premise / private-cloud mall deployment, multi-vendor marketplace, Spring Boot / Spring Cloud microservice mall reference implementation, and Java ecommerce source code for learning and secondary development.

> **This repository**: Lilishop backend services (Spring Boot 3 / Java 21, multi-module Maven project). The web frontends, mobile client and infrastructure live in the sibling repos `lilishop-ui`, `lilishop-uniapp` and `docker` — full URLs for both GitHub and Gitee mirrors are in the [Project Repositories](#project-repositories) table below.

---

## Official Website

- Website: https://pickmall.cn
- Documentation: https://docs.pickmall.cn

## Project Repositories

Lilishop is split across 4 sibling repositories, mirrored on both GitHub and Gitee:

| Repository | Purpose | GitHub | Gitee |
| :--- | :--- | :--- | :--- |
| **lilishop** | Backend services (Spring Boot / Java 21) | [github.com/lilishop/lilishop](https://github.com/lilishop/lilishop) | [gitee.com/beijing_hongye_huicheng/lilishop](https://gitee.com/beijing_hongye_huicheng/lilishop) |
| **lilishop-ui** | PC web frontends (admin / seller / buyer / IM) | [github.com/lilishop/lilishop-ui](https://github.com/lilishop/lilishop-ui) | [gitee.com/beijing_hongye_huicheng/lilishop-ui](https://gitee.com/beijing_hongye_huicheng/lilishop-ui) |
| **lilishop-uniapp** | Buyer mobile client (H5 / Mini Program / App) | [github.com/lilishop/lilishop-uniapp](https://github.com/lilishop/lilishop-uniapp) | [gitee.com/beijing_hongye_huicheng/lilishop-uniapp](https://gitee.com/beijing_hongye_huicheng/lilishop-uniapp) |
| **docker** | Infrastructure docker-compose configs | [github.com/lilishop/docker](https://github.com/lilishop/docker) | [gitee.com/beijing_hongye_huicheng/docker](https://gitee.com/beijing_hongye_huicheng/docker) |

---

## Core Features

### Full Platform Support

One codebase supports:

- PC
- H5
- WeChat Mini Program
- Android APP
- iOS APP

### Microservice Architecture

Built with:

- Java 21
- Spring Boot 4
- Spring Cloud
- Redis (cache + async message queue)
- Docker

Supports distributed deployment and high concurrency ecommerce scenarios.

### Ecommerce Features

- Multi merchant marketplace
- Product management
- Order management
- Promotion system
- Coupon system
- Affiliate distribution
- Live streaming ecommerce
- Member system
- Points system
- Visual decoration
- Multi-language support

---

## Tech Stack

### Backend

- Java 21
- Spring Boot 4
- Spring Cloud
- MyBatis Plus
- MySQL
- Redis (cache + async message queue)
- JWT
- Docker

### Frontend

- Vue.js
- Uniapp
- Vuex
- Axios
- iView
- uViewUI

---

## Online Demo

### Admin Panel

https://admin-b2b2c.pickmall.cn

Account: admin  
Password: 123456

### Store Panel

https://store-b2b2c.pickmall.cn

Account: 13011111111  
Password: 111111

### PC Mall

https://pc-b2b2c.pickmall.cn

---

## Quick Start

The fastest path on this branch is the All-In-One image, which exposes one HTTP entrypoint and proxies backend traffic through `/api/`. Full deployment guide: [docs.pickmall.cn/deply/deply.html](https://docs.pickmall.cn/deply/deply.html).

> The clone URLs below use GitHub; substitute the Gitee mirror (`gitee.com/beijing_hongye_huicheng/<repo>.git`) if it's closer to you — see the [Project Repositories](#project-repositories) table for full URLs.

```bash
# 1. Start the All-In-One image.
git clone https://github.com/lilishop/docker.git && cd docker
./install/install-lilishop.sh

# 2. Build standalone API modules from source.
git clone https://github.com/lilishop/lilishop.git && cd lilishop
mvn -pl buyer-api,manager-api,im-api -am -DskipTests package
```

Supported deployment on this branch is **standalone microservices** (`buyer-api` :8888, `manager-api` :8887 with embedded message consumer + scheduled tasks, `im-api` :8885). Async messages use a **Redis List queue** (no RocketMQ). Scheduled tasks use Spring `@Scheduled` (no XXL-Job Admin). The historical `lilishop-all` module has been removed.

---

## FAQ

### Is Lilishop / PickMall really open source?

Yes. Full source for the backend, all four web frontends, the mobile client, and the infrastructure scripts is published under the **AGPL-3.0** license on GitHub and Gitee. Commercial use that doesn't comply with AGPL-3.0 requires a separate license from the upstream vendor.

### Does it support a true microservice architecture?

Yes. Each backend deployable module (`buyer-api`, `manager-api`, `im-api`) runs as its own Spring Boot process. Message consumption and scheduled jobs run inside `manager-api`. See `README.md` (Chinese) for `java -jar` commands.

### Can I deploy it with Docker and Kubernetes?

Yes. The `docker` repo provides installer scripts. [`deploy-api.yml`](./deploy-api.yml) defines three backend Deployments/Services (ports 8888/8887/8885). Run **one replica** of `manager-api` for message consumption and scheduled tasks.

### Does it support WeChat Mini Program and native mobile apps?

Yes. The buyer-facing client lives in the `lilishop-uniapp` repo (see [Project Repositories](#project-repositories)) and is built with **uni-app**, which compiles to H5, WeChat Mini Program, Alipay Mini Program, and Android / iOS native apps from a single codebase.

### What's the tech stack at a glance?

Java 21, Spring Boot, MyBatis-Plus, MySQL, Redis, Spring Security + JWT, Docker. Product search uses MySQL (Elasticsearch removed). Async messaging uses Redis queues. Scheduled tasks use Spring `@Scheduled`. Frontend: Vue for admin / seller / buyer / IM; uni-app for mobile.

---

## License

Lilishop / PickMall is licensed under **AGPL-3.0** — see the [LICENSE](./LICENSE) file. Commercial licensing (one-time perpetual license with ongoing upgrade support) is available; contact the team via [pickmall.cn](https://pickmall.cn).
