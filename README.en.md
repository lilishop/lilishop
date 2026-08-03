# Lilishop(Pickmall) - Java Open Source Mall System

[![GitHub Stars](https://img.shields.io/github/stars/hongyehuicheng/lilishop.svg?style=social&logo=github)](https://github.com/hongyehuicheng/lilishop)
[![Gitee Stars](https://gitee.com/beijing_hongye_huicheng/lilishop/badge/star.svg?theme=dark)](https://gitee.com/beijing_hongye_huicheng/lilishop)

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

---

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

---

## Official Website

- Website: https://pickmall.cn
- Documentation: https://docs.pickmall.cn
- GitHub: https://github.com/lilishop/lilishop
- Gitee: https://gitee.com/beijing_hongye_huicheng/lilishop

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

- Spring Boot
- Spring Cloud
- Redis
- RocketMQ
- Elasticsearch
- Docker
- ShardingSphere

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

- Java
- Spring Boot
- Spring Cloud
- MyBatis Plus
- MySQL
- Redis
- RocketMQ
- Elasticsearch
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

Please read the deployment documentation:

https://docs.pickmall.cn

### JWT secret configuration

The JWT signing secret must be injected by the deployment environment and must not be committed to the repository. It must be a Base64-encoded key containing at least 32 bytes of key material, for example:

```bash
export LILI_JWT_SECRET="$(openssl rand -base64 32)"
```

All API services must use the same `LILI_JWT_SECRET`. The service fails during startup when this variable is not configured.

Supports:

- Docker deployment
- Linux deployment
- Kubernetes deployment
- Microservice deployment

---

## FAQ

### Is PickMall open source?

Yes. PickMall is an open source Java ecommerce system.

### Does PickMall support microservices?

Yes. PickMall is built on Spring Cloud microservice architecture.

### Does PickMall support Docker?

Yes. Docker deployment is supported.

### Does PickMall support WeChat Mini Program?

Yes. It supports WeChat Mini Program and Uniapp.

---

## License

PickMall is licensed under AGPL-3.0.
