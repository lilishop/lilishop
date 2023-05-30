/*
 Navicat MySQL Data Transfer

 Source Server         : 192.168.0.116
 Source Server Type    : MySQL
 Source Server Version : 80023
 Source Host           : 192.168.0.116:3306
 Source Schema         : lilishop


 Target Server Type    : MySQL
 Target Server Version : 80023
 File Encoding         : 65001

 Date: 27/07/2021 19:42:15
*/

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for li_notice_message
-- ----------------------------
DROP TABLE IF EXISTS `li_notice_message`;
CREATE TABLE `li_notice_message` (
  `id` bigint NOT NULL,
  `create_by` varchar(255) DEFAULT NULL,
  `create_time` datetime(6) DEFAULT NULL,
  `delete_flag` bit(1) DEFAULT NULL,
  `update_by` varchar(255) DEFAULT NULL,
  `update_time` datetime(6) DEFAULT NULL,
  `notice_content` varchar(255) DEFAULT NULL,
  `notice_node` varchar(255) DEFAULT NULL,
  `notice_status` varchar(255) DEFAULT NULL,
  `notice_title` varchar(255) DEFAULT NULL,
  `variable` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of li_notice_message
-- ----------------------------
BEGIN;
INSERT INTO `li_notice_message` VALUES (1, NULL, '2021-07-14 19:22:03.000000', NULL, NULL, '2021-07-14 19:22:09.000000', '订单 #{商品名称}已经提交成功，请及时付款哦～', '订单提交成功通知', 'OPEN', '订单提交成功通知', 'goods');
INSERT INTO `li_notice_message` VALUES (2, NULL, '2021-07-14 19:22:06.000000', NULL, NULL, '2021-07-14 19:22:11.000000', '订单#{商品名称}支付成功，我们将尽快为您安排发货哦~', '订单支付成功通知', 'OPEN', '订单支付成功通知', 'goods');
INSERT INTO `li_notice_message` VALUES (3, NULL, '2021-07-15 19:10:14.000000', NULL, NULL, '2021-07-15 19:10:17.000000', '订单#{商品名称}已发货，请注意物流进度哦~', '订单发货通知\n', 'OPEN', '订单发货通知', 'goods');
INSERT INTO `li_notice_message` VALUES (4, NULL, '2021-07-15 19:12:00.000000', NULL, NULL, '2021-07-15 19:12:02.000000', '订单#{商品名称}已完成，期待您分享商品评价与购物心得哦~', '订单完成通知', 'OPEN', '订单完成通知', 'goods');
INSERT INTO `li_notice_message` VALUES (5, NULL, '2021-07-15 19:14:01.000000', NULL, NULL, '2021-07-15 19:14:04.000000', '订单#{商品名称}已取消，取消原因为 #{取消原因}，请及时关注您的订单状态哦~', '订单取消成功通知', 'OPEN', '订单取消通知', 'goods,cancel_reason');
INSERT INTO `li_notice_message` VALUES (6, NULL, '2021-07-15 19:26:27.000000', NULL, NULL, '2021-07-15 19:26:34.000000', '商品#{商品名称}还没有收到您的评价呢，期待您与我们分享哦~', '订单评价提醒', 'OPEN', '订单评价提醒', 'goods');
INSERT INTO `li_notice_message` VALUES (7, NULL, '2021-07-15 19:26:25.000000', NULL, NULL, '2021-07-15 19:26:37.000000', '恭喜，您的拼团#{商品名称}已成团，我们将尽快为您安排发货哦~', '拼团成功通知', 'OPEN', '拼团成功通知', 'goods');
INSERT INTO `li_notice_message` VALUES (8, NULL, '2021-07-15 19:31:45.000000', NULL, NULL, '2021-07-15 19:31:49.000000', '售后单 #{商品名称} 已经提交成功，需要商家审核，请耐心等待哦~', '售后提交成功通知', 'OPEN', '售后提交成功通知', 'goods');
INSERT INTO `li_notice_message` VALUES (9, NULL, '2021-07-15 19:32:44.000000', NULL, NULL, '2021-07-15 19:32:49.000000', '售后单 #{商品名称} 已通过退货审核，请尽快安排将货物退还至商家哦~', '退货审核通过通知', 'OPEN', '退货审核通过通知', 'goods');
INSERT INTO `li_notice_message` VALUES (10, NULL, '2021-07-15 19:32:46.000000', NULL, NULL, '2021-07-15 19:32:52.000000', '售后单 #{商品名称} 已通过退款审核，将在1-3个工作日内自动退款~', '退款审核通过通知', 'OPEN', '退款审核通过通知', 'goods');
INSERT INTO `li_notice_message` VALUES (11, NULL, '2021-07-15 19:36:48.000000', NULL, NULL, '2021-07-15 19:36:50.000000', '很抱歉，售后单#{商品名称}未通过商家退货审核，原因是：#{拒绝原因}', '退货审核未通过通知', 'OPEN', '退货审核未通过通知', 'goods,refuse');
INSERT INTO `li_notice_message` VALUES (12, NULL, '2021-07-15 19:36:46.000000', NULL, NULL, '2021-07-15 19:36:52.000000', '很抱歉，售后单#{商品名称}未通过商家退款审核，原因是：#{拒绝原因}', '退款审核未通过通知', 'OPEN', '退款审核未通过通知', 'goods,refuse');
INSERT INTO `li_notice_message` VALUES (13, NULL, '2021-07-15 19:39:31.000000', NULL, NULL, '2021-07-15 19:39:34.000000', '很抱歉，您的退货物品{退单第一行商品名称}被商家拒收，原因是：#{拒绝原因}', '退货物品拒收通知', 'OPEN', '退货物品拒收通知', 'goods,refuse');
INSERT INTO `li_notice_message` VALUES (14, NULL, '2021-07-15 19:39:39.000000', NULL, NULL, '2021-07-15 19:39:43.000000', '订单 #{商品名称} 商家已签收哦，请关注后续退款状态，将在1-3个工作日内自动退款~', '退货物品签收通知', 'OPEN', '退货物品签收通知', 'goods');
INSERT INTO `li_notice_message` VALUES (15, NULL, '2021-07-15 19:44:38.000000', NULL, NULL, '2021-07-15 19:44:40.000000', '售后单 #{商品名称} 已经完成，请关注及时关注售后单状态~', '售后完成通知', 'OPEN', '售后完成通知', 'goods');
INSERT INTO `li_notice_message` VALUES (16, NULL, '2021-07-15 19:47:27.000000', NULL, NULL, '2021-07-15 19:47:31.000000', '您当前到账{获得积分}积分，消费#{消费积分} 积分，积分可以抵现金哦，真的很棒棒呢~', '积分变更通知', 'OPEN', '积分变更通知', 'income_points,expenditure_points');
INSERT INTO `li_notice_message` VALUES (17, NULL, '2021-07-15 19:53:10.000000', NULL, NULL, '2021-07-15 19:53:13.000000', '您的余额账户今日收入¥#{收入金额}，支出¥#{支出金额}，如遇异常变动，请及时联系我们哦~', '余额账户变更通知', 'OPEN', '余额账户变更通知', 'income,expenditure');
INSERT INTO `li_notice_message` VALUES (18, NULL, '2021-07-15 19:57:01.000000', NULL, NULL, '2021-07-15 19:57:04.000000', '恭喜您，您的提处理成功，提现金额为¥#{收入金额}，请及时关注余额变动哦~', '余额提现成功通知', 'OPEN', '余额提现成功通知', 'income');
INSERT INTO `li_notice_message` VALUES (19, NULL, '2021-07-21 18:58:10.000000', NULL, NULL, '2021-07-21 18:58:13.000000', '恭喜您，您的提现金额为¥#{金额}的申请已经提交审核，请及时关注审核动态哦~', '提现申请提交成功通知', 'OPEN', '提现申请提交成功通知', 'price');
INSERT INTO `li_notice_message` VALUES (20, NULL, '2021-07-21 19:16:58.000000', NULL, NULL, '2021-07-21 19:17:02.000000', '很抱歉，您的提现金额为¥#{金额}的申请已被拒绝，如有问题请及时联系管理员哦~', '提现申请驳回通知', 'OPEN', '提现申请驳回通知', 'price');
INSERT INTO `li_notice_message` VALUES (21, NULL, '2021-07-26 18:59:00.000000', NULL, NULL, '2021-07-26 18:59:03.000000', '恭喜您，您的提处理成功，提现金额为¥#{收入金额}，将在1-3个工作日内到账~', '微信提现成功通知', 'OPEN', '微信提现成功通知', 'income');
COMMIT;

SET FOREIGN_KEY_CHECKS = 1;
