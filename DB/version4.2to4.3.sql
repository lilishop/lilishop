/*
 Navicat Premium Data Transfer

 Source Server         : 192.168.0.116
 Source Server Type    : MySQL
 Source Server Version : 80023
 Source Host           : 192.168.0.116:3306
 Source Schema         : lilishop

 Target Server Type    : MySQL
 Target Server Version : 80023
 File Encoding         : 65001

 Date: 27/07/2021 17:04:46
*/

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;
/** 添加结算单积分、砍价结算价信息**/
ALTER TABLE li_bill ADD point_settlement_price double DEFAULT 0.00 COMMENT '积分商品结算金额 ';
ALTER TABLE li_bill ADD kanjia_settlement_price double DEFAULT 0.00 COMMENT '砍价商品结算金额';
ALTER TABLE li_store_flow ADD point_settlement_price double DEFAULT 0.00 COMMENT '积分商品结算金额';
ALTER TABLE li_store_flow ADD kanjia_settlement_price double DEFAULT 0.00 COMMENT '砍价商品结算金额';
ALTER TABLE li_order ADD order_promotion_type varchar(255) DEFAULT null COMMENT '订单促销类型';

/** 热词管理*/
UPDATE `lilishop`.`li_menu` SET `create_by` = 'admin', `create_time` = '2021-03-03 09:54:51', `delete_flag` = b'0', `update_by` = 'admin', `update_time` = '2021-07-23 19:07:45', `description` = 'null', `front_route` = 'page/article-manage/hotWords', `icon` = 'md-aperture', `level` = 2, `name` = 'hotKeyWord', `parent_id` = '1367050250249830400', `path` = 'hotKeyWord', `sort_order` = 0.00, `title` = '搜索热词', `front_component` = 'null' WHERE `id` = 1367050829697122304;

-- ----------------------------
-- Table structure for li_menu
-- ----------------------------
DROP TABLE IF EXISTS `li_menu`;
CREATE TABLE `li_menu` (
  `id` bigint NOT NULL COMMENT 'ID',
  `create_by` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '创建者',
  `create_time` datetime DEFAULT NULL COMMENT '创建时间',
  `delete_flag` bit(1) DEFAULT NULL COMMENT '删除标志 true/false 删除/未删除',
  `update_by` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '更新者',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  `description` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '说明备注',
  `front_route` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '前端路由',
  `icon` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '图标',
  `level` int DEFAULT NULL COMMENT '层级',
  `name` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '菜单/权限名称',
  `parent_id` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '父id',
  `path` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '赋权API地址,正则表达式',
  `sort_order` decimal(10,2) DEFAULT NULL COMMENT '排序值',
  `title` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '菜单标题',
  `front_component` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '文件地址',
  `permission` varchar(255) DEFAULT NULL COMMENT '权限url',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of li_menu
-- ----------------------------
BEGIN;
INSERT INTO `li_menu` VALUES (1348810750596767744, 'admin', '2021-01-12 09:55:17', b'0', 'admin', '2021-01-15 09:42:50', 'null', '1', 'ios-american-football', 0, 'settings', '0', '1', 2.00, '设置', 'null', NULL);
INSERT INTO `li_menu` VALUES (1348810864748945408, 'admin', '2021-01-12 09:55:45', b'0', 'admin', '2021-03-15 20:57:12', 'null', 'null', 'ios-american-football', 0, 'log', '0', 'null', 3.00, '日志', 'null', NULL);
INSERT INTO `li_menu` VALUES (1349237129847005184, 'admin', '2021-01-13 14:09:34', b'0', 'admin', '2021-01-15 09:43:16', 'null', 'Main', 'ios-american-football', 1, 'sys', '1348810750596767744', '/sys', 1.00, '系统设置', 'null', NULL);
INSERT INTO `li_menu` VALUES (1349237207378714624, 'admin', '2021-01-13 14:09:53', b'0', 'admin', '2021-07-27 16:07:49', 'null', 'Main', 'ios-american-football', 1, 'member', '1348810750596767744', '/member', 0.00, '用户管理', 'null', '/manager/user*,/manager/department*,/manager/role*,/manager/menu*');
INSERT INTO `li_menu` VALUES (1349237928434098176, 'admin', '2021-01-13 14:13:03', b'0', 'admin', '2021-07-27 16:09:11', 'null', 'Main', 'ios-american-football', 1, 'log', '1348810864748945408', '/log', 1.00, '系统监控', 'null', '/manager/log*');
INSERT INTO `li_menu` VALUES (1349246048900243456, 'admin', '2021-01-13 14:45:00', b'0', 'admin', '2021-07-27 16:07:57', 'null', 'sys/setting-manage/settingManage', 'ios-american-football', 2, 'setting', '1349237129847005184', 'setting', 1.00, '系统设置', 'null', '/manager/system/setting/get*,/manager/system/setting/put*');
INSERT INTO `li_menu` VALUES (1349246347597602816, 'admin', '2021-01-13 14:46:12', b'0', 'admin', '2021-07-27 16:08:03', 'null', 'sys/oss-manage/ossManage', 'ios-american-football', 2, 'oss-manage', '1349237129847005184', 'oss-manage', 3.00, 'OSS资源', '', '/manager/file*');
INSERT INTO `li_menu` VALUES (1349246468775239680, 'admin', '2021-01-13 14:46:41', b'0', 'admin', '2021-07-27 16:08:14', 'null', 'region/index', 'ios-american-football', 2, 'region', '1349237129847005184', 'region', 4.00, '行政地区', 'null', '/manager/region*');
INSERT INTO `li_menu` VALUES (1349246671158796288, 'admin', '2021-01-13 14:47:29', b'0', 'admin', '2021-07-27 16:08:09', 'null', 'logistics/index', 'ios-american-football', 2, 'logistics', '1349237129847005184', 'logistics', 5.00, '物流公司', 'null', '/manager/logistics*');
INSERT INTO `li_menu` VALUES (1349246896661356544, 'admin', '2021-01-13 14:48:23', b'0', 'admin', '2021-07-27 16:08:23', 'null', 'sys/setting-manage/settingManage', 'ios-american-football', 2, 'authLogin', '1349237129847005184', 'authLogin', 6.00, '信任登录', 'null', '/manager/system/setting/get*,/manager/system/setting/put*');
INSERT INTO `li_menu` VALUES (1349247081504333824, 'admin', '2021-01-13 14:49:07', b'0', 'admin', '2021-07-27 16:08:45', 'null', 'sys/setting-manage/settingManage', 'ios-american-football', 2, 'pay', '1349237129847005184', 'pay', 7.00, '支付设置', 'null', '/manager/system/setting/get*,/manager/system/setting/put*,/manager/system/setting*');
INSERT INTO `li_menu` VALUES (1349247640584085504, 'admin', '2021-01-13 14:51:20', b'0', 'admin', '2021-07-27 16:08:56', 'null', 'sensitiveWords/index', 'ios-american-football', 2, 'sensitiveWords', '1349237129847005184', 'sensitiveWords', 8.00, '敏感词', 'null', '/manager/sensitiveWords*');
INSERT INTO `li_menu` VALUES (1349254815809298432, 'admin', '2021-01-13 15:19:51', b'0', 'admin', '2021-01-15 11:15:40', 'null', 'sys/user-manage/userManage', 'ios-american-football', 2, 'user-manage', '1349237207378714624', 'user-manage', 1.00, '用户管理', 'null', NULL);
INSERT INTO `li_menu` VALUES (1349255214977015808, 'admin', '2021-01-13 15:21:26', b'0', 'admin', '2021-01-15 11:16:21', 'null', 'sys/department-manage/departmentManage', 'ios-american-football', 2, 'department-manage', '1349237207378714624', 'department-manage', 3.00, '部门管理', 'null', NULL);
INSERT INTO `li_menu` VALUES (1349255404425338880, 'admin', '2021-01-13 15:22:11', b'0', 'admin', '2021-02-24 09:22:21', 'null', 'sys/role-manage/roleManage', 'ios-american-football', 2, 'role-manage', '1349237207378714624', 'role-manage', 4.00, '角色权限', 'null', NULL);
INSERT INTO `li_menu` VALUES (1349256082979840000, 'admin', '2021-01-13 15:24:53', b'0', 'admin', '2021-01-15 11:18:14', 'null', 'sys/log-manage/logManage', 'ios-american-football', 2, 'log-manage', '1349237928434098176', 'log-manage', 2.00, '日志管理', 'null', NULL);
INSERT INTO `li_menu` VALUES (1357584224760102912, 'admin', '2021-02-05 06:57:57', b'0', 'admin', '2021-07-27 16:09:02', 'null', 'sys/app-version/appVersion', 'ios-american-football', 2, 'appVersion', '1349237129847005184', 'appVersion', 9.00, 'APP版本', 'null', '/manager/systems/app*');
INSERT INTO `li_menu` VALUES (1357873097859923969, 'admin', '2021-02-24 09:53:02', b'0', 'admin', '2021-02-24 09:53:12', NULL, 'sys/menu-manage/menuManage', 'ios-american-football', 2, 'menuManage', '1349237207378714624', 'menu-manage', 2.00, '菜单管理', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367038467288072192, 'admin', '2021-03-03 09:05:44', b'0', 'admin', '2021-03-03 09:09:27', 'null', 'null', 'ios-person-add', 0, 'member', '0', 'null', 0.00, '会员', 'null', NULL);
INSERT INTO `li_menu` VALUES (1367039534616805376, 'admin', '2021-03-03 09:09:58', b'0', 'admin', '2021-05-18 10:51:12', 'null', 'null', 'md-reorder', 0, 'order', '0', 'null', 0.00, '订单', 'null', NULL);
INSERT INTO `li_menu` VALUES (1367039950368800768, 'admin', '2021-03-03 09:11:37', b'0', NULL, NULL, NULL, NULL, 'ios-share', 0, 'goods', '0', NULL, 0.20, '商品', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367040067201138688, 'admin', '2021-03-03 09:12:05', b'0', NULL, NULL, NULL, NULL, 'ios-hammer', 0, 'promotion', '0', NULL, 0.30, '促销', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367040599596728320, 'admin', '2021-03-03 09:14:12', b'0', 'admin', '2021-03-03 09:52:13', 'null', 'null', 'ios-color-palette', 0, 'operate', '0', 'null', 0.50, '运营', 'null', NULL);
INSERT INTO `li_menu` VALUES (1367040819248234496, 'admin', '2021-03-03 09:15:04', b'0', NULL, NULL, NULL, NULL, 'ios-stats', 0, 'statistics', '0', NULL, 0.70, '统计', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367041332861730816, 'admin', '2021-03-03 09:17:07', b'0', NULL, NULL, NULL, 'Main', 'ios-aperture', 1, '/', '1367038467288072192', '/', 0.00, '会员管理', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367041461194850304, 'admin', '2021-03-03 09:17:37', b'0', 'admin', '2021-07-27 16:02:17', NULL, 'member/list/index', 'ios-aperture', 2, 'memberList', '1367041332861730816', 'memberList', 0.00, '会员列表', NULL, '/manager/member*,/manager/orders*,/manager/wallet/log*,/manager/receipt*');
INSERT INTO `li_menu` VALUES (1367041575619657728, 'admin', '2021-03-03 09:18:05', b'0', 'admin', '2021-07-27 15:59:50', NULL, 'member/list/memberRecycle', 'ios-aperture', 2, 'memberRecycle', '1367041332861730816', 'memberRecycle', 1.00, '回收站', NULL, '/manager/member*');
INSERT INTO `li_menu` VALUES (1367042490443497472, 'admin', '2021-03-03 09:21:43', b'0', NULL, NULL, NULL, 'Main', 'ios-aperture', 1, '/', '1367038467288072192', '/', 1.00, '预存款', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367042664410644480, 'admin', '2021-03-03 09:22:24', b'0', 'admin', '2021-07-27 16:02:44', 'null', 'member/advance/walletLog', 'ios-aperture', 2, 'walletLog', '1367042490443497472', 'walletLog', 0.00, '会员资金', 'null', '/manager/wallet/log*');
INSERT INTO `li_menu` VALUES (1367042804944994304, 'admin', '2021-03-03 09:22:58', b'0', 'admin', '2021-07-27 16:02:48', NULL, 'member/advance/recharge', 'ios-alert', 2, 'recharge', '1367042490443497472', 'recharge', 1.00, '充值记录', NULL, '/manager/recharge*');
INSERT INTO `li_menu` VALUES (1367042804944994305, 'admin', '2021-03-03 09:22:58', b'0', 'admin', '2021-07-27 16:02:52', NULL, 'member/advance/withdrawApply', 'ios-alert', 2, 'withdrawApply', '1367042490443497472', 'withdrawApply', 1.00, '提现申请', NULL, '/manager/members/withdraw-apply*');
INSERT INTO `li_menu` VALUES (1367042917113266176, 'admin', '2021-03-03 09:23:25', b'0', NULL, NULL, NULL, 'Main', 'ios-aperture', 1, 'commont', '1367038467288072192', '/', 0.00, '评价', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367043020976816128, 'admin', '2021-03-03 09:23:49', b'0', 'admin', '2021-07-27 16:02:35', NULL, 'goods/goods-review/index', 'md-aperture', 2, 'goodsReview', '1367042917113266176', 'goodsReview', 0.00, '会员评价', NULL, '/manager/memberEvaluation*');
INSERT INTO `li_menu` VALUES (1367043443917848576, 'admin', '2021-03-03 09:25:30', b'0', 'admin', '2021-07-27 16:03:00', NULL, 'Main', 'md-aperture', 1, 'order', '1367039534616805376', '/', 0.00, '订单', NULL, '/manager/orders*');
INSERT INTO `li_menu` VALUES (1367043505771249664, 'admin', '2021-03-03 09:25:45', b'0', NULL, NULL, NULL, 'Main', 'md-aperture', 1, 'aftersale', '1367039534616805376', '/', 0.00, '售后', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367043642379730944, 'admin', '2021-03-03 09:26:17', b'0', NULL, NULL, NULL, 'order/order/orderList', 'ios-aperture', 2, 'orderList', '1367043443917848576', 'orderList', 0.00, '商品订单', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367043791105556480, 'admin', '2021-03-03 09:26:53', b'0', NULL, NULL, NULL, 'order/order/fictitiousOrderList', 'ios-aperture', 2, 'fictitiousOrderList', '1367043443917848576', 'fictitiousOrderList', 1.00, '虚拟订单', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367043980407078912, 'admin', '2021-03-03 09:27:38', b'0', 'admin', '2021-07-27 16:03:43', NULL, 'order/after-order/afterSaleOrder', 'md-alert', 2, 'afterSaleOrder', '1367043505771249664', 'afterSaleOrder', 0.00, '售后管理', NULL, '/manager/afterSale*');
INSERT INTO `li_menu` VALUES (1367044121163726848, 'admin', '2021-03-03 09:28:12', b'0', 'admin', '2021-07-27 16:03:48', NULL, 'order/after-order/orderComplaint', 'md-alert', 2, 'orderComplaint', '1367043505771249664', 'orderComplaint', 2.00, '交易投诉', NULL, '/manager/complain*');
INSERT INTO `li_menu` VALUES (1367044247978508288, 'admin', '2021-03-03 09:28:42', b'0', 'admin', '2021-07-27 16:03:52', NULL, 'order/after-order/afterSale', 'md-aperture', 2, 'afterSaleReason', '1367043505771249664', 'afterSaleReason', 3.00, '售后原因', NULL, '/manager/afterSaleReason*');
INSERT INTO `li_menu` VALUES (1367044376391319552, 'admin', '2021-03-03 09:29:12', b'0', 'admin', '2021-07-27 16:04:08', NULL, 'Main', 'md-aperture', 1, 'goodsManager', '1367039950368800768', '/', 0.00, '商品管理', NULL, '/manager/goods*');
INSERT INTO `li_menu` VALUES (1367044657296441344, 'admin', '2021-03-03 09:30:19', b'0', NULL, NULL, NULL, 'Main', 'ios-aperture', 1, 'association', '1367039950368800768', '/', 1.00, '关联管理', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367045529720061952, 'admin', '2021-03-03 09:33:47', b'0', 'admin', '2021-07-27 15:38:46', NULL, 'goods/goods-info/goods', 'md-aperture', 2, 'managerGoods', '1367044376391319552', 'managerGoods', 0.00, '平台商品', NULL, 'null');
INSERT INTO `li_menu` VALUES (1367045630710513664, 'admin', '2021-03-03 09:34:11', b'0', 'admin', '2021-07-27 15:38:56', NULL, 'goods/goods-info/goodsApply', 'ios-alert', 2, 'applyGoods', '1367044376391319552', 'applyGoods', 1.00, '商品审核', NULL, 'null');
INSERT INTO `li_menu` VALUES (1367045794284175360, 'admin', '2021-03-03 09:34:50', b'0', 'admin', '2021-07-27 16:04:18', NULL, 'goods/goods-manage/category', 'md-alert', 2, 'goodsCategory', '1367044657296441344', 'goodsCategory', 0.00, '商品分类', NULL, '/manager/goods/category*,/manager/goods/brand*,/manager/goods/spec*,/manager/goods/parameters*');
INSERT INTO `li_menu` VALUES (1367045921434501120, 'admin', '2021-03-03 09:35:21', b'0', 'admin', '2021-07-27 16:04:23', NULL, 'goods/goods-manage/brand', 'md-alert', 2, 'goodsBrand', '1367044657296441344', 'goodsBrand', 1.00, '品牌列表', NULL, '/manager/goods/brand*');
INSERT INTO `li_menu` VALUES (1367046068369358848, 'admin', '2021-03-03 09:35:56', b'0', 'admin', '2021-07-27 16:04:27', NULL, 'goods/goods-manage/spec', 'md-aperture', 2, 'goodsSpec', '1367044657296441344', 'goodsSpec', 2.00, '规格列表', NULL, '/manager/goods/spec*');
INSERT INTO `li_menu` VALUES (1367046266214678528, 'admin', '2021-03-03 09:36:43', b'0', 'admin', '2021-07-27 16:04:32', NULL, 'goods-unit/index', 'md-alert', 2, 'goodsUnit', '1367044657296441344', 'goodsUnit', 4.00, '计量单位', NULL, '/manager/goods/goodsUnit*');
INSERT INTO `li_menu` VALUES (1367048084701315072, 'admin', '2021-03-03 09:43:57', b'0', 'admin', '2021-03-03 09:52:17', 'null', 'null', 'ios-pricetags', 0, 'shop', '0', 'null', 0.40, '店铺', 'null', NULL);
INSERT INTO `li_menu` VALUES (1367048684339986432, 'admin', '2021-03-03 09:46:20', b'0', NULL, NULL, NULL, 'Main', 'md-aperture', 1, 'shopManager', '1367048084701315072', '/', 0.00, '店铺管理', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367048754229673984, 'admin', '2021-03-03 09:46:36', b'0', NULL, NULL, NULL, 'Main', 'md-aperture', 1, 'bill', '1367048084701315072', '/', 0.00, ' 店铺结算', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367048832210173952, 'admin', '2021-03-03 09:46:55', b'0', 'admin', '2021-07-27 16:05:30', NULL, 'seller/shop/shopList', 'md-aperture', 2, 'shopList', '1367048684339986432', 'shopList', 0.00, '店铺列表', NULL, '/manager/store*');
INSERT INTO `li_menu` VALUES (1367048967635861504, 'admin', '2021-03-03 09:47:27', b'0', 'admin', '2021-07-27 16:05:32', NULL, 'seller/shop/shopAuditList', 'md-alert', 2, 'shopAuth', '1367048684339986432', 'shopAuth', 1.00, '店铺审核', NULL, '/manager/store*');
INSERT INTO `li_menu` VALUES (1367049068122996736, 'admin', '2021-03-03 09:47:51', b'0', 'admin', '2021-07-27 16:05:36', NULL, 'seller/bill/bill', 'md-alert', 2, 'billList', '1367048754229673984', 'billList', 0.00, '店铺结算', NULL, '/manager/store/bill*');
INSERT INTO `li_menu` VALUES (1367049214198022144, 'admin', '2021-03-03 09:48:26', b'0', NULL, NULL, NULL, 'Main', 'md-aperture', 1, 'promotionManager', '1367040067201138688', '/', 0.00, '促销管理', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367049384792948736, 'admin', '2021-03-03 09:49:07', b'0', 'admin', '2021-07-27 16:04:42', NULL, 'promotion/coupon/coupon', 'md-alert', 2, 'coupon', '1367049214198022144', 'coupon', 0.00, '优惠券', NULL, '/manager/promotion/coupon*');
INSERT INTO `li_menu` VALUES (1367049500782231552, 'admin', '2021-03-03 09:49:34', b'0', 'admin', '2021-07-27 16:04:53', 'null', 'promotion/fullCut/full-cut', 'md-alert', 2, 'fullCut', '1367049214198022144', 'fullCut', 1.00, '满额活动', 'null', '/manager/promotion/fullDiscount*');
INSERT INTO `li_menu` VALUES (1367049611578966016, 'admin', '2021-03-03 09:50:01', b'0', 'admin', '2021-07-27 16:04:58', 'null', 'promotion/seckill/seckill', 'md-alert', 2, 'seckill', '1367049214198022144', 'seckill', 2.00, '秒杀活动', 'null', '/manager/promotion/seckill*');
INSERT INTO `li_menu` VALUES (1367049712657498112, 'admin', '2021-03-03 09:50:25', b'0', 'admin', '2021-07-27 16:05:02', 'null', 'promotion/pintuan/pintuan', 'md-alert', 2, 'pintuan', '1367049214198022144', 'pintuan', 3.00, '拼团活动', 'null', '/manager/promotion/pintuan*');
INSERT INTO `li_menu` VALUES (1367050250249830400, 'admin', '2021-03-03 09:52:33', b'0', 'admin', '2021-03-22 20:38:14', 'null', 'Main', 'md-aperture', 1, 'document', '1367040599596728320', '/', 2.00, '文章管理', 'null', NULL);
INSERT INTO `li_menu` VALUES (1367050320584114176, 'admin', '2021-03-03 09:52:50', b'0', 'admin', '2021-07-27 16:05:49', NULL, 'Main', 'md-aperture', 1, 'floor', '1367040599596728320', '/', 0.00, '楼层装修', NULL, '/manager/pageData*,/manager/file*,/manager/article-category*,/manager/article*,/manager/promotion*,/manager/goods*,/manager/store*');
INSERT INTO `li_menu` VALUES (1367050530030878720, 'admin', '2021-03-03 09:53:40', b'0', 'admin', '2021-03-04 01:05:57', 'null', 'lili-floor-renovation/floorList', 'md-alert', 2, 'pcFloor', '1367050320584114176', 'pcFloor', 0.00, 'PC端', 'null', NULL);
INSERT INTO `li_menu` VALUES (1367050673312497664, 'admin', '2021-03-03 09:54:14', b'0', 'admin', '2021-03-04 01:06:04', 'null', 'lili-floor-renovation/wap/wapList', 'md-aperture', 2, 'wapList', '1367050320584114176', 'wapList', 1.00, '移动端', 'null', NULL);
INSERT INTO `li_menu` VALUES (1367050829697122304, 'admin', '2021-03-03 09:54:51', b'0', 'admin', '2021-07-27 16:06:32', 'null', 'page/article-manage/hotWords', 'md-aperture', 2, 'hotKeyWord', '1367050250249830400', 'hotKeyWord', 0.00, '搜索热词', 'null', '/manager/hotwords*');
INSERT INTO `li_menu` VALUES (1367050939084570624, 'admin', '2021-03-03 09:55:17', b'0', 'admin', '2021-07-27 16:06:38', NULL, 'page/article-manage/ArticleCategory', 'md-aperture', 2, 'article-category', '1367050250249830400', 'article-category', 1.00, '文章分类', NULL, '/manager/article-category*');
INSERT INTO `li_menu` VALUES (1367051048232943616, 'admin', '2021-03-03 09:55:43', b'0', 'admin', '2021-07-27 16:06:42', NULL, 'page/article-manage/articleList', 'md-alert', 2, 'articleList', '1367050250249830400', 'articleList', 3.00, '文章管理', NULL, '/manager/article-category*,/manager/article*');
INSERT INTO `li_menu` VALUES (1367052616634204160, 'admin', '2021-03-03 10:01:57', b'0', 'admin', '2021-07-27 16:07:38', NULL, 'Main', 'md-aperture', 1, 'statistics', '1367040819248234496', '/', 0.00, '统计', NULL, '/manager/store*,/manager/member*');
INSERT INTO `li_menu` VALUES (1367052705725415424, 'admin', '2021-03-03 10:02:18', b'0', 'admin', '2021-03-11 22:11:05', 'null', 'statistics/member', 'md-alert', 2, 'memberStatistics', '1367052616634204160', 'memberStatistics', 0.00, '会员统计', 'null', NULL);
INSERT INTO `li_menu` VALUES (1367052805503713280, 'admin', '2021-03-03 10:02:42', b'0', 'admin', '2021-03-11 22:11:14', 'null', 'statistics/order', 'md-alert', 2, 'orderStatistics', '1367052616634204160', 'orderStatistics', 1.00, '订单统计', 'null', NULL);
INSERT INTO `li_menu` VALUES (1367052915314786304, 'admin', '2021-03-03 10:03:08', b'0', 'admin', '2021-03-11 22:11:23', 'null', 'statistics/goods', 'md-alert', 2, 'goodsStatistics', '1367052616634204160', 'goodsStatistics', 2.00, '商品统计', 'null', NULL);
INSERT INTO `li_menu` VALUES (1367053087121866752, 'admin', '2021-03-03 10:03:49', b'0', 'admin', '2021-03-11 22:11:34', 'null', 'statistics/traffic', 'md-alert', 2, 'trafficStatistics', '1367052616634204160', 'trafficStatistics', 4.00, '流量统计', 'null', NULL);
INSERT INTO `li_menu` VALUES (1372807928452481024, 'admin', '2021-03-19 02:11:30', b'0', NULL, NULL, NULL, 'Main', 'ios-aperture', 1, 'flow', '1367039534616805376', '/', 3.00, '流水', NULL, NULL);
INSERT INTO `li_menu` VALUES (1372808148565360640, 'admin', '2021-03-19 02:12:23', b'0', 'admin', '2021-07-27 16:03:57', NULL, 'order/flow/paymentLog', 'md-alert', 2, 'paymentLog', '1372807928452481024', 'paymentLog', 1.00, '收款记录', NULL, '/manager/paymentLog*');
INSERT INTO `li_menu` VALUES (1372808352295288832, 'admin', '2021-03-19 02:13:11', b'0', 'admin', '2021-07-27 16:04:01', NULL, 'order/flow/refundLog', 'ios-aperture', 2, 'refundLog', '1372807928452481024', 'refundLog', 2.00, '退款流水', NULL, '/manager/refundLog*');
INSERT INTO `li_menu` VALUES (1373166892465782784, 'admin', '2021-03-20 01:57:54', b'0', 'admin', '2021-03-22 20:13:48', 'null', 'Main', 'ios-aperture', 1, '/', '1367038467288072192', '/', 0.00, '积分', 'null', NULL);
INSERT INTO `li_menu` VALUES (1373167227385151488, 'admin', '2021-03-20 01:59:14', b'0', 'admin', '2021-07-27 16:02:40', 'null', 'member/point/point', 'ios-aperture', 2, 'point', '1373166892465782784', 'point', 0.00, '积分历史', 'null', '/manager/member/memberPointsHistory*');
INSERT INTO `li_menu` VALUES (1373791578371391488, 'admin', '2021-03-21 19:20:11', b'0', 'admin', '2021-07-27 16:05:38', NULL, 'seller/bill/accountStatementBill', 'md-alert', 2, 'accountStatementBill', '1367048754229673984', 'accountStatementBill', 0.00, '商家对账', NULL, '/manager/store/bill*');
INSERT INTO `li_menu` VALUES (1374154349697040384, 'admin', '2021-03-22 19:21:42', b'0', 'admin', '2021-07-27 16:06:55', 'null', 'Main', 'md-aperture', 1, 'feedback', '1367040599596728320', '/', 3.00, '意见反馈', 'null', '/manager/feedback*');
INSERT INTO `li_menu` VALUES (1374155741123837952, 'admin', '2021-03-22 19:27:14', b'0', 'admin', '2021-07-27 15:41:40', 'null', 'page/feedback/feedback', 'md-aperture', 2, 'feedback', '1374154349697040384', 'feedback', 0.00, '意见反馈', 'null', 'null');
INSERT INTO `li_menu` VALUES (1374173575405109248, 'admin', '2021-03-22 20:38:06', b'0', 'admin', '2021-03-22 20:52:58', 'null', 'Main', 'ios-analytics', 1, 'distributionManager', '1367040599596728320', '/', 1.00, '分销管理', 'null', NULL);
INSERT INTO `li_menu` VALUES (1374177618072436736, 'admin', '2021-03-22 20:54:10', b'0', 'admin', '2021-07-27 16:05:58', 'null', 'distribution/distributionSetting', 'ios-basketball', 2, 'distributionSetting', '1374173575405109248', 'distributionSetting', 0.00, '分销设置', 'null', '/manager/system/setting/put/DISTRIBUTION_SETTING*,/manager/system/setting/get/DISTRIBUTION_SETTING*');
INSERT INTO `li_menu` VALUES (1374177789581721600, 'admin', '2021-03-22 20:54:51', b'0', 'admin', '2021-07-27 16:06:15', 'null', 'distribution/distributionGoods', 'ios-chatbubbles', 2, 'distributionGoods', '1374173575405109248', 'distributionGoods', 3.00, '分销商品', 'null', '/manager/distribution/goods*');
INSERT INTO `li_menu` VALUES (1374177910411231232, 'admin', '2021-03-22 20:55:19', b'0', 'admin', '2021-07-27 16:06:20', 'null', 'distribution/distributionOrder', 'ios-cloudy', 2, 'distributionOrder', '1374173575405109248', 'distributionOrder', 4.00, '分销订单', 'null', '/manager/distribution/order*,/manager/store*');
INSERT INTO `li_menu` VALUES (1374178079181635584, 'admin', '2021-03-22 20:56:00', b'0', 'admin', '2021-07-27 16:06:05', 'null', 'distribution/distributionApply', 'md-egg', 2, 'distributionApply', '1374173575405109248', 'distributionApply', 1.00, '分销申请', 'null', '/manager/distribution*');
INSERT INTO `li_menu` VALUES (1374178303975358464, 'admin', '2021-03-22 20:56:53', b'0', 'admin', '2021-07-27 16:06:08', 'null', 'distribution/distribution', 'md-person', 2, 'distribution', '1374173575405109248', 'distribution', 2.00, '分销员', 'null', '/manager/distribution*');
INSERT INTO `li_menu` VALUES (1374916594269945856, 'admin', '2021-03-24 21:50:35', b'0', 'admin', '2021-07-27 16:08:51', NULL, 'sys/slider/slider', 'ios-aperture', 2, 'slider', '1349237129847005184', 'slider', 7.00, '验证码', NULL, '/manager/verificationSource*');
INSERT INTO `li_menu` VALUES (1376450531517530112, 'admin', '2021-03-29 03:25:55', b'0', NULL, NULL, NULL, 'Main', 'md-basketball', 1, 'notice', '1367040599596728320', '/', 5.00, '站内信', NULL, NULL);
INSERT INTO `li_menu` VALUES (1376450662098796544, 'admin', '2021-03-29 03:26:26', b'0', 'admin', '2021-07-27 16:07:23', NULL, 'sys/message/noticeMessageTemplate', 'ios-american-football', 2, 'noticeMessageTemplate', '1376450531517530112', 'noticeMessageTemplate', 1.00, '站内信', NULL, '/manager/noticeMessage*,/manager/message*,/manager/store*,/manager/member*');
INSERT INTO `li_menu` VALUES (1376450766817984512, 'admin', '2021-03-29 03:26:51', b'0', 'admin', '2021-03-29 03:27:25', 'null', 'Main', 'md-checkmark', 1, 'sms', '1367040599596728320', '/', 6.00, '短信管理', 'null', NULL);
INSERT INTO `li_menu` VALUES (1376450876423536640, 'admin', '2021-03-29 03:27:17', b'0', 'admin', '2021-07-27 16:07:29', NULL, 'sys/message/sms', 'ios-timer', 2, 'sms', '1376450766817984512', 'sms', 1.00, '短信', NULL, '/manager/sms*,/manager/member*');
INSERT INTO `li_menu` VALUES (1384035281702748160, 'admin', '2021-04-19 14:45:00', b'0', 'admin', '2021-07-27 16:08:18', 'null', 'member/message-manage/weChatMessageManager', 'md-aperture', 2, 'message-manage', '1349237129847005184', 'message-manage', 5.00, '微信消息', 'null', '/manager/message/wechat*,/manager/message/wechatMPMessage*');
INSERT INTO `li_menu` VALUES (1403988156444962818, 'admin', '2021-06-13 16:10:36', b'0', 'admin', '2021-07-27 16:04:48', 'null', 'promotion/couponActivity/coupon', '', 2, 'coupon-activity', '1367049214198022144', 'coupon-activity', 0.00, '券活动', 'null', '/manager/promotion/couponActivity*');
INSERT INTO `li_menu` VALUES (1407601962899230721, 'admin', '2021-06-23 15:30:35', b'0', 'admin', '2021-07-27 16:05:08', NULL, 'Main', '', 1, 'liveManage', '1367040067201138688', '/', 2.00, '直播管理', NULL, '/manager/broadcast*');
INSERT INTO `li_menu` VALUES (1407602049759072258, 'admin', '2021-06-23 15:30:55', b'0', NULL, NULL, NULL, 'promotion/live/live', '', 2, 'live', '1407601962899230721', 'live', 1.00, '直播管理', NULL, NULL);
INSERT INTO `li_menu` VALUES (1407602441964244994, 'admin', '2021-06-23 15:32:29', b'0', NULL, NULL, NULL, 'Main', '', 1, 'pointManage', '1367040067201138688', '/', 3.00, '积分活动', NULL, NULL);
INSERT INTO `li_menu` VALUES (1407602516912263170, 'admin', '2021-06-23 15:32:47', b'0', 'admin', '2021-07-27 16:05:14', NULL, 'promotion/pointsGoods/pointsGoods', '', 2, 'pointsGoods', '1407602441964244994', 'pointsGoods', 1.00, '积分商品', NULL, '/manager/promotion/pointsGoods*,/manager/goods*');
INSERT INTO `li_menu` VALUES (1407602673334636546, 'admin', '2021-06-23 15:33:24', b'0', 'admin', '2021-07-27 16:05:18', NULL, 'promotion/pointsGoodsCategory/pointsGoodsCategory', '', 2, 'pointsGoodsCategory', '1407602441964244994', 'pointsGoodsCategory', 2.00, '积分分类', NULL, '/manager/promotion/pointsGoodsCategory*');
INSERT INTO `li_menu` VALUES (1410862675914764290, 'admin', '2021-07-02 15:27:29', b'0', 'admin', '2021-07-27 16:06:26', 'null', 'distribution/distributionCash', '', 2, 'distributionCash', '1374173575405109248', 'distributionCash', 5.00, '分销提现', 'null', '/manager/distribution/cash*');
INSERT INTO `li_menu` VALUES (1419926569920536578, 'admin', '2021-07-27 15:44:10', b'0', 'admin', '2021-07-27 16:07:10', NULL, 'customWords/index', NULL, 2, 'customWords', '1367050250249830400', 'customWords', 4.00, 'ES分词', NULL, '/manager/manager/custom-words*');
COMMIT;
SET FOREIGN_KEY_CHECKS = 1;

/** 新增会员获的总积分 **/
ALTER TABLE li_member ADD total_point bigint ( 20 ) DEFAULT 0 COMMENT '积分总数量';