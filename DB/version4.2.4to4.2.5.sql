/**增加店铺发货信息**/
ALTER TABLE li_store_detail ADD  `sales_consignor_address_id` varchar(255) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '发货地址id';
ALTER TABLE li_store_detail ADD  `sales_consignor_address_path` varchar(255) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '发货地址名称';
ALTER TABLE li_store_detail ADD  `sales_consignor_detail` varchar(255) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '发货详细地址';
ALTER TABLE li_store_detail ADD  `sales_consignor_mobile` varchar(255) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '发货人手机';
ALTER TABLE li_store_detail ADD  `sales_consignor_name` varchar(255) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '发货人姓名';

/**增加电子面单店铺信息**/
ALTER TABLE `li_store_logistics` ADD `customer_name` varchar(255) DEFAULT NULL COMMENT '客户代码';
ALTER TABLE `li_store_logistics` ADD `customer_pwd` varchar(255) DEFAULT NULL COMMENT '客户密码';
ALTER TABLE `li_store_logistics` ADD `month_code` varchar(255) DEFAULT NULL COMMENT '月结号/密钥';
ALTER TABLE `li_store_logistics` ADD `send_site` varchar(255) DEFAULT NULL COMMENT '归属网点';
ALTER TABLE `li_store_logistics` ADD `send_staff` varchar(255) DEFAULT NULL COMMENT '收件快递员';
ALTER TABLE `li_store_logistics` ADD `face_sheet_flag` bit(1) DEFAULT NULL COMMENT '是否使用电子面单';
ALTER TABLE `li_store_logistics` ADD `pay_type` varchar(255) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '支付方式';
ALTER TABLE `li_store_logistics` ADD `exp_type` varchar(255) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '快递类型';

/** 店铺--默认页面是否开启**/
ALTER TABLE li_store ADD page_show bit(1) DEFAULT NULL COMMENT '默认页面是否开启';

/** 创建店员表 **/
/*
 Navicat Premium Data Transfer

 Source Server         : lilishop
 Source Server Type    : MySQL
 Source Server Version : 80025
 Source Host           : 192.168.0.116:3306
 Source Schema         : zhimai1

 Target Server Type    : MySQL
 Target Server Version : 80025
 File Encoding         : 65001

 Date: 03/03/2022 19:30:20
*/

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for li_clerk
-- ----------------------------
DROP TABLE IF EXISTS `li_clerk`;
CREATE TABLE `li_clerk`  (
                             `id` bigint NOT NULL COMMENT 'ID',
                             `create_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '创建者',
                             `create_time` datetime(6) NULL DEFAULT NULL COMMENT '创建时间',
                             `delete_flag` bit(1) NULL DEFAULT NULL COMMENT '删除标志 true/false 删除/未删除',
                             `update_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '更新者',
                             `update_time` datetime(6) NULL DEFAULT NULL COMMENT '更新时间',
                             `clerk_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '店员名称',
                             `member_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '会员id',
                             `store_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '店铺id',
                             `department_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '部门id',
                             `role_ids` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '角色',
                             `shopkeeper` bit(1) NULL DEFAULT NULL COMMENT '是否是店主',
                             `is_super` bit(1) NULL DEFAULT NULL COMMENT '是否是超级管理员 超级管理员/普通管理员',
                             `status` bit(1) NULL DEFAULT NULL COMMENT '状态',
                             PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_bin ROW_FORMAT = Dynamic;

SET FOREIGN_KEY_CHECKS = 1;


/** 店员角色**/
/*
 Navicat Premium Data Transfer

 Source Server         : lilishop
 Source Server Type    : MySQL
 Source Server Version : 80025
 Source Host           : 192.168.0.116:3306
 Source Schema         : zhimai1

 Target Server Type    : MySQL
 Target Server Version : 80025
 File Encoding         : 65001

 Date: 03/03/2022 19:30:39
*/

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for li_clerk_role
-- ----------------------------
DROP TABLE IF EXISTS `li_clerk_role`;
CREATE TABLE `li_clerk_role`  (
                                  `id` bigint NOT NULL COMMENT 'ID',
                                  `clerk_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '店员唯一id',
                                  `role_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '角色唯一id',
                                  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_bin ROW_FORMAT = Dynamic;

SET FOREIGN_KEY_CHECKS = 1;


/** 店铺部门 **/
/*
 Navicat Premium Data Transfer

 Source Server         : lilishop
 Source Server Type    : MySQL
 Source Server Version : 80025
 Source Host           : 192.168.0.116:3306
 Source Schema         : zhimai1

 Target Server Type    : MySQL
 Target Server Version : 80025
 File Encoding         : 65001

 Date: 03/03/2022 19:31:39
*/

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for li_store_department
-- ----------------------------
DROP TABLE IF EXISTS `li_store_department`;
CREATE TABLE `li_store_department`  (
                                        `id` bigint NOT NULL COMMENT 'ID',
                                        `create_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '创建者',
                                        `create_time` datetime(6) NULL DEFAULT NULL COMMENT '创建时间',
                                        `delete_flag` bit(1) NULL DEFAULT NULL COMMENT '删除标志 true/false 删除/未删除',
                                        `update_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '更新者',
                                        `update_time` datetime(6) NULL DEFAULT NULL COMMENT '更新时间',
                                        `title` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '部门名称',
                                        `store_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '店铺id',
                                        `parent_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '父id',
                                        `sort_order` decimal(20, 2) NULL DEFAULT NULL COMMENT '排序值',
                                        PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_bin ROW_FORMAT = Dynamic;

SET FOREIGN_KEY_CHECKS = 1;

/** 部门角色关联 **/
/*
 Navicat Premium Data Transfer

 Source Server         : lilishop
 Source Server Type    : MySQL
 Source Server Version : 80025
 Source Host           : 192.168.0.116:3306
 Source Schema         : zhimai1

 Target Server Type    : MySQL
 Target Server Version : 80025
 File Encoding         : 65001

 Date: 03/03/2022 19:32:01
*/

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for li_store_department_role
-- ----------------------------
DROP TABLE IF EXISTS `li_store_department_role`;
CREATE TABLE `li_store_department_role`  (
                                             `id` bigint NOT NULL COMMENT 'ID',
                                             `create_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '创建者',
                                             `create_time` datetime(6) NULL DEFAULT NULL COMMENT '创建时间',
                                             `delete_flag` bit(1) NULL DEFAULT NULL COMMENT '删除标志 true/false 删除/未删除',
                                             `update_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '更新者',
                                             `update_time` datetime(6) NULL DEFAULT NULL COMMENT '更新时间',
                                             `role_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '角色id',
                                             `department_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '部门id',
                                             PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_bin ROW_FORMAT = Dynamic;

SET FOREIGN_KEY_CHECKS = 1;

/**店铺角色菜单**/
/*
 Navicat Premium Data Transfer

 Source Server         : lilishop
 Source Server Type    : MySQL
 Source Server Version : 80025
 Source Host           : 192.168.0.116:3306
 Source Schema         : zhimai1

 Target Server Type    : MySQL
 Target Server Version : 80025
 File Encoding         : 65001

 Date: 03/03/2022 19:34:42
*/

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for li_store_menu_role
-- ----------------------------
DROP TABLE IF EXISTS `li_store_menu_role`;
CREATE TABLE `li_store_menu_role`  (
                                       `id` bigint NOT NULL COMMENT 'ID',
                                       `create_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '创建者',
                                       `create_time` datetime(6) NULL DEFAULT NULL COMMENT '创建时间',
                                       `delete_flag` bit(1) NULL DEFAULT NULL COMMENT '删除标志 true/false 删除/未删除',
                                       `update_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '更新者',
                                       `update_time` datetime(6) NULL DEFAULT NULL COMMENT '更新时间',
                                       `role_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '角色id',
                                       `menu_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '菜单',
                                       `store_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '店铺id',
                                       `is_super` bit(1) NULL DEFAULT NULL COMMENT '是否拥有操作数据权限，为否则只有查看权限',
                                       PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_bin ROW_FORMAT = Dynamic;

SET FOREIGN_KEY_CHECKS = 1;


/**店铺角色**/
    /*
 Navicat Premium Data Transfer

 Source Server         : lilishop
 Source Server Type    : MySQL
 Source Server Version : 80025
 Source Host           : 192.168.0.116:3306
 Source Schema         : zhimai1

 Target Server Type    : MySQL
 Target Server Version : 80025
 File Encoding         : 65001

 Date: 03/03/2022 19:32:59
*/

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for li_store_role
-- ----------------------------
DROP TABLE IF EXISTS `li_store_role`;
CREATE TABLE `li_store_role`  (
                                  `id` bigint NOT NULL COMMENT 'ID',
                                  `create_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '创建者',
                                  `create_time` datetime(6) NULL DEFAULT NULL COMMENT '创建时间',
                                  `delete_flag` bit(1) NULL DEFAULT NULL COMMENT '删除标志 true/false 删除/未删除',
                                  `update_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '更新者',
                                  `update_time` datetime(6) NULL DEFAULT NULL COMMENT '更新时间',
                                  `name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '角色名称',
                                  `store_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '店铺id',
                                  `default_role` bit(1) NULL DEFAULT NULL COMMENT '是否为注册默认角色',
                                  `description` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '备注',
                                  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_bin ROW_FORMAT = DYNAMIC;

SET FOREIGN_KEY_CHECKS = 1;


    /*
 Navicat Premium Data Transfer

 Source Server         : lilishop
 Source Server Type    : MySQL
 Source Server Version : 80025
 Source Host           : 192.168.0.116:3306
 Source Schema         : zhimai1

 Target Server Type    : MySQL
 Target Server Version : 80025
 File Encoding         : 65001

 Date: 02/03/2022 09:57:49
*/

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for li_menu
-- ----------------------------
DROP TABLE IF EXISTS `li_menu`;
CREATE TABLE `li_menu`  (
                            `id` bigint NOT NULL COMMENT 'ID',
                            `create_by` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '创建者',
                            `create_time` datetime NULL DEFAULT NULL COMMENT '创建时间',
                            `delete_flag` bit(1) NULL DEFAULT NULL COMMENT '删除标志 true/false 删除/未删除',
                            `update_by` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '更新者',
                            `update_time` datetime NULL DEFAULT NULL COMMENT '更新时间',
                            `description` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '说明备注',
                            `front_route` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '前端路由',
                            `icon` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '图标',
                            `level` int NULL DEFAULT NULL COMMENT '层级',
                            `name` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '菜单/权限名称',
                            `parent_id` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '父id',
                            `path` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '赋权API地址,正则表达式',
                            `sort_order` decimal(10, 2) NULL DEFAULT NULL COMMENT '排序值',
                            `title` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '菜单标题',
                            `front_component` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '文件地址',
                            `permission` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '权限url',
                            PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8 COLLATE = utf8_general_ci ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of li_menu
-- ----------------------------
INSERT INTO `li_menu` VALUES (1348810750596767744, 'admin', '2021-01-12 09:55:17', b'0', 'admin', '2021-01-15 09:42:50', 'null', '1', 'ios-american-football', 0, 'settings', '0', '1', 2.00, '设置', 'null', NULL);
INSERT INTO `li_menu` VALUES (1348810864748945408, 'admin', '2021-01-12 09:55:45', b'0', 'admin', '2021-03-15 20:57:12', 'null', 'null', 'ios-american-football', 0, 'log', '0', 'null', 3.00, '日志', 'null', '/manager/setting/log*');
INSERT INTO `li_menu` VALUES (1349237129847005184, 'admin', '2021-01-13 14:09:34', b'0', 'admin', '2021-01-15 09:43:16', 'null', 'Main', 'ios-american-football', 1, 'sys', '1348810750596767744', '/sys', 1.00, '系统设置', 'null', NULL);
INSERT INTO `li_menu` VALUES (1349237207378714624, 'admin', '2021-01-13 14:09:53', b'0', 'admin', '2021-07-27 16:07:49', 'null', 'Main', 'ios-american-football', 1, 'member', '1348810750596767744', '/member', 0.00, '用户管理', 'null', '/manager/permission/department*,/manager/passport/user*,/manager/permission/role*,/manager/permission/menu*,/manager/passport/user/admin/edit*');
INSERT INTO `li_menu` VALUES (1349237928434098176, 'admin', '2021-01-13 14:13:03', b'0', 'admin', '2021-07-27 16:09:11', 'null', 'Main', 'ios-american-football', 1, 'log', '1348810864748945408', '/log', 1.00, '系统监控', 'null', '/manager/log*');
INSERT INTO `li_menu` VALUES (1349246048900243456, 'admin', '2021-01-13 14:45:00', b'0', 'admin', '2021-07-27 16:07:57', 'null', 'sys/setting-manage/settingManage', 'ios-american-football', 2, 'setting', '1349237129847005184', 'setting', 1.00, '系统设置', 'null', '/manager/system/setting/get*,/manager/system/setting/put*,/manager/setting/setting*');
INSERT INTO `li_menu` VALUES (1349246347597602816, 'admin', '2021-01-13 14:46:12', b'0', 'admin', '2021-07-27 16:08:03', 'null', 'sys/oss-manage/ossManage', 'ios-american-football', 2, 'oss-manage', '1349237129847005184', 'oss-manage', 3.00, 'OSS资源', '', '/manager/common/file*');
INSERT INTO `li_menu` VALUES (1349246468775239680, 'admin', '2021-01-13 14:46:41', b'0', 'admin', '2021-07-27 16:08:14', 'null', 'region/index', 'ios-american-football', 2, 'region', '1349237129847005184', 'region', 4.00, '行政地区', 'null', '/manager/region*');
INSERT INTO `li_menu` VALUES (1349246671158796288, 'admin', '2021-01-13 14:47:29', b'0', 'admin', '2021-07-27 16:08:09', 'null', 'logistics/index', 'ios-american-football', 2, 'logistics', '1349237129847005184', 'logistics', 5.00, '物流公司', 'null', '/manager/other/logistics*');
INSERT INTO `li_menu` VALUES (1349246896661356544, 'admin', '2021-01-13 14:48:23', b'0', 'admin', '2021-07-27 16:08:23', 'null', 'sys/setting-manage/settingManage', 'ios-american-football', 2, 'authLogin', '1349237129847005184', 'authLogin', 6.00, '信任登录', 'null', '/manager/system/setting/get*,/manager/system/setting/put*,/manager/setting/setting*\r\n');
INSERT INTO `li_menu` VALUES (1349247081504333824, 'admin', '2021-01-13 14:49:07', b'0', 'admin', '2021-07-27 16:08:45', 'null', 'sys/setting-manage/settingManage', 'ios-american-football', 2, 'pay', '1349237129847005184', 'pay', 7.00, '支付设置', 'null', '/manager/system/setting/get*,/manager/system/setting/put*,/manager/system/setting*');
INSERT INTO `li_menu` VALUES (1349247640584085504, 'admin', '2021-01-13 14:51:20', b'0', 'admin', '2021-07-27 16:08:56', 'null', 'sensitiveWords/index', 'ios-american-football', 2, 'sensitiveWords', '1349237129847005184', 'sensitiveWords', 8.00, '敏感词', 'null', '/manager/other/sensitiveWords*');
INSERT INTO `li_menu` VALUES (1349254815809298432, 'admin', '2021-01-13 15:19:51', b'0', 'admin', '2021-01-15 11:15:40', 'null', 'sys/user-manage/userManage', 'ios-american-football', 2, 'user-manage', '1349237207378714624', 'user-manage', 1.00, '用户管理', 'null', NULL);
INSERT INTO `li_menu` VALUES (1349255214977015808, 'admin', '2021-01-13 15:21:26', b'0', 'admin', '2021-01-15 11:16:21', 'null', 'sys/department-manage/departmentManage', 'ios-american-football', 2, 'department-manage', '1349237207378714624', 'department-manage', 3.00, '部门管理', 'null', '/manager/permission/department*,/manager/permission/departmentRole*,');
INSERT INTO `li_menu` VALUES (1349255404425338880, 'admin', '2021-01-13 15:22:11', b'0', 'admin', '2021-02-24 09:22:21', 'null', 'sys/role-manage/roleManage', 'ios-american-football', 2, 'role-manage', '1349237207378714624', 'role-manage', 4.00, '角色权限', 'null', '/manager/permission/role*,/manager/permission/roleMenu*');
INSERT INTO `li_menu` VALUES (1349256082979840000, 'admin', '2021-01-13 15:24:53', b'0', 'admin', '2021-01-15 11:18:14', 'null', 'sys/log-manage/logManage', 'ios-american-football', 2, 'log-manage', '1349237928434098176', 'log-manage', 2.00, '日志管理', 'null', NULL);
INSERT INTO `li_menu` VALUES (1357584224760102912, 'admin', '2021-02-05 06:57:57', b'0', 'admin', '2021-07-27 16:09:02', 'null', 'sys/app-version/appVersion', 'ios-american-football', 2, 'appVersion', '1349237129847005184', 'appVersion', 9.00, 'APP版本', 'null', '/manager/other/appVersion*');
INSERT INTO `li_menu` VALUES (1357873097859923969, 'admin', '2021-02-24 09:53:02', b'0', 'admin', '2021-02-24 09:53:12', NULL, 'sys/menu-manage/menuManage', 'ios-american-football', 2, 'menuManage', '1349237207378714624', 'menu-manage', 2.00, '菜单管理', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367038467288072192, 'admin', '2021-03-03 09:05:44', b'0', 'admin', '2021-03-03 09:09:27', 'null', 'null', 'ios-person-add', 0, 'member', '0', 'null', 0.00, '会员', 'null', NULL);
INSERT INTO `li_menu` VALUES (1367039534616805376, 'admin', '2021-03-03 09:09:58', b'0', 'admin', '2021-05-18 10:51:12', 'null', 'null', 'md-reorder', 0, 'order', '0', 'null', 0.00, '订单', 'null', NULL);
INSERT INTO `li_menu` VALUES (1367039950368800768, 'admin', '2021-03-03 09:11:37', b'0', NULL, NULL, NULL, NULL, 'ios-share', 0, 'goods', '0', NULL, 0.20, '商品', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367040067201138688, 'admin', '2021-03-03 09:12:05', b'0', 'admin', '2021-12-02 19:45:22', NULL, 'null', 'ios-hammer', 0, 'promotions', '0', 'null', 0.30, '促销', NULL, 'null');
INSERT INTO `li_menu` VALUES (1367040599596728320, 'admin', '2021-03-03 09:14:12', b'0', 'admin', '2021-03-03 09:52:13', 'null', 'null', 'ios-color-palette', 0, 'operate', '0', 'null', 0.50, '运营', 'null', NULL);
INSERT INTO `li_menu` VALUES (1367040819248234496, 'admin', '2021-03-03 09:15:04', b'0', 'lili_ftyy', '2022-03-01 15:13:04', NULL, 'null', 'ios-stats', 0, 'statistics', '0', 'null', 0.70, '统计', NULL, 'null');
INSERT INTO `li_menu` VALUES (1367041332861730816, 'admin', '2021-03-03 09:17:07', b'0', NULL, NULL, NULL, 'Main', 'ios-aperture', 1, '/', '1367038467288072192', '/', 0.00, '会员管理', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367041461194850304, 'admin', '2021-03-03 09:17:37', b'0', 'admin', '2021-07-27 16:02:17', NULL, 'member/list/index', 'ios-aperture', 2, 'memberList', '1367041332861730816', 'memberList', 0.00, '会员列表', NULL, '/manager/common/file*,/manager/passport/member*');
INSERT INTO `li_menu` VALUES (1367041575619657728, 'admin', '2021-03-03 09:18:05', b'0', 'admin', '2021-07-27 15:59:50', NULL, 'member/list/memberRecycle', 'ios-aperture', 2, 'memberRecycle', '1367041332861730816', 'memberRecycle', 1.00, '回收站', NULL, '/manager/member*');
INSERT INTO `li_menu` VALUES (1367042490443497472, 'admin', '2021-03-03 09:21:43', b'0', NULL, NULL, NULL, 'Main', 'ios-aperture', 1, '/', '1367038467288072192', '/', 1.00, '预存款', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367042664410644480, 'admin', '2021-03-03 09:22:24', b'0', 'admin', '2021-07-27 16:02:44', 'null', 'member/advance/walletLog', 'ios-aperture', 2, 'walletLog', '1367042490443497472', 'walletLog', 0.00, '会员资金', 'null', '/manager/wallet/log*');
INSERT INTO `li_menu` VALUES (1367042804944994304, 'admin', '2021-03-03 09:22:58', b'0', 'admin', '2021-07-27 16:02:48', NULL, 'member/advance/recharge', 'ios-alert', 2, 'recharge', '1367042490443497472', 'recharge', 1.00, '充值记录', NULL, '/manager/wallet/recharge*');
INSERT INTO `li_menu` VALUES (1367042804944994305, 'admin', '2021-03-03 09:22:58', b'0', 'admin', '2021-07-27 16:02:52', NULL, 'member/advance/withdrawApply', 'ios-alert', 2, 'withdrawApply', '1367042490443497472', 'withdrawApply', 1.00, '提现申请', NULL, '/manager/wallet/withdrawApply*');
INSERT INTO `li_menu` VALUES (1367042917113266176, 'admin', '2021-03-03 09:23:25', b'0', NULL, NULL, NULL, 'Main', 'ios-aperture', 1, 'commont', '1367038467288072192', '/', 0.00, '评价', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367043020976816128, 'admin', '2021-03-03 09:23:49', b'0', 'admin', '2021-07-27 16:02:35', NULL, 'goods/goods-review/index', 'md-aperture', 2, 'goodsReview', '1367042917113266176', 'goodsReview', 0.00, '会员评价', NULL, '/manager/memberEvaluation*');
INSERT INTO `li_menu` VALUES (1367043443917848576, 'admin', '2021-03-03 09:25:30', b'0', 'admin', '2021-07-27 16:03:00', NULL, 'Main', 'md-aperture', 1, 'order', '1367039534616805376', '/', 0.00, '订单', NULL, '/manager/orders*');
INSERT INTO `li_menu` VALUES (1367043505771249664, 'admin', '2021-03-03 09:25:45', b'0', NULL, NULL, NULL, 'Main', 'md-aperture', 1, 'aftersale', '1367039534616805376', '/', 0.00, '售后', NULL, NULL);
INSERT INTO `li_menu` VALUES (1367043642379730944, 'admin', '2021-03-03 09:26:17', b'0', NULL, NULL, NULL, 'order/order/orderList', 'ios-aperture', 2, 'orderList', '1367043443917848576', 'orderList', 0.00, '商品订单', NULL, '/manager/order/order*');
INSERT INTO `li_menu` VALUES (1367043791105556480, 'admin', '2021-03-03 09:26:53', b'0', NULL, NULL, NULL, 'order/order/fictitiousOrderList', 'ios-aperture', 2, 'fictitiousOrderList', '1367043443917848576', 'fictitiousOrderList', 1.00, '虚拟订单', NULL, '/manager/order/order*');
INSERT INTO `li_menu` VALUES (1367043980407078912, 'admin', '2021-03-03 09:27:38', b'0', 'admin', '2021-07-27 16:03:43', NULL, 'order/after-order/afterSaleOrder', 'md-alert', 2, 'afterSaleOrder', '1367043505771249664', 'afterSaleOrder', 0.00, '售后管理', NULL, '/manager/order/afterSale*');
INSERT INTO `li_menu` VALUES (1367044121163726848, 'admin', '2021-03-03 09:28:12', b'0', 'admin', '2021-07-27 16:03:48', NULL, 'order/after-order/orderComplaint', 'md-alert', 2, 'orderComplaint', '1367043505771249664', 'orderComplaint', 2.00, '交易投诉', NULL, '/manager/order/complain*');
INSERT INTO `li_menu` VALUES (1367044247978508288, 'admin', '2021-03-03 09:28:42', b'0', 'admin', '2021-07-27 16:03:52', NULL, 'order/after-order/afterSale', 'md-aperture', 2, 'afterSaleReason', '1367043505771249664', 'afterSaleReason', 3.00, '售后原因', NULL, '/manager/order/afterSaleReason*');
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
INSERT INTO `li_menu` VALUES (1367048832210173952, 'admin', '2021-03-03 09:46:55', b'0', 'admin', '2021-07-27 16:05:30', NULL, 'seller/shop/shopList', 'md-aperture', 2, 'shopList', '1367048684339986432', 'shopList', 0.00, '店铺列表', NULL, '/manager/order/order*,/manager/store*');
INSERT INTO `li_menu` VALUES (1367048967635861504, 'admin', '2021-03-03 09:47:27', b'0', 'admin', '2021-07-27 16:05:32', NULL, 'seller/shop/shopAuditList', 'md-alert', 2, 'shopAuth', '1367048684339986432', 'shopAuth', 1.00, '店铺审核', NULL, '/manager/store*');
INSERT INTO `li_menu` VALUES (1367049068122996736, 'admin', '2021-03-03 09:47:51', b'0', 'admin', '2021-07-27 16:05:36', NULL, 'seller/bill/bill', 'md-alert', 2, 'billList', '1367048754229673984', 'billList', 0.00, '店铺结算', NULL, '/manager/order/bill*');
INSERT INTO `li_menu` VALUES (1367049214198022144, 'admin', '2021-03-03 09:48:26', b'0', 'admin', '2021-12-02 19:45:28', NULL, 'Main', 'md-aperture', 1, 'promotionsManager', '1367040067201138688', '/', 0.00, '促销管理', NULL, 'null');
INSERT INTO `li_menu` VALUES (1367049384792948736, 'admin', '2021-03-03 09:49:07', b'0', 'admin', '2021-12-02 19:54:12', NULL, 'promotions/coupon/coupon', 'md-alert', 2, 'promotions/coupon', '1367049214198022144', 'promotions/coupon', 0.00, '优惠券', NULL, '/manager/promotion/coupon*');
INSERT INTO `li_menu` VALUES (1367049500782231552, 'admin', '2021-03-03 09:49:34', b'0', 'admin', '2021-12-02 19:41:37', 'null', 'promotions/full-discount/full-discount', 'md-alert', 2, 'promotions/full-discount', '1367049214198022144', 'promotions/full-discount', 1.00, '满额活动', 'null', '/manager/promotion/fullDiscount*');
INSERT INTO `li_menu` VALUES (1367049611578966016, 'admin', '2021-03-03 09:50:01', b'0', 'admin', '2021-12-02 20:16:10', 'null', 'promotions/seckill/seckill', 'md-alert', 2, 'promotions/seckill', '1367049214198022144', 'promotions/seckill', 2.00, '秒杀活动', 'null', '/manager/promotion/seckill*');
INSERT INTO `li_menu` VALUES (1367049712657498112, 'admin', '2021-03-03 09:50:25', b'0', 'admin', '2021-12-02 20:22:04', 'null', 'promotions/pintuan/pintuan', 'md-alert', 2, 'promotions/pintuan', '1367049214198022144', 'promotions/pintuan', 3.00, '拼团活动', 'null', '/manager/promotion/pintuan*');
INSERT INTO `li_menu` VALUES (1367050250249830400, 'admin', '2021-03-03 09:52:33', b'0', 'admin', '2021-03-22 20:38:14', 'null', 'Main', 'md-aperture', 1, 'document', '1367040599596728320', '/', 2.00, '文章管理', 'null', NULL);
INSERT INTO `li_menu` VALUES (1367050320584114176, 'admin', '2021-03-03 09:52:50', b'0', 'admin', '2021-07-27 16:05:49', NULL, 'Main', 'md-aperture', 1, 'floor', '1367040599596728320', '/', 0.00, '楼层装修', NULL, '/manager/pageData*,/manager/file*,/manager/article-category*,/manager/article*,/manager/promotion*,/manager/goods*,/manager/store*');
INSERT INTO `li_menu` VALUES (1367050530030878720, 'admin', '2021-03-03 09:53:40', b'0', 'admin', '2021-03-04 01:05:57', 'null', 'page-decoration/floorList', 'md-alert', 2, 'pcFloor', '1367050320584114176', 'pcFloor', 0.00, 'PC端', 'null', '/manager/other/pageData*');
INSERT INTO `li_menu` VALUES (1367050673312497664, 'admin', '2021-03-03 09:54:14', b'0', 'admin', '2021-03-04 01:06:04', 'null', 'page-decoration/wap/wapList', 'md-aperture', 2, 'wapList', '1367050320584114176', 'wapList', 1.00, '移动端', 'null', '/manager/other/pageData*');
INSERT INTO `li_menu` VALUES (1367050829697122304, 'admin', '2021-03-03 09:54:51', b'0', 'admin', '2021-07-27 16:06:32', 'null', 'page/article-manage/hotWords', 'md-aperture', 2, 'hotKeyWord', '1367050250249830400', 'hotKeyWord', 0.00, '搜索热词', 'null', '/manager/hotwords*');
INSERT INTO `li_menu` VALUES (1367050939084570624, 'admin', '2021-03-03 09:55:17', b'0', 'admin', '2021-07-27 16:06:38', NULL, 'page/article-manage/ArticleCategory', 'md-aperture', 2, 'article-category', '1367050250249830400', 'article-category', 1.00, '文章分类', NULL, '/manager/other/articleCategory*');
INSERT INTO `li_menu` VALUES (1367051048232943616, 'admin', '2021-03-03 09:55:43', b'0', 'admin', '2021-07-27 16:06:42', NULL, 'page/article-manage/articleList', 'md-alert', 2, 'articleList', '1367050250249830400', 'articleList', 3.00, '文章管理', NULL, '/manager/other/article*,/manager/other/articleCategory*');
INSERT INTO `li_menu` VALUES (1367052616634204160, 'admin', '2021-03-03 10:01:57', b'0', 'admin', '2021-07-27 16:07:38', NULL, 'Main', 'md-aperture', 1, 'statistics', '1367040819248234496', '/', 0.00, '统计', NULL, '/manager/store*,/manager/member*');
INSERT INTO `li_menu` VALUES (1367052705725415424, 'admin', '2021-03-03 10:02:18', b'0', 'admin', '2021-03-11 22:11:05', 'null', 'statistics/member', 'md-alert', 2, 'memberStatistics', '1367052616634204160', 'memberStatistics', 0.00, '会员统计', 'null', NULL);
INSERT INTO `li_menu` VALUES (1367052805503713280, 'admin', '2021-03-03 10:02:42', b'0', 'admin', '2021-03-11 22:11:14', 'null', 'statistics/order', 'md-alert', 2, 'orderStatistics', '1367052616634204160', 'orderStatistics', 1.00, '订单统计', 'null', NULL);
INSERT INTO `li_menu` VALUES (1367052915314786304, 'admin', '2021-03-03 10:03:08', b'0', 'admin', '2021-03-11 22:11:23', 'null', 'statistics/goods', 'md-alert', 2, 'goodsStatistics', '1367052616634204160', 'goodsStatistics', 2.00, '商品统计', 'null', NULL);
INSERT INTO `li_menu` VALUES (1367053087121866752, 'admin', '2021-03-03 10:03:49', b'0', 'admin', '2021-03-11 22:11:34', 'null', 'statistics/traffic', 'md-alert', 2, 'trafficStatistics', '1367052616634204160', 'trafficStatistics', 4.00, '流量统计', 'null', NULL);
INSERT INTO `li_menu` VALUES (1372807928452481024, 'admin', '2021-03-19 02:11:30', b'0', NULL, NULL, NULL, 'Main', 'ios-aperture', 1, 'flow', '1367039534616805376', '/', 3.00, '流水', NULL, NULL);
INSERT INTO `li_menu` VALUES (1372808148565360640, 'admin', '2021-03-19 02:12:23', b'0', 'admin', '2021-07-27 16:03:57', NULL, 'order/flow/paymentLog', 'md-alert', 2, 'paymentLog', '1372807928452481024', 'paymentLog', 1.00, '收款记录', NULL, '/manager/order/paymentLog*');
INSERT INTO `li_menu` VALUES (1372808352295288832, 'admin', '2021-03-19 02:13:11', b'0', 'admin', '2021-07-27 16:04:01', NULL, 'order/flow/refundLog', 'ios-aperture', 2, 'refundLog', '1372807928452481024', 'refundLog', 2.00, '退款流水', NULL, '/manager/order/refundLog*');
INSERT INTO `li_menu` VALUES (1373166892465782784, 'admin', '2021-03-20 01:57:54', b'0', 'admin', '2021-03-22 20:13:48', 'null', 'Main', 'ios-aperture', 1, '/', '1367038467288072192', '/', 0.00, '积分', 'null', NULL);
INSERT INTO `li_menu` VALUES (1373167227385151488, 'admin', '2021-03-20 01:59:14', b'0', 'admin', '2021-07-27 16:02:40', 'null', 'member/point/point', 'ios-aperture', 2, 'point', '1373166892465782784', 'point', 0.00, '积分历史', 'null', '/manager/member/memberPointsHistory*');
INSERT INTO `li_menu` VALUES (1373791578371391488, 'admin', '2021-03-21 19:20:11', b'0', 'admin', '2021-07-27 16:05:38', NULL, 'seller/bill/accountStatementBill', 'md-alert', 2, 'accountStatementBill', '1367048754229673984', 'accountStatementBill', 0.00, '商家对账', NULL, '/manager/order/bill*');
INSERT INTO `li_menu` VALUES (1374154349697040384, 'admin', '2021-03-22 19:21:42', b'0', 'admin', '2021-07-27 16:06:55', 'null', 'Main', 'md-aperture', 1, 'feedback', '1367040599596728320', '/', 3.00, '意见反馈', 'null', '/manager/other/feedback*');
INSERT INTO `li_menu` VALUES (1374155741123837952, 'admin', '2021-03-22 19:27:14', b'0', 'admin', '2021-07-27 15:41:40', 'null', 'page/feedback/feedback', 'md-aperture', 2, 'feedback', '1374154349697040384', 'feedback', 0.00, '意见反馈', 'null', 'null');
INSERT INTO `li_menu` VALUES (1374173575405109248, 'admin', '2021-03-22 20:38:06', b'0', 'admin', '2021-03-22 20:52:58', 'null', 'Main', 'ios-analytics', 1, 'distributionManager', '1367040599596728320', '/', 1.00, '分销管理', 'null', NULL);
INSERT INTO `li_menu` VALUES (1374177618072436736, 'admin', '2021-03-22 20:54:10', b'0', 'admin', '2021-07-27 16:05:58', 'null', 'distribution/distributionSetting', 'ios-basketball', 2, 'distributionSetting', '1374173575405109248', 'distributionSetting', 0.00, '分销设置', 'null', '/manager/system/setting/put/DISTRIBUTION_SETTING*,/manager/system/setting/get/DISTRIBUTION_SETTING*');
INSERT INTO `li_menu` VALUES (1374177789581721600, 'admin', '2021-03-22 20:54:51', b'0', 'admin', '2021-07-27 16:06:15', 'null', 'distribution/distributionGoods', 'ios-chatbubbles', 2, 'distributionGoods', '1374173575405109248', 'distributionGoods', 3.00, '分销商品', 'null', '/manager/distribution/goods*');
INSERT INTO `li_menu` VALUES (1374177910411231232, 'admin', '2021-03-22 20:55:19', b'0', 'admin', '2021-07-27 16:06:20', 'null', 'distribution/distributionOrder', 'ios-cloudy', 2, 'distributionOrder', '1374173575405109248', 'distributionOrder', 4.00, '分销订单', 'null', '/manager/distribution/order*,/manager/store*');
INSERT INTO `li_menu` VALUES (1374178079181635584, 'admin', '2021-03-22 20:56:00', b'0', 'admin', '2021-07-27 16:06:05', 'null', 'distribution/distributionApply', 'md-egg', 2, 'distributionApply', '1374173575405109248', 'distributionApply', 1.00, '分销申请', 'null', '/manager/distribution*');
INSERT INTO `li_menu` VALUES (1374178303975358464, 'admin', '2021-03-22 20:56:53', b'0', 'admin', '2021-07-27 16:06:08', 'null', 'distribution/distribution', 'md-person', 2, 'distribution', '1374173575405109248', 'distribution', 2.00, '分销员', 'null', '/manager/distribution*');
INSERT INTO `li_menu` VALUES (1374916594269945856, 'admin', '2021-03-24 21:50:35', b'0', 'admin', '2021-07-27 16:08:51', NULL, 'sys/slider/slider', 'ios-aperture', 2, 'slider', '1349237129847005184', 'slider', 7.00, '验证码', NULL, '/manager/other/verificationSource*');
INSERT INTO `li_menu` VALUES (1376450531517530112, 'admin', '2021-03-29 03:25:55', b'0', NULL, NULL, NULL, 'Main', 'md-basketball', 1, 'notice', '1367040599596728320', '/', 5.00, '站内信', NULL, NULL);
INSERT INTO `li_menu` VALUES (1376450662098796544, 'admin', '2021-03-29 03:26:26', b'0', 'admin', '2021-07-27 16:07:23', NULL, 'sys/message/noticeMessageTemplate', 'ios-american-football', 2, 'noticeMessageTemplate', '1376450531517530112', 'noticeMessageTemplate', 1.00, '站内信', NULL, '/manager/other/message*');
INSERT INTO `li_menu` VALUES (1376450766817984512, 'admin', '2021-03-29 03:26:51', b'0', 'admin', '2021-03-29 03:27:25', 'null', 'Main', 'md-checkmark', 1, 'sms', '1367040599596728320', '/', 6.00, '短信管理', 'null', NULL);
INSERT INTO `li_menu` VALUES (1376450876423536640, 'admin', '2021-03-29 03:27:17', b'0', 'admin', '2021-07-27 16:07:29', NULL, 'sys/message/sms', 'ios-timer', 2, 'sms', '1376450766817984512', 'sms', 1.00, '短信', NULL, '/manager/sms/sms*,/manager/passport/member*');
INSERT INTO `li_menu` VALUES (1384035281702748160, 'admin', '2021-04-19 14:45:00', b'0', 'admin', '2021-07-27 16:08:18', 'null', 'member/message-manage/weChatMessageManager', 'md-aperture', 2, 'message-manage', '1349237129847005184', 'message-manage', 5.00, '微信消息', 'null', '/manager/wechat/wechatMessage*');
INSERT INTO `li_menu` VALUES (1403988156444962818, 'admin', '2021-06-13 16:10:36', b'0', 'admin', '2021-12-02 19:54:37', 'null', 'promotions/coupon-activity/coupon', '', 2, 'promotions/coupon-activity', '1367049214198022144', 'promotions/coupon-activity', 0.00, '券活动', 'null', '/manager/promotion/couponActivity*');
INSERT INTO `li_menu` VALUES (1407601962899230721, 'admin', '2021-06-23 15:30:35', b'0', 'admin', '2021-07-27 16:05:08', NULL, 'Main', '', 1, 'liveManage', '1367040067201138688', '/', 2.00, '直播管理', NULL, '/manager/broadcast*');
INSERT INTO `li_menu` VALUES (1407602049759072258, 'admin', '2021-06-23 15:30:55', b'0', 'admin', '2021-12-07 10:54:54', NULL, 'promotions/live/live', '', 2, 'promotions/live', '1407601962899230721', 'promotions/live', 1.00, '直播管理', NULL, 'null');
INSERT INTO `li_menu` VALUES (1407602441964244994, 'admin', '2021-06-23 15:32:29', b'0', NULL, NULL, NULL, 'Main', '', 1, 'pointManage', '1367040067201138688', '/', 3.00, '积分活动', NULL, NULL);
INSERT INTO `li_menu` VALUES (1407602516912263170, 'admin', '2021-06-23 15:32:47', b'0', 'admin', '2021-12-03 19:18:30', NULL, 'promotions/points-goods/points-goods', '', 2, 'promotions/points-goods', '1407602441964244994', 'promotions/points-goods', 1.00, '积分商品', NULL, '/manager/promotion/pointsGoods*,/manager/goods*');
INSERT INTO `li_menu` VALUES (1407602673334636546, 'admin', '2021-06-23 15:33:24', b'0', 'admin', '2021-12-03 19:19:23', NULL, 'promotions/points-goods-category/points-goods-category', '', 2, 'promotions/points-goods-category', '1407602441964244994', 'promotions/points-goods-category', 2.00, '积分分类', NULL, '/manager/promotion/pointsGoodsCategory*');
INSERT INTO `li_menu` VALUES (1410862675914764290, 'admin', '2021-07-02 15:27:29', b'0', 'admin', '2021-07-27 16:06:26', 'null', 'distribution/distributionCash', '', 2, 'distributionCash', '1374173575405109248', 'distributionCash', 5.00, '分销提现', 'null', '/manager/distribution/cash*');
INSERT INTO `li_menu` VALUES (1419926569920536578, 'admin', '2021-07-27 15:44:10', b'0', 'admin', '2021-07-27 16:07:10', NULL, 'customWords/index', NULL, 2, 'customWords', '1367050250249830400', 'customWords', 4.00, 'ES分词', NULL, '/manager/other/customWords*');
INSERT INTO `li_menu` VALUES (1430799171593535490, 'admin', '2021-08-26 15:48:00', b'0', 'admin', '2021-12-02 20:21:34', NULL, 'promotions/kanjia/kanjia-activity-goods', NULL, 2, 'promotions/kanjia', '1367049214198022144', 'promotions/kanjia', 6.00, '砍价活动', NULL, '/manager/promotion/kanJiaGoods*');
INSERT INTO `li_menu` VALUES (1495665663207432193, 'admin', '2022-02-21 15:44:17', b'0', 'admin', '2022-02-21 15:45:44', NULL, 'distribution/distrbutionGrade', NULL, 2, 'distrbutionGrade', '1374173575405109248', 'distrbutionGrade', 0.00, '分销等级', NULL, '/manager/distrbutionGrade*');

SET FOREIGN_KEY_CHECKS = 1;



    /*
 Navicat Premium Data Transfer

 Source Server         : lilishop
 Source Server Type    : MySQL
 Source Server Version : 80025
 Source Host           : 192.168.0.116:3306
 Source Schema         : zhimai1

 Target Server Type    : MySQL
 Target Server Version : 80025
 File Encoding         : 65001

 Date: 01/03/2022 15:36:03
*/

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for li_store_menu
-- ----------------------------
DROP TABLE IF EXISTS `li_store_menu`;
CREATE TABLE `li_store_menu`  (
                                  `id` bigint NOT NULL COMMENT 'ID',
                                  `create_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '创建者',
                                  `create_time` datetime(6) NULL DEFAULT NULL COMMENT '创建时间',
                                  `delete_flag` bit(1) NULL DEFAULT NULL COMMENT '删除标志 true/false 删除/未删除',
                                  `update_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '更新者',
                                  `update_time` datetime NULL DEFAULT NULL COMMENT '更新时间',
                                  `description` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '说明备注',
                                  `front_route` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '前端路由',
                                  `icon` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '图标',
                                  `level` int NULL DEFAULT NULL COMMENT '层级',
                                  `name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '菜单/权限名称',
                                  `parent_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '父id',
                                  `path` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '赋权API地址,正则表达式',
                                  `sort_order` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '排序值',
                                  `title` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '菜单标题',
                                  `permission` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '权限url',
                                  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_bin ROW_FORMAT = DYNAMIC;

-- ----------------------------
-- Records of li_store_menu
-- ----------------------------
INSERT INTO `li_store_menu` VALUES (1348810750596767744, NULL, '2022-01-11 22:35:33.000000', b'0', NULL, '2022-01-11 22:36:52', NULL, 'Main', 'ios-american-football', 0, 'goods', '0', '/', '1', '商品', NULL);
INSERT INTO `li_store_menu` VALUES (1348810864748945408, NULL, '2022-01-11 22:35:37.000000', b'0', NULL, '2022-01-11 22:36:55', NULL, 'Main', 'ios-american-football', 1, 'Main', '1348810750596767744', '/', '1.1', '商品管理', '');
INSERT INTO `li_store_menu` VALUES (1349237129847005184, NULL, '2022-01-11 22:35:40.000000', b'0', NULL, '2022-01-11 22:36:59', NULL, 'goods-operation', 'ios-american-football', 2, 'goods-operation', '1348810864748945408', '/goods-operation', '1.11', '商品发布', '/store/goods/category*,/store/goods/categorySpec*,/store/goods/categoryParameters*,/store/goods/draftGoods*,/store/goods/label*,/store/goods/goods*,/store/goods/goodsUnit*,/store/goods/spec*');
INSERT INTO `li_store_menu` VALUES (1349237207378714624, NULL, '2022-01-11 22:35:42.000000', b'0', NULL, '2022-01-11 22:37:02', NULL, 'goods/goods-seller/goods', 'ios-american-football', 2, 'goods', '1348810864748945408', 'goods', '1.12', '商品列表', NULL);
INSERT INTO `li_store_menu` VALUES (1349237928434098176, NULL, '2022-01-11 22:35:45.000000', b'0', NULL, '2022-01-11 22:37:05', NULL, 'goods/goods-seller/draftGoods', 'ios-american-football', 2, 'template-goods', '1348810864748945408', 'template-goods', '1.13', '商品模板', NULL);
INSERT INTO `li_store_menu` VALUES (1349246048900243456, NULL, '2022-01-11 22:35:48.000000', b'0', NULL, '2022-01-11 22:37:08', NULL, 'goods/goods-manage/category', 'ios-american-football', 2, 'category', '1348810864748945408', 'category', '1.14', '店铺分类', NULL);
INSERT INTO `li_store_menu` VALUES (1349246347597602816, NULL, '2022-01-11 22:35:51.000000', b'0', NULL, '2022-01-11 22:37:11', NULL, 'Main', 'ios-american-football', 0, 'order', '0', '/', '2', '订单', NULL);
INSERT INTO `li_store_menu` VALUES (1349246468775239680, NULL, '2022-01-11 22:38:28.000000', b'0', NULL, '2022-01-11 22:38:31', NULL, 'Main', 'ios-american-football', 1, 'Main', '1349246347597602816', '/', '2.1', '订单管理', NULL);
INSERT INTO `li_store_menu` VALUES (1349246671158796288, NULL, '2022-01-11 22:38:42.000000', b'0', NULL, '2022-01-11 22:38:35', NULL, 'order/order/orderList', 'ios-american-football', 2, 'orderList', '1349246468775239680', 'orderList', '2.11', '商品订单', '/store/order/order*');
INSERT INTO `li_store_menu` VALUES (1349246896661356544, NULL, '2022-01-11 22:40:27.000000', b'0', NULL, '2022-01-11 22:40:34', NULL, 'order/order/virtualOrderList', 'ios-american-football', 2, 'virtualOrderList', '1349246468775239680', 'virtualOrderList', '2.12', '虚拟订单', '/store/order/order*');
INSERT INTO `li_store_menu` VALUES (1349247081504333824, NULL, '2022-01-11 22:41:47.000000', b'0', NULL, '2022-01-11 22:41:50', NULL, 'Main', 'ios-american-football', 1, 'Main', '1349246347597602816', '/', '2.2', '评价管理', NULL);
INSERT INTO `li_store_menu` VALUES (1349247640584085504, NULL, '2022-01-11 22:43:29.000000', b'0', NULL, '2022-01-11 22:45:47', NULL, 'member/memberComment', 'ios-american-football', 2, 'memberComment', '1349247081504333824', 'memberComment', '2.21', '评价管理', '/store/member/evaluation*');
INSERT INTO `li_store_menu` VALUES (1349254815809298432, NULL, '2022-01-11 22:45:35.000000', b'0', NULL, '2022-01-11 22:45:50', NULL, 'Main', 'ios-american-football', 1, 'Main', '1349246347597602816', '/', '2.3', '售后管理', NULL);
INSERT INTO `li_store_menu` VALUES (1349255214977015808, NULL, '2022-01-11 22:49:22.000000', b'0', NULL, '2022-01-11 22:49:27', NULL, 'order/after-order/returnGoodsOrder', 'ios-american-football', 2, 'returnGoodsOrder', '1349254815809298432', 'returnGoodsOrder', '2.31', '退货管理', '/store/order/afterSale*');
INSERT INTO `li_store_menu` VALUES (1349255404425338880, NULL, '2022-01-11 22:51:20.000000', b'0', NULL, '2022-01-11 22:51:22', NULL, 'order/after-order/returnMoneyOrder', 'ios-american-football', 2, 'returnMoneyOrder', '1349254815809298432', 'returnMoneyOrder', '2.32', '退款管理', '/store/order/afterSale*');
INSERT INTO `li_store_menu` VALUES (1349256082979840000, NULL, '2022-01-11 22:52:50.000000', b'0', NULL, '2022-01-11 22:52:55', NULL, 'order/after-order/orderComplaint', 'ios-american-football', 2, 'orderComplaint', '1349254815809298432', 'orderComplaint', '2.33', '投诉管理', '/store/order/complain*');
INSERT INTO `li_store_menu` VALUES (1357584224760102912, NULL, '2022-01-11 23:02:20.000000', b'0', NULL, '2022-01-11 23:02:25', NULL, 'Main', 'ios-american-football', 0, 'bill', '0', '/', '3', '财务', NULL);
INSERT INTO `li_store_menu` VALUES (1357873097859923969, NULL, '2022-01-11 23:04:13.000000', b'0', NULL, '2022-01-11 23:04:17', NULL, 'Main', 'ios-american-football', 1, 'Main', '1357584224760102912', '/', '3.1', '财务管理', '');
INSERT INTO `li_store_menu` VALUES (1367038467288072192, NULL, '2022-01-11 23:06:11.000000', b'0', NULL, '2022-01-11 23:06:16', NULL, 'shop/bill/accountStatementBill', 'ios-american-football', 2, 'accountStatementBill', '1357873097859923969', 'accountStatementBill', '3.11', '财务对账', '/store/order/bill*');
INSERT INTO `li_store_menu` VALUES (1367039534616805376, NULL, '2022-01-11 23:07:54.000000', b'0', NULL, '2022-01-11 23:07:57', NULL, 'shop/bill/storeBill', 'ios-american-football', 2, 'storeBill', '1357873097859923969', 'storeBill', '3.12', '店铺结算', '/store/order/bill*');
INSERT INTO `li_store_menu` VALUES (1367039950368800768, NULL, '2022-01-11 23:09:26.000000', b'0', NULL, '2022-01-11 23:09:29', NULL, 'Main', 'ios-american-football', 1, 'Main', '1357584224760102912', '/', '3.2', '发票管理', NULL);
INSERT INTO `li_store_menu` VALUES (1367040067201138688, NULL, '2022-01-11 23:11:14.000000', b'0', NULL, '2022-01-11 23:11:18', NULL, 'order/receiptManager/receipt', 'ios-american-football', 2, 'receipt', '1367039950368800768', 'receipt', '3.21', '发票管理', '/store/trade/receipt*');
INSERT INTO `li_store_menu` VALUES (1367040599596728320, NULL, '2022-01-11 23:12:49.000000', b'0', NULL, '2022-01-11 23:12:52', NULL, 'Main', 'ios-american-football', 0, 'promotion', '0', '/', '4', '营销', NULL);
INSERT INTO `li_store_menu` VALUES (1367040819248234496, NULL, '2022-01-11 23:14:35.000000', b'0', NULL, '2022-01-11 23:14:38', NULL, 'Main', 'ios-american-football', 1, 'Main', '1367040599596728320', '/', '4.1', '平台活动', NULL);
INSERT INTO `li_store_menu` VALUES (1367041332861730816, NULL, '2022-01-11 23:15:42.000000', b'0', NULL, '2022-01-11 23:15:48', NULL, 'promotion/pintuan/pintuan', 'ios-american-football', 2, 'pintuan', '1367040819248234496', 'pintuan', '4.11', '拼团活动', '/store/promotion/pintuan*');
INSERT INTO `li_store_menu` VALUES (1367041461194850304, NULL, '2022-01-11 23:17:32.000000', b'0', NULL, '2022-01-11 23:17:44', NULL, 'promotion/seckill/seckill', 'ios-american-football', 2, 'seckill', '1367040819248234496', 'seckill', '4.12', '秒杀活动', '/store/promotion/seckill*');
INSERT INTO `li_store_menu` VALUES (1367041575619657728, NULL, '2022-01-11 23:20:19.000000', b'0', NULL, '2022-01-11 23:20:24', NULL, 'Main', 'ios-american-football', 1, 'Main', '1367040599596728320', '/', '4.2', '直播活动', '');
INSERT INTO `li_store_menu` VALUES (1367042490443497472, NULL, '2022-01-11 23:21:13.000000', b'0', NULL, '2022-01-11 23:21:22', NULL, 'promotion/live/live', 'ios-american-football', 2, 'live', '1367041575619657728', 'live', '4.21', '直播管理', '/store/broadcast/studio*');
INSERT INTO `li_store_menu` VALUES (1367042664410644480, NULL, '2022-01-11 23:22:42.000000', b'0', NULL, '2022-01-11 23:22:59', NULL, 'promotion/live/liveGoods', 'ios-american-football', 2, 'liveGoods', '1367041575619657728', 'liveGoods', '4.22', '直播商品', '/store/broadcast/commodity*');
INSERT INTO `li_store_menu` VALUES (1367042804944994304, NULL, '2022-01-11 23:24:24.000000', b'0', NULL, '2022-01-11 23:24:38', NULL, 'Main', 'ios-american-football', 1, 'Main', '1367040599596728320', '/', '4.3', '商家活动', NULL);
INSERT INTO `li_store_menu` VALUES (1367042804944994305, NULL, '2022-01-11 23:24:29.000000', b'0', NULL, '2022-01-11 23:24:42', NULL, 'promotion/full-discount/full-discount', 'ios-american-football', 2, 'full-cut', '1367042804944994304', 'full-discount', '4.31', '满额活动', '/store/promotion/fullDiscount*');
INSERT INTO `li_store_menu` VALUES (1367042917113266176, NULL, '2022-01-11 23:26:45.000000', b'0', NULL, '2022-01-11 23:26:50', NULL, 'promotion/coupon/coupon', 'ios-american-football', 2, 'coupon', '1367042804944994304', 'coupon', '4.32', '优惠券', '/store/promotion/coupon*');
INSERT INTO `li_store_menu` VALUES (1367043020976816128, NULL, '2022-01-11 23:28:50.000000', b'0', NULL, '2022-01-11 23:29:02', NULL, 'Main', 'ios-american-football', 1, 'Main', '1367040599596728320', '/', '4.4', '分销管理', NULL);
INSERT INTO `li_store_menu` VALUES (1367043443917848576, NULL, '2022-01-11 23:28:53.000000', b'0', NULL, '2022-01-11 23:29:04', NULL, 'distribution/distributionGoods', 'ios-american-football', 2, 'distributionGoods', '1367043020976816128', 'distributionGoods', '4.41', '分销商品', '/store/distribution/goods*');
INSERT INTO `li_store_menu` VALUES (1367043505771249664, NULL, '2022-01-11 23:28:56.000000', b'0', NULL, '2022-01-11 23:29:07', NULL, 'distribution/distributionOrder', 'ios-american-football', 2, 'distributionOrder', '1367043020976816128', 'distributionOrderdistributionOrder', '4.42', '分销订单', '/store/distribution/order*');
INSERT INTO `li_store_menu` VALUES (1367044121163726848, NULL, '2022-01-12 21:47:03.000000', b'0', NULL, '2022-01-12 21:47:25', NULL, 'Main', 'ios-american-football', 0, 'statistics', '0', '/', '5', '统计', NULL);
INSERT INTO `li_store_menu` VALUES (1367044247978508288, NULL, '2022-01-12 21:47:19.000000', b'0', NULL, '2022-01-12 21:47:28', NULL, 'Main', 'ios-american-football', 1, 'Main', '1367044121163726848', '/', '5.1', '统计管理', NULL);
INSERT INTO `li_store_menu` VALUES (1367044376391319552, NULL, '2022-01-12 21:49:45.000000', b'0', NULL, '2022-01-12 21:50:01', NULL, 'statistics/goods', 'ios-american-football', 2, 'goodsStatistics', '1367044247978508288', 'goodsStatistics', '5.11', '商品统计', '/store/statistics/goods*');
INSERT INTO `li_store_menu` VALUES (1367044657296441344, NULL, '2022-01-12 21:49:48.000000', b'0', NULL, '2022-01-12 21:49:58', NULL, 'statistics/order', 'ios-american-football', 2, 'orderStatistics', '1367044247978508288', 'orderStatistics', '5.12', '订单统计', '/store/statistics/order*,/store/statistics/order*,/store/statistics/order*,/store/statistics/order*');
INSERT INTO `li_store_menu` VALUES (1367045529720061952, NULL, '2022-01-12 21:49:51.000000', b'0', NULL, '2022-01-12 21:50:03', NULL, 'statistics/traffic', 'ios-american-football', 2, 'trafficStatistics', '1367044247978508288', 'trafficStatistics', '5.13', '流量统计', '/store/statistics/view*');
INSERT INTO `li_store_menu` VALUES (1367045630710513664, NULL, '2022-01-12 21:52:59.000000', b'0', NULL, '2022-01-12 21:53:09', NULL, 'Main', 'ios-american-football', 0, 'settings', '0', '/', '6', '设置', NULL);
INSERT INTO `li_store_menu` VALUES (1367045794284175360, NULL, '2022-01-12 21:53:03.000000', b'0', NULL, '2022-01-12 21:53:12', NULL, 'Main', 'ios-american-football', 1, 'Main', '1367045630710513664', '/', '6.1', '配送设置', NULL);
INSERT INTO `li_store_menu` VALUES (1367045921434501120, NULL, '2022-01-12 21:55:49.000000', b'0', NULL, '2022-01-12 21:55:52', NULL, 'shop/ship/shipTemplate', 'ios-american-football', 2, 'shipTemplate', '1367045794284175360', 'shipTemplate', '6.11', '配送模板', '/store/setting/freightTemplate*');
INSERT INTO `li_store_menu` VALUES (1367046068369358848, NULL, '2022-01-12 21:58:05.000000', b'0', NULL, '2022-01-12 21:58:13', NULL, 'shop/ship/logistics', 'ios-american-football', 2, 'logistics', '1367045794284175360', 'logistics', '6.12', '物流公司', '/store/other/logistics*');
INSERT INTO `li_store_menu` VALUES (1367046266214678528, NULL, '2022-01-12 21:59:07.000000', b'0', NULL, '2022-01-12 21:59:43', NULL, 'Main', 'ios-american-football', 1, 'Main', '1367045630710513664', '/', '6.2', '店铺管理', NULL);
INSERT INTO `li_store_menu` VALUES (1367048084701315072, NULL, '2022-01-12 21:59:32.000000', b'0', NULL, '2022-01-12 21:59:48', NULL, 'shop/shopSetting', 'ios-american-football', 2, 'shopSetting', '1367046266214678528', 'shopSetting', '6.21', '店铺设置', NULL);
INSERT INTO `li_store_menu` VALUES (1367048684339986432, NULL, '2022-01-12 21:59:36.000000', b'0', NULL, '2022-01-12 21:59:51', NULL, 'shop/shopAddress', 'ios-american-football', 2, 'shopAddress', '1367046266214678528', 'shopAddress', '6.22', '自提管理', '/store/member/storeAddress*');
INSERT INTO `li_store_menu` VALUES (1367048754229673984, NULL, '2022-01-12 22:02:11.000000', b'0', NULL, '2022-01-12 22:04:36', NULL, 'Main', 'ios-american-football', 0, 'Main', '0', '/', '7', '消息', NULL);
INSERT INTO `li_store_menu` VALUES (1367048832210173952, NULL, '2022-01-12 22:02:49.000000', b'0', NULL, '2022-01-12 22:04:39', NULL, 'Main', 'ios-american-football', 1, 'Main', '1367048754229673984', '/', '7.1', '系统消息', NULL);
INSERT INTO `li_store_menu` VALUES (1367048967635861503, NULL, '2022-02-18 16:08:30.000000', b'0', NULL, '2022-02-18 16:08:36', NULL, 'Main', 'ios-american-football', 1, 'Main', '1367045630710513664', '/', '6.3', '店员设置', '');
INSERT INTO `li_store_menu` VALUES (1367048967635861504, NULL, '2022-01-12 22:02:51.000000', b'0', NULL, '2022-01-12 22:04:45', NULL, 'message', 'ios-american-football', 2, 'message_index', '1367048832210173952', 'message', '7.11', '系统消息', '/store/message/storeMessage*');
INSERT INTO `li_store_menu` VALUES (1367048967635861505, NULL, '2022-02-18 16:12:18.000000', b'0', NULL, '2022-02-18 16:12:21', NULL, 'shop/system/clerk/clerkManage', 'ios-american-football', 2, '\nclerkManage', '1367048967635861503', '\nclerkManage', '6.31', '店员管理', '/store/department*,/store/clerk*,/store/role*,/store/department*');
INSERT INTO `li_store_menu` VALUES (1367048967635861506, NULL, '2022-02-18 16:25:27.000000', NULL, NULL, '2022-02-18 16:25:31', NULL, 'shop/system/department/storeDepartmentManage', 'ios-american-football', 2, 'storeDepartmentManage', '1367048967635861503', 'storeDepartmentManage', '6.32', '部门管理', '/store/department*');
INSERT INTO `li_store_menu` VALUES (1367048967635861507, NULL, '2022-02-18 16:27:28.000000', NULL, NULL, '2022-02-18 16:27:30', NULL, 'shop/system/role/storeRoleManage', 'ios-american-football', 2, 'storeRoleManage', '1367048967635861503', 'storeRoleManage', '6.33', '角色权限', '/store/role*');
INSERT INTO `li_store_menu` VALUES (1367048967635861510, NULL, '2022-03-07 14:45:10.000000', b'0', NULL, '2022-03-07 14:45:13', NULL, 'shop/floorList', 'ios-american-football', 2, 'floorList', '1367046266214678528', 'floorList', '6.23', 'PC端', '/store/other/pageData*');
INSERT INTO `li_store_menu` VALUES (1367048967635861511, NULL, '2022-03-07 15:13:52.000000', b'0', NULL, '2022-03-07 15:13:55', NULL, 'shop/wap/wapList', 'ios-american-football', 2, 'wapList', '1367046266214678528', 'wapList', '6.24', '移动端', '/store/other/pageData*');

SET FOREIGN_KEY_CHECKS = 1;


insert li_clerk(id,create_by,create_time,store_id,clerk_name,member_id,shopkeeper,is_super,`status`) select id,member_name,current_date(),id,member_name,member_id ,TRUE,TRUE,TRUE from li_store


