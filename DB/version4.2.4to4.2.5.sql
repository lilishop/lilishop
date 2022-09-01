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

