/**
 * 营销促销玩法：限时直降、第N件优惠
 * 及 store_flow 营销补贴字段、管理端/商家端菜单
 */

-- 限时直降
CREATE TABLE IF NOT EXISTS `li_flash_discount` (
  `id` varchar(255) NOT NULL,
  `create_by` varchar(64) DEFAULT NULL,
  `create_time` datetime DEFAULT NULL,
  `delete_flag` bit(1) DEFAULT b'0',
  `update_by` varchar(64) DEFAULT NULL,
  `update_time` datetime DEFAULT NULL,
  `store_id` varchar(255) DEFAULT NULL,
  `store_name` varchar(255) DEFAULT NULL,
  `promotion_name` varchar(255) DEFAULT NULL,
  `start_time` datetime DEFAULT NULL,
  `end_time` datetime DEFAULT NULL,
  `scope_type` varchar(255) DEFAULT 'PORTION_GOODS',
  `scope_id` mediumtext,
  `limit_num` int DEFAULT NULL COMMENT '限购数量，0不限',
  `description` varchar(500) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 第N件优惠
CREATE TABLE IF NOT EXISTS `li_nth_item_discount` (
  `id` varchar(255) NOT NULL,
  `create_by` varchar(64) DEFAULT NULL,
  `create_time` datetime DEFAULT NULL,
  `delete_flag` bit(1) DEFAULT b'0',
  `update_by` varchar(64) DEFAULT NULL,
  `update_time` datetime DEFAULT NULL,
  `store_id` varchar(255) DEFAULT NULL,
  `store_name` varchar(255) DEFAULT NULL,
  `promotion_name` varchar(255) DEFAULT NULL,
  `start_time` datetime DEFAULT NULL,
  `end_time` datetime DEFAULT NULL,
  `scope_type` varchar(255) DEFAULT 'PORTION_GOODS',
  `scope_id` mediumtext,
  `nth_num` int DEFAULT 2 COMMENT '第N件',
  `discount_type` varchar(32) DEFAULT 'HALF' COMMENT 'HALF半价/FREE免单/RATE打折',
  `discount_value` decimal(10,2) DEFAULT NULL COMMENT '折扣值，RATE时为折数',
  `description` varchar(500) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- store_flow 营销补贴字段（若已存在请跳过对应语句）
ALTER TABLE `li_store_flow` ADD COLUMN `flash_discount_subsidy` decimal(10,2) DEFAULT 0 COMMENT '限时直降补贴';
ALTER TABLE `li_store_flow` ADD COLUMN `nth_item_subsidy` decimal(10,2) DEFAULT 0 COMMENT '第N件优惠补贴';

-- 管理端菜单
INSERT INTO `li_menu` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`, `permission`)
SELECT 1997010100000000001, 'admin', NOW(), b'0', 'admin', NOW(), NULL, 'promotions/flash-discount', 'md-flash', 2, 'flash-discount', '1367048084701315072', 'flash-discount', 2.10, '限时直降', NULL, '/manager/promotion/flashDiscount/*'
FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM `li_menu` WHERE `id` = 1997010100000000001);

INSERT INTO `li_menu` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`, `permission`)
SELECT 1997010100000000004, 'admin', NOW(), b'0', 'admin', NOW(), NULL, 'promotions/nth-item-discount', 'md-pricetag', 2, 'nth-item-discount', '1367048084701315072', 'nth-item-discount', 2.13, '第N件优惠', NULL, '/manager/promotion/nthItemDiscount/*'
FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM `li_menu` WHERE `id` = 1997010100000000004);

-- 商家端菜单
INSERT INTO `li_store_menu` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `permission`)
SELECT 1997010100000000001, 'admin', NOW(), b'0', 'admin', NOW(), NULL, 'promotion/flash-discount/flash-discount', 'md-flash', 2, 'flash-discount', '1357873097859923969', 'flash-discount', '2.10', '限时直降', '/store/promotion/flashDiscount/*'
FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM `li_store_menu` WHERE `id` = 1997010100000000001);

INSERT INTO `li_store_menu` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `permission`)
SELECT 1997010100000000004, 'admin', NOW(), b'0', 'admin', NOW(), NULL, 'promotion/nth-item-discount/nth-item-discount', 'md-pricetag', 2, 'nth-item-discount', '1357873097859923969', 'nth-item-discount', '2.13', '第N件优惠', '/store/promotion/nthItemDiscount/*'
FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM `li_store_menu` WHERE `id` = 1997010100000000004);
