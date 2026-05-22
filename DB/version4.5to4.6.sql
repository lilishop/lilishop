DROP TABLE IF EXISTS `li_member_group`;
CREATE TABLE `li_member_group` (
                                   `id` bigint NOT NULL COMMENT 'ID',
                                   `create_by` varchar(255) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
                                   `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
                                   `delete_flag` bit(1) DEFAULT NULL COMMENT '删除标志 true/false 删除/未删除',
                                   `update_by` varchar(255) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '更新者',
                                   `update_time` datetime(6) DEFAULT NULL COMMENT '更新时间',
                                   `group_name` varchar(255) COLLATE utf8mb4_bin NOT NULL COMMENT '分组名称',
                                   `description` varchar(255) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '分组描述',
                                   PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=Dynamic;

DROP TABLE IF EXISTS `li_member_group_user`;
CREATE TABLE `li_member_group_user` (
                                        `id` bigint NOT NULL COMMENT 'ID',
                                        `create_by` varchar(255) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
                                        `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
                                        `delete_flag` bit(1) DEFAULT NULL COMMENT '删除标志 true/false 删除/未删除',
                                        `update_by` varchar(255) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '更新者',
                                        `update_time` datetime(6) DEFAULT NULL COMMENT '更新时间',
                                        `group_id` varchar(255) COLLATE utf8mb4_bin NOT NULL COMMENT '分组ID',
                                        `member_id` varchar(255) COLLATE utf8mb4_bin NOT NULL COMMENT '客户ID',
                                        PRIMARY KEY (`id`),
                                        UNIQUE KEY `uniq_group_member` (`group_id`,`member_id`),
                                        KEY `idx_group_id` (`group_id`),
                                        KEY `idx_member_id` (`member_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=Dynamic;
SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

CREATE TABLE IF NOT EXISTS `li_goods_group` (
  `id` bigint NOT NULL COMMENT 'ID',
  `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
  `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
  `delete_flag` bit(1) DEFAULT b'0' COMMENT '删除标志 true/false 删除/未删除',
  `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '更新者',
  `update_time` datetime(6) DEFAULT NULL COMMENT '更新时间',
  `group_name` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '分组名称',
  `description` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '分组描述',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_li_goods_group_create_time` (`create_time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='商品分组';

CREATE TABLE IF NOT EXISTS `li_goods_group_goods` (
  `id` bigint NOT NULL COMMENT 'ID',
  `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
  `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
  `delete_flag` bit(1) DEFAULT b'0' COMMENT '删除标志 true/false 删除/未删除',
  `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '更新者',
  `update_time` datetime(6) DEFAULT NULL COMMENT '更新时间',
  `group_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '分组ID',
  `goods_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '商品ID',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `uk_li_goods_group_goods` (`group_id`,`goods_id`) USING BTREE,
  KEY `idx_li_goods_group_goods_group_id` (`group_id`) USING BTREE,
  KEY `idx_li_goods_group_goods_goods_id` (`goods_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='商品分组商品关联';

SET FOREIGN_KEY_CHECKS = 1;
SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- 在 商品 -> 关联管理/关联商品 下新增子菜单：商品分组
-- parent_id = 1367044657296441344 （二级目录 association / Main）
-- 若已存在同 parent 且同 path 节点，则不会重复插入
INSERT INTO `li_menu` (
  `id`,
  `create_by`,
  `create_time`,
  `delete_flag`,
  `update_by`,
  `update_time`,
  `description`,
  `front_route`,
  `icon`,
  `level`,
  `name`,
  `parent_id`,
  `path`,
  `sort_order`,
  `title`,
  `front_component`,
  `permission`
)
SELECT
  200401020001000001,
  'admin',
  NOW(),
  b'0',
  'admin',
  NOW(),
  '商品-关联商品子菜单-商品分组',
  'goods/group/index',
  'md-list',
  2,
  'managerGoodsGroup',
  '1367044657296441344',
  'goods-group',
  3.00,
  '商品分组',
  NULL,
  '/manager/goods/goodsGroup*'
FROM DUAL
WHERE NOT EXISTS (
  SELECT 1
  FROM `li_menu`
  WHERE `parent_id` = '1367044657296441344'
    AND `path` = 'goods-group'
);

SET FOREIGN_KEY_CHECKS = 1;

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- 在 商品 -> 关联管理/关联商品 下新增子菜单：虚拟销量
-- parent_id = 1367044657296441344 （二级目录 association / Main）
-- 若已存在同 parent 且同 path 节点，则不会重复插入
INSERT INTO `li_menu` (
  `id`,
  `create_by`,
  `create_time`,
  `delete_flag`,
  `update_by`,
  `update_time`,
  `description`,
  `front_route`,
  `icon`,
  `level`,
  `name`,
  `parent_id`,
  `path`,
  `sort_order`,
  `title`,
  `front_component`,
  `permission`
)
SELECT
  200401020001000002,
  'admin',
  NOW(),
  b'0',
  'admin',
  NOW(),
  '商品-关联商品子菜单-虚拟销量',
  'goods/virtual-sales/index',
  'md-stats',
  2,
  'goods-virtual-sales',
  '1367044657296441344',
  'goods-virtual-sales',
  3.50,
  '虚拟销量',
  NULL,
  '/manager/goods/goods/list*,/manager/goods/goods/get*'
FROM DUAL
WHERE NOT EXISTS (
  SELECT 1
  FROM `li_menu`
  WHERE `parent_id` = '1367044657296441344'
    AND `path` = 'goods-virtual-sales'
);

SET FOREIGN_KEY_CHECKS = 1;

ALTER TABLE `li_goods_sku`
ADD COLUMN `virtual_sales` INT DEFAULT 0 COMMENT '虚拟销量' AFTER `buy_count`;
SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- 扩展发票信息字段（买家发票详情/订单发票信息使用）
ALTER TABLE `li_receipt`
  ADD COLUMN `receipt_type` varchar(32) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '发票类型：电子普通发票/增值税专用发票' AFTER `receipt_title`,
  ADD COLUMN `receipt_phone` varchar(64) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '收票人手机号' AFTER `taxpayer_id`,
  ADD COLUMN `receipt_email` varchar(128) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '收票人邮箱' AFTER `receipt_phone`,
  ADD COLUMN `company_address` varchar(255) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '单位地址' AFTER `receipt_email`,
  ADD COLUMN `company_phone` varchar(64) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '单位电话' AFTER `company_address`,
  ADD COLUMN `bank_name` varchar(128) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '开户银行' AFTER `company_phone`,
  ADD COLUMN `bank_account` varchar(128) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '银行账户' AFTER `bank_name`,
  ADD COLUMN `personal_name` varchar(128) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '个人名称' AFTER `receipt_title`,
  ADD COLUMN `company_name` varchar(128) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '单位名称' AFTER `personal_name`,
 ADD COLUMN `invoice_address` varchar(500) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin DEFAULT NULL COMMENT '发票地址(URL)' AFTER `company_name`;

ALTER TABLE `li_member_evaluation`
    ADD COLUMN `top` bit(1) NOT NULL DEFAULT b'0' COMMENT '是否置顶' AFTER `reply_status`;


CREATE TABLE IF NOT EXISTS `li_member_grade` (
  `id` bigint NOT NULL COMMENT 'ID',
  `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
  `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
  `delete_flag` bit(1) DEFAULT b'0' COMMENT '删除标志 true/false 删除/未删除',
  `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '更新者',
  `update_time` datetime(6) DEFAULT NULL COMMENT '更新时间',
  `grade_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '等级名称',
  `is_default` bit(1) NOT NULL DEFAULT b'0' COMMENT '是否默认等级',
  `grade_image` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '等级图标URL',
  `grade_background` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '等级背景图URL',
  `grade_font_color` varchar(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '等级字体颜色',
  `required_experience` int NOT NULL COMMENT '所需经验值',
  `grade_sort` int NOT NULL DEFAULT 1 COMMENT '等级排序',
  `grade_state` varchar(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL DEFAULT 'OPEN' COMMENT '等级开关 OPEN/CLOSE',
  `benefit_ids` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '会员权益ID列表',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `uk_li_member_grade_name` (`grade_name`) USING BTREE,
  UNIQUE KEY `uk_li_member_grade_required_experience` (`required_experience`) USING BTREE,
  KEY `idx_li_member_grade_state_sort` (`grade_state`,`grade_sort`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='客户等级';

-- 兼容存量库：补齐默认等级字段
SET @li_member_grade_is_default_exists := (
  SELECT COUNT(1)
  FROM INFORMATION_SCHEMA.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'li_member_grade'
    AND COLUMN_NAME = 'is_default'
);
SET @li_member_grade_is_default_sql := IF(
  @li_member_grade_is_default_exists = 0,
  'ALTER TABLE `li_member_grade` ADD COLUMN `is_default` bit(1) NOT NULL DEFAULT b''0'' COMMENT ''是否默认等级'' AFTER `grade_name`',
  'SELECT 1'
);
PREPARE li_stmt FROM @li_member_grade_is_default_sql;
EXECUTE li_stmt;
DEALLOCATE PREPARE li_stmt;


CREATE TABLE IF NOT EXISTS `li_member_experience_log` (
  `id` bigint NOT NULL COMMENT 'ID',
  `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
  `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
  `delete_flag` bit(1) DEFAULT b'0' COMMENT '删除标志 true/false 删除/未删除',
  `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '更新者',
  `update_time` datetime(6) DEFAULT NULL COMMENT '更新时间',
  `member_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '客户ID',
  `member_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '客户名称',
  `rule_key` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '规则编码',
  `biz_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '业务ID',
  `experience` bigint NOT NULL DEFAULT 0 COMMENT '当前经验值',
  `before_experience` bigint NOT NULL DEFAULT 0 COMMENT '变动前经验值',
  `variable_experience` bigint NOT NULL DEFAULT 0 COMMENT '变动经验值',
  `content` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '备注',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `uk_li_member_experience_log_member_rule_biz` (`member_id`,`rule_key`,`biz_id`) USING BTREE,
  KEY `idx_li_member_experience_log_member_rule` (`member_id`,`rule_key`) USING BTREE,
  KEY `idx_li_member_experience_log_create_time` (`create_time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='客户经验值流水';

CREATE TABLE IF NOT EXISTS `li_member_share_log` (
  `id` bigint NOT NULL COMMENT 'ID',
  `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
  `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
  `delete_flag` bit(1) DEFAULT b'0' COMMENT '删除标志 true/false 删除/未删除',
  `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '更新者',
  `update_time` datetime(6) DEFAULT NULL COMMENT '更新时间',
  `member_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '会员ID',
  `share_scene` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '分享场景',
  `share_page` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '分享页面',
  `related_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '关联业务ID',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_li_member_share_log_member` (`member_id`) USING BTREE,
  KEY `idx_li_member_share_log_create_time` (`create_time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='会员分享记录';


CREATE TABLE IF NOT EXISTS `li_member_share_code` (
  `id` bigint NOT NULL COMMENT 'ID',
  `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
  `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
  `delete_flag` bit(1) DEFAULT b'0' COMMENT '删除标志 true/false 删除/未删除',
  `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '更新者',
  `update_time` datetime(6) DEFAULT NULL COMMENT '更新时间',
  `member_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '会员ID',
  `share_code` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '分享码',
  `state` varchar(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL DEFAULT 'OPEN' COMMENT '状态 OPEN/CLOSE',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `uk_li_member_share_code_member` (`member_id`) USING BTREE,
  UNIQUE KEY `uk_li_member_share_code_code` (`share_code`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='会员分享码';

CREATE TABLE IF NOT EXISTS `li_member_share_register_log` (
  `id` bigint NOT NULL COMMENT 'ID',
  `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
  `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
  `delete_flag` bit(1) DEFAULT b'0' COMMENT '删除标志 true/false 删除/未删除',
  `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '更新者',
  `update_time` datetime(6) DEFAULT NULL COMMENT '更新时间',
  `inviter_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '分享人ID',
  `invitee_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '被分享注册用户ID',
  `invitee_mobile` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '被分享注册用户手机号',
  `share_code` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '分享码',
  `reward_experience` bigint NOT NULL DEFAULT 0 COMMENT '奖励经验值',
  `reward_status` varchar(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '奖励状态 SUCCESS/SKIPPED',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `uk_li_member_share_register_log_invitee` (`invitee_id`) USING BTREE,
  KEY `idx_li_member_share_register_log_inviter` (`inviter_id`) USING BTREE,
  KEY `idx_li_member_share_register_log_create_time` (`create_time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='分享注册奖励记录';

CREATE TABLE IF NOT EXISTS `li_member_share_buy_log` (
  `id` bigint NOT NULL COMMENT 'ID',
  `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
  `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
  `delete_flag` bit(1) DEFAULT b'0' COMMENT '删除标志 true/false 删除/未删除',
  `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '更新者',
  `update_time` datetime(6) DEFAULT NULL COMMENT '更新时间',
  `inviter_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '分享人ID',
  `invitee_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '被分享购买用户ID',
  `order_sn` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '订单编号',
  `order_amount` double DEFAULT NULL COMMENT '订单金额',
  `reward_experience` bigint NOT NULL DEFAULT 0 COMMENT '奖励经验值',
  `reward_status` varchar(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '奖励状态 SUCCESS/SKIPPED',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `uk_li_member_share_buy_log_order_sn` (`order_sn`) USING BTREE,
  KEY `idx_li_member_share_buy_log_inviter` (`inviter_id`) USING BTREE,
  KEY `idx_li_member_share_buy_log_create_time` (`create_time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='分享购买奖励记录';

CREATE TABLE IF NOT EXISTS `li_member_benefit` (
   `id` bigint NOT NULL COMMENT 'ID',
   `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
    `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
    `delete_flag` bit(1) DEFAULT b'0' COMMENT '删除标志 true/false 删除/未删除',
    `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '更新者',
    `update_time` datetime(6) DEFAULT NULL COMMENT '更新时间',
    `benefit_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '权益名称',
    `benefit_logo` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '权益LOGO',
    `benefit_type` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '权益类型',
    `benefit_desc` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '权益说明',
    `benefit_sort` int NOT NULL DEFAULT 1 COMMENT '排序',
    `benefit_state` varchar(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL DEFAULT 'OPEN' COMMENT '状态 OPEN/CLOSE',
    `benefit_config` varchar(2000) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '扩展配置(JSON)',
    PRIMARY KEY (`id`) USING BTREE,
    KEY `idx_li_member_benefit_state_sort` (`benefit_state`,`benefit_sort`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='客户权益';

CREATE TABLE IF NOT EXISTS `li_member_grade_benefit_grant` (
    `id` bigint NOT NULL COMMENT 'ID',
    `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '创建者',
    `create_time` datetime(6) DEFAULT NULL COMMENT '创建时间',
    `delete_flag` bit(1) DEFAULT b'0' COMMENT '删除标志',
    `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '更新者',
    `update_time` datetime(6) DEFAULT NULL COMMENT '更新时间',
    `member_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '会员ID',
    `grade_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '等级ID',
    PRIMARY KEY (`id`) USING BTREE,
    UNIQUE KEY `uk_member_grade_benefit_grant` (`member_id`,`grade_id`) USING BTREE,
    KEY `idx_grade_id` (`grade_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='等级权益发放记录（每个会员每个等级最多一条）';




INSERT INTO `li_member_grade` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `grade_name`, `is_default`, `grade_image`, `grade_background`, `grade_font_color`, `required_experience`, `grade_sort`, `grade_state`, `benefit_ids`) VALUES (2053052134349000706, 'admin', '2026-05-09 17:59:07.351000', b'0', 'admin', '2026-05-14 18:31:55.976000', '青铜会员', b'1', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/undefined/faff58991aa843cfb82736df62a01be5.png', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/undefined/6ce081d844bf4eca91e22375f4eb8250.png', '', 1, 1, 'OPEN', '2054486882581671938,2054490052556947458');
INSERT INTO `li_member_grade` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `grade_name`, `is_default`, `grade_image`, `grade_background`, `grade_font_color`, `required_experience`, `grade_sort`, `grade_state`, `benefit_ids`) VALUES (2054025986981900289, 'admin', '2026-05-12 10:28:51.912000', b'0', 'admin', '2026-05-14 18:33:17.942000', '白银客户', b'0', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/undefined/28b03dc7f07a4ce0bf5597c9407a2395.png', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/undefined/51a3870f22fb40e0b5882dedb6bee011.png', '#566671', 500, 2, 'OPEN', '2054490052556947458,2054504770944266241,2054486882581671938');
INSERT INTO `li_member_grade` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `grade_name`, `is_default`, `grade_image`, `grade_background`, `grade_font_color`, `required_experience`, `grade_sort`, `grade_state`, `benefit_ids`) VALUES (2054125386035027969, 'admin', '2026-05-12 17:03:50.494000', b'0', 'admin', '2026-05-14 18:34:27.212000', '黄金客户', b'0', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/undefined/ba25bbcc5b674c4c950ec381fa94b7b0.png', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/undefined/fcc8894b9fea42fba47799f46e3d2a5c.png', '', 2000, 3, 'OPEN', '');
INSERT INTO `li_member_grade` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `grade_name`, `is_default`, `grade_image`, `grade_background`, `grade_font_color`, `required_experience`, `grade_sort`, `grade_state`, `benefit_ids`) VALUES (2054873406041178113, 'admin', '2026-05-14 18:36:12.360000', b'0', NULL, NULL, '铂金客户', b'0', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/undefined/3138f452a2574a62912529cb5f141fe9.png', 'https://lilishop-oss.oss-cn-beijing.aliyuncs.com/MANAGER/undefined/85ce70f0f41f4489a6c4c9874998d188.png', '', 5000, 4, 'OPEN', NULL);

INSERT INTO `li_menu` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`, `permission`) VALUES (2053037117402689538, 'admin', '2026-05-09 16:59:27', b'0', NULL, NULL, NULL, 'Main', NULL, 1, 'member-grade', '1367038467288072192', '/', 10.00, '客户等级', NULL, '');
INSERT INTO `li_menu` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`, `permission`) VALUES (2053037215004143617, 'admin', '2026-05-09 16:59:50', b'0', NULL, NULL, NULL, 'member-grade', NULL, 2, 'member-grade', '2053037117402689538', 'member-grade', 1.00, '等级列表', NULL, '');
INSERT INTO `li_menu` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`, `permission`) VALUES (2053042774654746625, 'admin', '2026-05-09 17:21:56', b'0', 'admin', '2026-05-12 18:39:16', NULL, 'member-grade-experience', NULL, 2, 'member-grade-experience', '2053037117402689538', 'member-grade-experience', 2.00, '经验值设置', NULL, '');
INSERT INTO `li_menu` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`, `permission`) VALUES (2053060244543295489, 'admin', '2026-05-09 18:31:21', b'0', 'admin', '2026-05-09 18:31:39', NULL, 'member-grade-experience-log', NULL, 2, 'member-grade-experience-log', '2053037117402689538', 'member-grade-experience-log', 3.00, '经验值记录', NULL, '');
INSERT INTO `li_menu` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`, `permission`) VALUES (2054458120364294145, 'admin', '2026-05-13 15:06:01', b'0', 'admin', '2026-05-14 18:25:54', NULL, 'member-benefit', NULL, 2, 'member-benefit', '2053037117402689538', 'member-benefit', 4.00, '客户权益', NULL, '');

INSERT INTO `li_setting` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `setting_value`) VALUES ('EXPERIENCE_SETTING', 'admin', '2026-05-09 17:48:35.153000', b'0', 'admin', '2026-05-14 18:37:25.848000', '{\"items\":[{\"ruleKey\":\"CONSUME\",\"ruleName\":\"消费\",\"enabled\":false,\"value\":1,\"maxValue\":null},{\"ruleKey\":\"REGISTER\",\"ruleName\":\"注册\",\"enabled\":false,\"value\":10,\"maxValue\":null},{\"ruleKey\":\"SIGN_IN\",\"ruleName\":\"签到\",\"enabled\":false,\"value\":10,\"maxValue\":null},{\"ruleKey\":\"COMMENT\",\"ruleName\":\"评价\",\"enabled\":false,\"value\":5,\"maxValue\":null},{\"ruleKey\":\"SHARE\",\"ruleName\":\"分享商城\",\"enabled\":false,\"value\":1,\"maxValue\":100},{\"ruleKey\":\"PROFILE\",\"ruleName\":\"完善信息\",\"enabled\":false,\"value\":10,\"maxValue\":null},{\"ruleKey\":\"FOLLOW_STORE\",\"ruleName\":\"关注店铺\",\"enabled\":false,\"value\":1,\"maxValue\":100},{\"ruleKey\":\"BIND_WECHAT\",\"ruleName\":\"绑定微信\",\"enabled\":false,\"value\":100,\"maxValue\":null},{\"ruleKey\":\"ADD_ADDRESS\",\"ruleName\":\"添加收货地址\",\"enabled\":false,\"value\":10,\"maxValue\":null},{\"ruleKey\":\"SHARE_REGISTER\",\"ruleName\":\"分享注册\",\"enabled\":false,\"value\":1,\"maxValue\":100},{\"ruleKey\":\"SHARE_BUY\",\"ruleName\":\"分享购买\",\"enabled\":false,\"value\":1,\"maxValue\":100}],\"description\":\"签到领京豆活动规则\\n • 活动说明 参与活动前请认真阅读活动规则，参与本次活动即视为用户同意并接受本规则的全部内容。\\n • 活动举办方 本活动由京东举办。\\n • 活动时间（本活动所有时间均以北京时间为准） 1. 活动时间：2024年11月15日12:00:00至2025年12月31日23:59:59\\n • 活动玩法 1. 活动条件：符合【京东注册】条件的用户可参与本次签到领京豆活动。本次活动仅限京东普通个人用户参与，企业用户、渠道经销商、专卖店、批发商用户、异常用户不予参与。 2. 签到玩法：活动期间内，用户可通过【京东首页-京豆ICON-签到领京豆】或者【京东首页-我的-京豆】入口来访签到，可获得签到奖励。每位京东用户【每天】仅可参与【1】次本活动，超过前述次数限制的将被视为无效。3. 连签玩法：当连续签到至规定天数时，当天签到可额外获得京豆奖励。过程中任意一天中断签到，则本轮次连续签到周期从头开始计算奖励（例如：后天可领取第7天的签到奖励，但第6天时没有签到，则第7天签到时不可领取奖励，将从第1天开始计算签到进程和奖励）。完成一个轮次的连续签到后，循环下一个连签奖励周期重新开始（例\"}');



CREATE TABLE `li_gift_card_cash_activity` (
  `id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL,
  `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
  `create_time` datetime(6) DEFAULT NULL,
  `delete_flag` bit(1) DEFAULT b'0',
  `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
  `update_time` datetime(6) DEFAULT NULL,

  `gift_card_name` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '活动名称/礼品卡名称',
  `card_type` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL DEFAULT 'CASH' COMMENT 'CASH/PICKUP(本期未实现)',
  `face_value` decimal(12,2) NOT NULL COMMENT '单卡面额',
  `validity_type` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL DEFAULT 'LONG_TERM',
  `valid_end_time` datetime(6) DEFAULT NULL,
  `valid_months_after_activate` int DEFAULT NULL,
  `stock_total` int NOT NULL COMMENT '活动库存：最多可制凭证张数',

  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_li_gift_card_cash_activity_time` (`create_time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='现金礼品卡活动';

CREATE TABLE `li_gift_card_cash_create_batch` (
  `id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL,
  `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
  `create_time` datetime(6) DEFAULT NULL,
  `delete_flag` bit(1) DEFAULT b'0',
  `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
  `update_time` datetime(6) DEFAULT NULL,

  `activity_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '所属活动',
  `quantity` int NOT NULL COMMENT '本批次制卡张数',
  `batch_name` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '本批备注（可选）',
  `create_user_name` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '制卡人',
  `create_user_mobile` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '制卡人手机号',

  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_li_gift_card_cash_create_batch_activity` (`activity_id`),
  KEY `idx_li_gift_card_cash_create_batch_time` (`create_time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='现金礼品卡制卡批次';

CREATE TABLE `li_gift_card_cash_issue_batch` (
  `id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL,
  `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
  `create_time` datetime(6) DEFAULT NULL,
  `delete_flag` bit(1) DEFAULT b'0',
  `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
  `update_time` datetime(6) DEFAULT NULL,

  `activity_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '活动ID',
  `create_batch_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '制卡批次（活动池发放时为空）',
  `issue_user_name` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '发卡人',
  `issue_user_mobile` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '发卡人手机号',
  `total_cards` int NOT NULL COMMENT '本次发卡总张数',
  `total_rows` int NOT NULL COMMENT '导入有效行数',

  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_li_gift_card_cash_issue_batch_activity` (`activity_id`),
  KEY `idx_li_gift_card_cash_issue_batch_create` (`create_batch_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='现金礼品卡发卡/发送记录';

CREATE TABLE `li_gift_card_cash_credential` (
  `id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL,
  `create_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
  `create_time` datetime(6) DEFAULT NULL,
  `delete_flag` bit(1) DEFAULT b'0',
  `update_by` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
  `update_time` datetime(6) DEFAULT NULL,

  `activity_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL,
  `create_batch_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '制卡批次ID（制卡写入；导入发卡可为空）',
  `issue_batch_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
  `member_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '发卡后会员ID',
  `bind_time` datetime(6) DEFAULT NULL COMMENT '绑定时间（用户与卡绑定时写入）',
  `activate_member_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '激活会员ID',
  `activate_time` datetime(6) DEFAULT NULL COMMENT '激活时间（激活后方可使用）',
  `expire_time` datetime(6) DEFAULT NULL COMMENT '卡到期时间（按活动有效期规则计算）',

  `card_no` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL,
  `card_secret` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL,
  `face_value` decimal(12,2) NOT NULL COMMENT '单卡面额',
  `balance` decimal(12,2) DEFAULT NULL COMMENT '当前余额（发卡后初始化为面额）',
  `status` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT 'UNISSUED/BOUND/VOIDED/EXPIRED',

  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `uk_li_gift_card_cash_credential_card_no` (`card_no`),
  KEY `idx_li_gift_card_cash_cred_activity_status` (`activity_id`,`status`),
  KEY `idx_li_gift_card_cash_cred_batch_status` (`create_batch_id`,`status`),
  KEY `idx_li_gift_card_cash_cred_issue` (`issue_batch_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin ROW_FORMAT=DYNAMIC COMMENT='现金礼品卡卡密凭证';



ALTER TABLE `li_order`
    ADD COLUMN `deduction_method` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '抵扣方式' AFTER `payment_method`;

    -- 店铺流水/结算单：礼品卡平台补贴
ALTER TABLE `li_store_flow`
    ADD COLUMN `gift_card_subsidy_price` decimal(10,2) DEFAULT 0.00 COMMENT '礼品卡平台补贴金额' AFTER `site_coupon_commission`,
    ADD COLUMN `settlement_base_price` decimal(10,2) DEFAULT 0.00 COMMENT '商家结算计费基数' AFTER `final_price`;

ALTER TABLE `li_bill`
    ADD COLUMN `gift_card_subsidy` decimal(10,2) DEFAULT 0.00 COMMENT '礼品卡平台补贴（结算周期内）' AFTER `kanjia_refund_settlement_price`,
    ADD COLUMN `gift_card_refund_subsidy` decimal(10,2) DEFAULT 0.00 COMMENT '退货礼品卡平台补贴返还' AFTER `gift_card_subsidy`;