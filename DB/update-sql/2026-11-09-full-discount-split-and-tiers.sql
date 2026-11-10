-- 满额活动拆分为满减活动、满折活动、满赠活动并支持三级阶梯升级脚本
-- 包含：li_full_discount 表字段扩展、存量单门槛数据向阶梯结构迁移、商家端与管理端菜单拆分
-- 说明：包含 DDL 语句（ALTER TABLE），MySQL 执行 DDL 会隐式提交；脚本整体支持重复执行（幂等）。
-- @author Bulbasaur
-- @since 2026-11-09

-- 1. 扩展 li_full_discount 表字段（幂等添加 activity_type 与 rules_json）
SET @col_activity_type_exists = (
    SELECT COUNT(1)
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE()
      AND TABLE_NAME = 'li_full_discount'
      AND COLUMN_NAME = 'activity_type'
);
SET @sql_add_activity_type = IF(
    @col_activity_type_exists = 0,
    'ALTER TABLE `li_full_discount` ADD COLUMN `activity_type` VARCHAR(32) DEFAULT ''FULL_MINUS'' COMMENT ''活动子类型：FULL_MINUS-满减，FULL_RATE-满折，FULL_GIFT-满赠''',
    'SELECT 1'
);
PREPARE stmt_activity_type FROM @sql_add_activity_type;
EXECUTE stmt_activity_type;
DEALLOCATE PREPARE stmt_activity_type;

SET @col_rules_json_exists = (
    SELECT COUNT(1)
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE()
      AND TABLE_NAME = 'li_full_discount'
      AND COLUMN_NAME = 'rules_json'
);
SET @sql_add_rules_json = IF(
    @col_rules_json_exists = 0,
    'ALTER TABLE `li_full_discount` ADD COLUMN `rules_json` TEXT DEFAULT NULL COMMENT ''阶梯规则JSON列表（最多3级）''',
    'SELECT 1'
);
PREPARE stmt_rules_json FROM @sql_add_rules_json;
EXECUTE stmt_rules_json;
DEALLOCATE PREPARE stmt_rules_json;

-- 2. 存量历史数据幂等迁移：按原有标志位回填 activity_type 及单阶梯 rules_json
UPDATE `li_full_discount`
SET `activity_type` = CASE
    WHEN `full_rate_flag` = b'1' THEN 'FULL_RATE'
    WHEN (`full_minus_flag` IS NULL OR `full_minus_flag` = b'0') AND `gift_flag` = b'1' THEN 'FULL_GIFT'
    ELSE 'FULL_MINUS'
END
WHERE `activity_type` IS NULL OR `activity_type` = '';

UPDATE `li_full_discount`
SET `rules_json` = CASE
    WHEN `activity_type` = 'FULL_RATE' THEN
        JSON_ARRAY(JSON_OBJECT('fullMoney', COALESCE(`full_money`, 0), 'fullRate', COALESCE(`full_rate`, 9.0)))
    WHEN `activity_type` = 'FULL_GIFT' AND `gift_id` IS NOT NULL AND `gift_id` <> '' THEN
        JSON_ARRAY(JSON_OBJECT('fullMoney', COALESCE(`full_money`, 0), 'giftItems', JSON_ARRAY(JSON_OBJECT('skuId', `gift_id`, 'num', 1))))
    ELSE
        JSON_ARRAY(JSON_OBJECT('fullMoney', COALESCE(`full_money`, 0), 'fullMinus', COALESCE(`full_minus`, 0)))
END
WHERE (`rules_json` IS NULL OR `rules_json` = '')
  AND `full_money` IS NOT NULL;

-- 3. 拆分管理端菜单 li_menu（将原满额活动更新为满减活动，同级新增满折活动、满赠活动；不触碰 li_role_menu）
UPDATE `li_menu`
SET `title` = '满减活动',
    `description` = '满减活动列表',
    `update_by` = 'Bulbasaur',
    `update_time` = NOW()
WHERE (`front_route` = 'promotions/full-discount/full-discount' OR `name` = 'full-cut-activity' OR `name` = 'full-discount')
  AND `delete_flag` = b'0';

INSERT INTO `li_menu`
    (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`,
     `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`,
     `title`, `front_component`, `permission`)
SELECT 2121950000000000001, 'Bulbasaur', NOW(), b'0', 'Bulbasaur', NOW(),
       '满折活动列表', 'promotions/full-discount/full-rate', source.icon, source.level,
       'manager-full-rate', source.parent_id, 'full-rate',
       CAST(COALESCE(source.sort_order, 0) AS DECIMAL(10, 3)) + 0.01,
       '满折活动', source.front_component, source.permission
FROM `li_menu` source
WHERE (source.front_route = 'promotions/full-discount/full-discount' OR source.name IN ('full-cut-activity', 'full-discount'))
  AND source.delete_flag = b'0'
  AND NOT EXISTS (SELECT 1 FROM `li_menu` target WHERE target.name = 'manager-full-rate' AND target.delete_flag = b'0')
LIMIT 1;

INSERT INTO `li_menu`
    (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`,
     `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`,
     `title`, `front_component`, `permission`)
SELECT 2121950000000000002, 'Bulbasaur', NOW(), b'0', 'Bulbasaur', NOW(),
       '满赠活动列表', 'promotions/full-discount/full-gift', source.icon, source.level,
       'manager-full-gift', source.parent_id, 'full-gift',
       CAST(COALESCE(source.sort_order, 0) AS DECIMAL(10, 3)) + 0.02,
       '满赠活动', source.front_component, source.permission
FROM `li_menu` source
WHERE (source.front_route = 'promotions/full-discount/full-discount' OR source.name IN ('full-cut-activity', 'full-discount'))
  AND source.delete_flag = b'0'
  AND NOT EXISTS (SELECT 1 FROM `li_menu` target WHERE target.name = 'manager-full-gift' AND target.delete_flag = b'0')
LIMIT 1;

-- 4. 拆分商家端菜单 li_store_menu（将原满额活动更新为满减活动，同级新增满折活动、满赠活动）
UPDATE `li_store_menu`
SET `title` = '满减活动',
    `description` = '满减活动管理',
    `update_by` = 'Bulbasaur',
    `update_time` = NOW()
WHERE (`front_route` = 'promotion/full-discount/full-discount' OR `name` IN ('full-cut', 'full-discount'))
  AND `delete_flag` = b'0';

INSERT INTO `li_store_menu`
    (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`,
     `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`,
     `title`, `permission`)
SELECT 2121950000000000011, 'Bulbasaur', NOW(), b'0', 'Bulbasaur', NOW(),
       '满折活动管理', 'promotion/full-discount/full-rate', source.icon, source.level,
       'store-full-rate', source.parent_id, 'full-rate',
       CAST(COALESCE(source.sort_order, '0') AS DECIMAL(10, 3)) + 0.01,
       '满折活动', source.permission
FROM `li_store_menu` source
WHERE (source.front_route = 'promotion/full-discount/full-discount' OR source.name IN ('full-cut', 'full-discount'))
  AND source.delete_flag = b'0'
  AND NOT EXISTS (SELECT 1 FROM `li_store_menu` target WHERE target.name = 'store-full-rate' AND target.delete_flag = b'0')
LIMIT 1;

INSERT INTO `li_store_menu`
    (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`,
     `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`,
     `title`, `permission`)
SELECT 2121950000000000012, 'Bulbasaur', NOW(), b'0', 'Bulbasaur', NOW(),
       '满赠活动管理', 'promotion/full-discount/full-gift', source.icon, source.level,
       'store-full-gift', source.parent_id, 'full-gift',
       CAST(COALESCE(source.sort_order, '0') AS DECIMAL(10, 3)) + 0.02,
       '满赠活动', source.permission
FROM `li_store_menu` source
WHERE (source.front_route = 'promotion/full-discount/full-discount' OR source.name IN ('full-cut', 'full-discount'))
  AND source.delete_flag = b'0'
  AND NOT EXISTS (SELECT 1 FROM `li_store_menu` target WHERE target.name = 'store-full-gift' AND target.delete_flag = b'0')
LIMIT 1;
