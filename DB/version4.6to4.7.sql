/**
 * 财务中心菜单：平台报表、流水导出、商家本店汇总
 *
 * 管理端（li_menu）：
 *   - 财务中心（父菜单，挂在「店铺」下）
 *   - 平台经营报表 / 店铺结算汇总 / 支付方式汇总 / 结算台账 / 钱包流水
 *
 * 商家端（li_store_menu）：
 *   - 财务汇总（shop/finance/summary.vue）
 *
 * 执行后需在「权限管理」中为角色分配新菜单，并清除前端 localStorage 中的 menuData 缓存。
 */

-- 管理端：店铺 > 财务中心
INSERT INTO `li_menu` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`, `permission`)
SELECT 1996010100000000001, 'admin', NOW(), b'0', 'admin', NOW(), NULL, 'Main', 'md-stats', 1, 'financeCenter', '1367048084701315072', '/', 1.50, '财务中心', NULL, '/manager/finance/*'
FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM `li_menu` WHERE `id` = 1996010100000000001);

INSERT INTO `li_menu` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`, `permission`)
SELECT 1996010100000000002, 'admin', NOW(), b'0', 'admin', NOW(), NULL, 'finance/platform-report', 'md-alert', 2, 'finance-platform-report', '1996010100000000001', 'finance-platform-report', 0.00, '平台经营报表', NULL, '/manager/finance/report/*'
FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM `li_menu` WHERE `id` = 1996010100000000002);

INSERT INTO `li_menu` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`, `permission`)
SELECT 1996010100000000003, 'admin', NOW(), b'0', 'admin', NOW(), NULL, 'finance/store-settlement', 'md-alert', 2, 'finance-store-settlement', '1996010100000000001', 'finance-store-settlement', 1.00, '店铺结算汇总', NULL, '/manager/finance/report/*'
FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM `li_menu` WHERE `id` = 1996010100000000003);

INSERT INTO `li_menu` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`, `permission`)
SELECT 1996010100000000004, 'admin', NOW(), b'0', 'admin', NOW(), NULL, 'finance/payment-method', 'md-alert', 2, 'finance-payment-method', '1996010100000000001', 'finance-payment-method', 2.00, '支付方式汇总', NULL, '/manager/finance/report/*'
FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM `li_menu` WHERE `id` = 1996010100000000004);

INSERT INTO `li_menu` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`, `permission`)
SELECT 1996010100000000005, 'admin', NOW(), b'0', 'admin', NOW(), NULL, 'finance/settlement-ledger', 'md-alert', 2, 'finance-settlement-ledger', '1996010100000000001', 'finance-settlement-ledger', 3.00, '结算台账', NULL, '/manager/finance/report/*'
FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM `li_menu` WHERE `id` = 1996010100000000005);

INSERT INTO `li_menu` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `front_component`, `permission`)
SELECT 1996010100000000006, 'admin', NOW(), b'0', 'admin', NOW(), NULL, 'finance/walletLog', 'md-alert', 2, 'finance-wallet-log', '1996010100000000001', 'finance-wallet-log', 4.00, '钱包流水', NULL, '/manager/finance/wallet-log/*,/manager/wallet/log*'
FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM `li_menu` WHERE `id` = 1996010100000000006);

-- 商家端：财务 > 本店汇总
INSERT INTO `li_store_menu` (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `permission`)
SELECT 1996010100000000101, 'admin', NOW(), b'0', 'admin', NOW(), NULL, 'shop/finance/summary', 'ios-american-football', 2, 'shop-finance-summary', '1357873097859923969', 'shop-finance-summary', '3.13', '财务汇总', '/store/finance/*'
FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM `li_store_menu` WHERE `id` = 1996010100000000101);
