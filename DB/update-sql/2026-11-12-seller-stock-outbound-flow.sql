-- 商家端出库单与库存流水表结构及菜单初始化脚本；可重复执行。
-- @author Bulbasaur
-- @since 2026-11-12

-- 1. 出库单主表
CREATE TABLE IF NOT EXISTS `li_stock_outbound` (
  `id` bigint NOT NULL COMMENT 'ID',
  `create_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '创建者',
  `create_time` datetime(6) NULL DEFAULT NULL COMMENT '创建时间',
  `delete_flag` bit(1) NULL DEFAULT b'0' COMMENT '删除标志 true/false 删除/未删除',
  `update_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '更新者',
  `update_time` datetime(6) NULL DEFAULT NULL COMMENT '更新时间',
  `outbound_sn` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '出库单号',
  `store_id` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '店铺ID',
  `store_name` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '店铺名称',
  `status` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL DEFAULT 'DRAFT' COMMENT '出库单状态：DRAFT草稿、AUDITING待审核、COMPLETED已完成、REJECTED已驳回、CANCELLED已作废',
  `reason_id` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '出库原因ID',
  `reason_name` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '出库原因名称',
  `total_quantity` int NOT NULL DEFAULT 0 COMMENT '出库商品总数量',
  `remark` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '出库备注',
  `audit_by` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '审核人ID/名称',
  `audit_time` datetime NULL DEFAULT NULL COMMENT '审核时间',
  `audit_remark` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '审核备注/驳回原因',
  `complete_time` datetime NULL DEFAULT NULL COMMENT '出库完成时间',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE INDEX `uk_stock_outbound_sn`(`outbound_sn` ASC) USING BTREE,
  INDEX `idx_store_status_create`(`store_id` ASC, `status` ASC, `create_time` ASC) USING BTREE,
  INDEX `idx_store_create`(`store_id` ASC, `create_time` ASC) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_bin COMMENT = '商家出库单' ROW_FORMAT = DYNAMIC;

-- 2. 出库单明细表
CREATE TABLE IF NOT EXISTS `li_stock_outbound_item` (
  `id` bigint NOT NULL COMMENT 'ID',
  `create_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '创建者',
  `create_time` datetime(6) NULL DEFAULT NULL COMMENT '创建时间',
  `delete_flag` bit(1) NULL DEFAULT b'0' COMMENT '删除标志 true/false 删除/未删除',
  `update_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '更新者',
  `update_time` datetime(6) NULL DEFAULT NULL COMMENT '更新时间',
  `outbound_id` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '出库单ID',
  `goods_id` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '商品ID',
  `sku_id` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT 'SKU ID',
  `goods_name` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '商品名称',
  `specs` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '规格快照JSON',
  `goods_unit` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '计量单位',
  `quantity` int NOT NULL COMMENT '出库数量',
  `item_remark` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '明细备注',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `idx_outbound_id`(`outbound_id` ASC) USING BTREE,
  INDEX `idx_sku_id`(`sku_id` ASC) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_bin COMMENT = '商家出库单明细' ROW_FORMAT = DYNAMIC;

-- 3. 全局库存流水表
CREATE TABLE IF NOT EXISTS `li_stock_flow` (
  `id` bigint NOT NULL COMMENT 'ID',
  `create_time` datetime(6) NOT NULL COMMENT '创建时间',
  `store_id` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '店铺ID',
  `goods_id` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '商品ID',
  `sku_id` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT 'SKU ID',
  `goods_name` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '商品名称',
  `specs` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '规格快照',
  `flow_type` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '变动类型：PROCUREMENT_INBOUND采购入库、STOCK_OUTBOUND出库单、DAMAGE报损、INVENTORY_COUNT盘点调整、MANUAL_ADJUST手工调整、ORDER_PAID订单销售、ORDER_CANCEL订单取消回滚',
  `direction` varchar(8) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '变动方向：IN入库、OUT出库',
  `quantity` int NOT NULL COMMENT '变动数量绝对值(>0)',
  `before_stock` int NOT NULL COMMENT '变动前库存',
  `after_stock` int NOT NULL COMMENT '变动后库存',
  `biz_sn` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '关联业务单号/调整批次号',
  `operator` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '操作人标识',
  `remark` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '备注说明',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE INDEX `uk_flow`(`flow_type` ASC, `biz_sn` ASC, `sku_id` ASC) USING BTREE,
  INDEX `idx_store_create`(`store_id` ASC, `create_time` ASC) USING BTREE,
  INDEX `idx_store_sku_create`(`store_id` ASC, `sku_id` ASC, `create_time` ASC) USING BTREE,
  INDEX `idx_store_type_create`(`store_id` ASC, `flow_type` ASC, `create_time` ASC) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_bin COMMENT = '库存流水' ROW_FORMAT = DYNAMIC;

-- 4. 商家端菜单配置
START TRANSACTION;

-- 4.1 一级分组：库存管理（归属商品模块 1348810750596767744）
INSERT INTO `li_store_menu`
    (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`,
     `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `permission`)
SELECT 202611121000, 'Bulbasaur', NOW(), b'0', 'Bulbasaur', NOW(),
       '商家库存管理分组', 'Main', 'md-filing', 1,
       'stockManage', '1348810750596767744', '/', '5.5', '库存管理', ''
WHERE NOT EXISTS (SELECT 1 FROM `li_store_menu` WHERE `name` = 'stockManage' AND `delete_flag` = b'0');

-- 4.2 二级页面：出库单
INSERT INTO `li_store_menu`
    (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`,
     `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `permission`)
SELECT 202611121001, 'Bulbasaur', NOW(), b'0', 'Bulbasaur', NOW(),
       '商家出库单管理', 'stock/outbound/list', NULL, 2,
       'stockOutboundList', '202611121000', 'stockOutboundList', '1.0', '出库单', '/store/stock/outbound*'
WHERE NOT EXISTS (SELECT 1 FROM `li_store_menu` WHERE `name` = 'stockOutboundList' AND `delete_flag` = b'0');

-- 4.3 二级页面：库存流水
INSERT INTO `li_store_menu`
    (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`,
     `description`, `front_route`, `icon`, `level`, `name`, `parent_id`, `path`, `sort_order`, `title`, `permission`)
SELECT 202611121002, 'Bulbasaur', NOW(), b'0', 'Bulbasaur', NOW(),
       '商家库存变动流水台账', 'stock/flow/list', NULL, 2,
       'stockFlowList', '202611121000', 'stockFlowList', '2.0', '库存流水', '/store/stock/flow*'
WHERE NOT EXISTS (SELECT 1 FROM `li_store_menu` WHERE `name` = 'stockFlowList' AND `delete_flag` = b'0');

-- 4.4 继承已有角色绑定权限（继承 procurementStockList 分组及采购单的授权范围）
INSERT INTO `li_store_menu_role`
    (`id`, `create_by`, `create_time`, `delete_flag`, `update_by`, `update_time`, `role_id`, `menu_id`, `store_id`, `is_super`)
SELECT UUID_SHORT(), 'Bulbasaur', NOW(), b'0', 'Bulbasaur', NOW(), bindings.role_id,
       CAST(target.id AS CHAR), bindings.store_id, bindings.is_super
FROM `li_store_menu_role` bindings
INNER JOIN `li_store_menu` source ON CAST(source.id AS CHAR) = bindings.menu_id
CROSS JOIN `li_store_menu` target
WHERE source.name IN ('procurementStockList', 'procurementInboundList')
  AND target.name IN ('stockManage', 'stockOutboundList', 'stockFlowList')
  AND source.delete_flag = b'0' AND target.delete_flag = b'0' AND bindings.delete_flag = b'0'
  AND NOT EXISTS (
      SELECT 1 FROM `li_store_menu_role` existing
      WHERE existing.menu_id = CAST(target.id AS CHAR) AND existing.role_id = bindings.role_id
        AND existing.store_id <=> bindings.store_id AND existing.delete_flag = b'0'
  );

COMMIT;
