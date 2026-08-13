-- =============================================================================
-- 商家发货改造：li_order_package 扩展字段
-- 作者：Mike
-- 创建：2026-08-13
-- =============================================================================
-- 新环境若已有完整 DDL 可跳过；已有表执行「升级脚本」。
-- =============================================================================

-- -----------------------------------------------------------------------------
-- 升级脚本（可重复执行，列已存在时忽略错误）
-- -----------------------------------------------------------------------------
ALTER TABLE `li_order_package`
  ADD COLUMN `fulfillment_mode` varchar(32) NOT NULL DEFAULT 'SPLIT'
    COMMENT 'WHOLE|SPLIT|NO_DELIVERY' AFTER `status`;

ALTER TABLE `li_order_package`
  ADD COLUMN `delivery_remark` varchar(500) DEFAULT NULL
    COMMENT '无需配送说明' AFTER `fulfillment_mode`;

ALTER TABLE `li_order_package`
  ADD COLUMN `logistics_time` datetime DEFAULT NULL
    COMMENT '本批次发货时间' AFTER `delivery_remark`;

ALTER TABLE `li_order_package`
  MODIFY COLUMN `logistics_no` varchar(255) DEFAULT NULL;

ALTER TABLE `li_order_package`
  MODIFY COLUMN `logistics_code` varchar(64) DEFAULT NULL;

ALTER TABLE `li_order_package`
  MODIFY COLUMN `logistics_name` varchar(128) DEFAULT NULL;

-- -----------------------------------------------------------------------------
-- 历史数据回填（可选，上线前统计后人工执行）
-- -----------------------------------------------------------------------------
-- UPDATE li_order_item oi
-- INNER JOIN li_order o ON oi.order_sn = o.sn
-- SET oi.deliver_number = oi.num
-- WHERE o.deliver_status IN ('DELIVERED', 'COMPLETED')
--   AND (oi.deliver_number IS NULL OR oi.deliver_number = 0);
