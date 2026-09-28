-- =============================================================================
-- 电子卡券（卡密商品）卡池表 li_card_key
-- 作者：Mike
-- 创建：2026-07-31
-- 更新：2026-08-02（v4.9 全量促销：拼团预占状态 RESERVED）
-- =============================================================================
--
-- 状态语义（CardKeyStatusEnum）：
--   UNUSED    在库可售；可售库存 = COUNT(status='UNUSED' AND delete_flag=0)
--   RESERVED  拼团等场景 PAID 后预占，已绑定 order_sn/order_item_sn/member_id，
--             allocated_time 仍为 NULL；成团转 ALLOCATED，失败释放回 UNUSED
--   ALLOCATED 已售且已交付；allocated_time 写入发卡时间
--   VOIDED    作废，不可回收到 UNUSED
--
-- 新环境：直接执行「一、建表」。
-- 已有 li_card_key 表：执行「二、升级脚本」（可重复执行，索引/注释变更幂等）。
-- =============================================================================

-- -----------------------------------------------------------------------------
-- 一、建表（新库 / 首次部署）
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS `li_card_key` (
  `id`              varchar(32)   NOT NULL COMMENT '主键',
  `create_by`       varchar(64)   DEFAULT NULL COMMENT '创建者',
  `create_time`     datetime      DEFAULT NULL COMMENT '创建时间',
  `update_by`       varchar(64)   DEFAULT NULL COMMENT '更新者',
  `update_time`     datetime      DEFAULT NULL COMMENT '更新时间',
  `delete_flag`     bit(1)        DEFAULT b'0' COMMENT '删除标志 false/true',
  `sku_id`          varchar(32)   NOT NULL COMMENT 'SKU ID',
  `goods_id`        varchar(32)   NOT NULL COMMENT '商品 SPU ID',
  `store_id`        varchar(32)   NOT NULL COMMENT '店铺 ID',
  `card_no`         varchar(128)  NOT NULL COMMENT '卡号',
  `card_secret`     varchar(512)  NOT NULL COMMENT '卡密(明文)',
  `status`          varchar(32)   NOT NULL DEFAULT 'UNUSED' COMMENT 'UNUSED/RESERVED/ALLOCATED/VOIDED',
  `order_sn`        varchar(64)   DEFAULT NULL COMMENT '绑定订单号（RESERVED/ALLOCATED 时写入）',
  `order_item_sn`   varchar(64)   DEFAULT NULL COMMENT '绑定订单项号（RESERVED/ALLOCATED 时写入）',
  `member_id`       varchar(32)   DEFAULT NULL COMMENT '买家 ID（RESERVED/ALLOCATED 时写入）',
  `allocated_time`  datetime      DEFAULT NULL COMMENT '发卡时间（仅 ALLOCATED 时写入；RESERVED 预占时为 NULL）',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_sku_card_no` (`sku_id`, `card_no`),
  KEY `idx_sku_status` (`sku_id`, `status`, `delete_flag`),
  KEY `idx_store_sku` (`store_id`, `sku_id`),
  KEY `idx_order_sn` (`order_sn`),
  KEY `idx_order_item_sn` (`order_item_sn`),
  KEY `idx_order_status` (`order_sn`, `status`, `delete_flag`),
  KEY `idx_member_id` (`member_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='电子卡券卡密池';

-- -----------------------------------------------------------------------------
-- 二、升级脚本（v4.9：全量促销 / 拼团 RESERVED 预占）
-- 适用：已存在 li_card_key 且 status 注释仍为 UNUSED/ALLOCATED/VOIDED 的环境
-- -----------------------------------------------------------------------------

-- 2.1 扩展 status 枚举说明（无数据迁移；历史行仍为 UNUSED/ALLOCATED/VOIDED）
ALTER TABLE `li_card_key`
  MODIFY COLUMN `status` varchar(32) NOT NULL DEFAULT 'UNUSED'
    COMMENT 'UNUSED/RESERVED/ALLOCATED/VOIDED';

-- 2.2 订单绑定字段注释（RESERVED 复用 order_sn / order_item_sn / member_id，不新增列）
ALTER TABLE `li_card_key`
  MODIFY COLUMN `order_sn` varchar(64) DEFAULT NULL
    COMMENT '绑定订单号（RESERVED/ALLOCATED 时写入）';

ALTER TABLE `li_card_key`
  MODIFY COLUMN `order_item_sn` varchar(64) DEFAULT NULL
    COMMENT '绑定订单项号（RESERVED/ALLOCATED 时写入）';

ALTER TABLE `li_card_key`
  MODIFY COLUMN `member_id` varchar(32) DEFAULT NULL
    COMMENT '买家 ID（RESERVED/ALLOCATED 时写入）';

ALTER TABLE `li_card_key`
  MODIFY COLUMN `allocated_time` datetime DEFAULT NULL
    COMMENT '发卡时间（仅 ALLOCATED 时写入；RESERVED 预占时为 NULL）';

-- 2.3 拼团预占/释放：按 order_sn + status 行锁（selectReservedByOrder）
-- 若索引已存在会报错，可忽略或先 DROP INDEX idx_order_status
ALTER TABLE `li_card_key`
  ADD INDEX `idx_order_status` (`order_sn`, `status`, `delete_flag`);

-- -----------------------------------------------------------------------------
-- 三、可选校验（升级后人工执行）
-- -----------------------------------------------------------------------------
-- 可售库存（应与 li_goods_sku.quantity 一致，仅统计 UNUSED）：
-- SELECT sku_id, COUNT(*) AS unused_count
-- FROM li_card_key
-- WHERE status = 'UNUSED' AND delete_flag = b'0'
-- GROUP BY sku_id;
--
-- 拼团预占中卡密（商家卡池 RESERVED 统计）：
-- SELECT sku_id, COUNT(*) AS reserved_count
-- FROM li_card_key
-- WHERE status = 'RESERVED' AND delete_flag = b'0'
-- GROUP BY sku_id;
