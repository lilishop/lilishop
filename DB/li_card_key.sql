-- 作者：Mike
-- 日期：2026-07-31
-- 电子卡券（卡密商品）卡池表
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
  `card_secret`     varchar(512)  NOT NULL COMMENT '卡密(AES密文)',
  `status`          varchar(32)   NOT NULL DEFAULT 'UNUSED' COMMENT 'UNUSED/ALLOCATED/VOIDED',
  `order_sn`        varchar(64)   DEFAULT NULL COMMENT '主订单号',
  `order_item_sn`   varchar(64)   DEFAULT NULL COMMENT '订单项号',
  `member_id`       varchar(32)   DEFAULT NULL COMMENT '买家ID',
  `allocated_time`  datetime      DEFAULT NULL COMMENT '发卡时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_sku_card_no` (`sku_id`, `card_no`),
  KEY `idx_sku_status` (`sku_id`, `status`, `delete_flag`),
  KEY `idx_store_sku` (`store_id`, `sku_id`),
  KEY `idx_order_sn` (`order_sn`),
  KEY `idx_order_item_sn` (`order_item_sn`),
  KEY `idx_member_id` (`member_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='电子卡券卡密池';
