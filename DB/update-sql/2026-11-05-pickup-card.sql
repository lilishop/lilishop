-- 提货卡增量迁移：保留既有现金卡、凭证及订单，不更改菜单或角色授权。
-- @author Bulbasaur
-- @since 2026-11-05
-- DDL 可能隐式提交；上线前备份，逐项核验，不能整份事务回滚。
-- 依赖已有现金礼品卡表。旧凭证类型回填为 CASH；REDEEMED 为提货卡不可逆终态。

SET @pickup_ddl = IF(EXISTS(SELECT 1 FROM information_schema.columns WHERE table_schema = DATABASE() AND table_name = 'li_gift_card_cash_activity' AND column_name = 'pickup_mode'), 'SELECT 1', 'ALTER TABLE li_gift_card_cash_activity ADD COLUMN pickup_mode varchar(16) NULL COMMENT ''ONE任选一、ALL全选、ANY任选N种''');
PREPARE pickup_stmt FROM @pickup_ddl;
EXECUTE pickup_stmt;
DEALLOCATE PREPARE pickup_stmt;

SET @pickup_ddl = IF(EXISTS(SELECT 1 FROM information_schema.columns WHERE table_schema = DATABASE() AND table_name = 'li_gift_card_cash_activity' AND column_name = 'pickup_count'), 'SELECT 1', 'ALTER TABLE li_gift_card_cash_activity ADD COLUMN pickup_count int NULL COMMENT ''单次兑换SKU种数，每种一件''');
PREPARE pickup_stmt FROM @pickup_ddl;
EXECUTE pickup_stmt;
DEALLOCATE PREPARE pickup_stmt;

SET @pickup_ddl = IF(EXISTS(SELECT 1 FROM information_schema.columns WHERE table_schema = DATABASE() AND table_name = 'li_gift_card_cash_activity' AND column_name = 'pickup_sku_ids'), 'SELECT 1', 'ALTER TABLE li_gift_card_cash_activity ADD COLUMN pickup_sku_ids text NULL COMMENT ''可兑换SKU ID的JSON快照，制发卡后不可变''');
PREPARE pickup_stmt FROM @pickup_ddl;
EXECUTE pickup_stmt;
DEALLOCATE PREPARE pickup_stmt;

SET @pickup_ddl = IF(EXISTS(SELECT 1 FROM information_schema.columns WHERE table_schema = DATABASE() AND table_name = 'li_gift_card_cash_credential' AND column_name = 'card_type'), 'SELECT 1', 'ALTER TABLE li_gift_card_cash_credential ADD COLUMN card_type varchar(16) NOT NULL DEFAULT ''CASH'' COMMENT ''CASH现金卡、PICKUP提货卡快照''');
PREPARE pickup_stmt FROM @pickup_ddl;
EXECUTE pickup_stmt;
DEALLOCATE PREPARE pickup_stmt;

SET @pickup_ddl = IF(EXISTS(SELECT 1 FROM information_schema.columns WHERE table_schema = DATABASE() AND table_name = 'li_gift_card_cash_credential' AND column_name = 'pickup_trade_sn'), 'SELECT 1', 'ALTER TABLE li_gift_card_cash_credential ADD COLUMN pickup_trade_sn varchar(64) NULL COMMENT ''成功兑换交易号，取消不清空''');
PREPARE pickup_stmt FROM @pickup_ddl;
EXECUTE pickup_stmt;
DEALLOCATE PREPARE pickup_stmt;

-- 验证：旧现金卡应仍为 CASH；新提货卡面值与余额为0，可用性由激活/状态/有效期判断。
-- SELECT card_type, status, COUNT(*) FROM li_gift_card_cash_credential GROUP BY card_type, status;
-- SELECT id, pickup_mode, pickup_count, pickup_sku_ids FROM li_gift_card_cash_activity WHERE card_type='PICKUP';
-- 订单关联通过 li_order_gift_card_record 的 PICKUP_REDEEM 流水查询，不向 li_order 增加字段。
-- SELECT o.sn, o.goods_price, o.flow_price, r.credential_id FROM li_order o JOIN li_order_gift_card_record r ON r.order_sn=o.sn AND r.biz_type='PICKUP_REDEEM' AND r.delete_flag=0;
