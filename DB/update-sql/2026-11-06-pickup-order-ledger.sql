-- 将提货订单关联和取消幂等迁移至现有礼品卡流水，li_order 不保留提货附加字段。
-- @author Bulbasaur
-- @since 2026-11-06
-- 已运行旧版提货迁移的环境：备份后暂停兑换、订单取消和库存消息处理，再执行本脚本并发布新代码。
-- 全新环境：先执行 2026-11-05-pickup-card.sql，再执行本脚本；不存在旧订单列时自动跳过回填和删除。
-- 回填不修改订单、卡权益或库存；旧库存已释放标志转为零金额 PICKUP_CANCEL 处理记录，避免重复补库存。
-- DDL 可能隐式提交，不能将整份脚本作为一个事务回滚。未在真实数据库执行验证。

SET @pickup_has_card = EXISTS(SELECT 1 FROM information_schema.columns WHERE table_schema=DATABASE() AND table_name='li_order' AND column_name='pickup_card_id');
SET @pickup_has_stock = EXISTS(SELECT 1 FROM information_schema.columns WHERE table_schema=DATABASE() AND table_name='li_order' AND column_name='pickup_stock_released');

-- 缺少原兑换记录时，使用旧订单关联与成交快照补齐；不覆盖已有记录。
SET @pickup_order_sql = IF(@pickup_has_card, 'INSERT INTO li_order_gift_card_record (id,create_time,update_time,delete_flag,order_sn,trade_sn,member_id,credential_id,card_no,deduct_amount,change_amount,balance_after,flow_type,biz_type,biz_sn) SELECT REPLACE(UUID(),''-'',''''),NOW(),NOW(),0,o.sn,o.trade_sn,o.member_id,o.pickup_card_id,c.card_no,ROUND(COALESCE(o.goods_price,0),2),0,0,''DECREASE'',''PICKUP_REDEEM'',o.sn FROM li_order o LEFT JOIN li_gift_card_cash_credential c ON c.id=o.pickup_card_id WHERE o.pickup_card_id IS NOT NULL AND o.pickup_card_id<>'''' AND NOT EXISTS(SELECT 1 FROM li_order_gift_card_record r WHERE r.order_sn=o.sn AND r.biz_type=''PICKUP_REDEEM'' AND r.delete_flag=0)', 'SELECT 1');
PREPARE pickup_order_stmt FROM @pickup_order_sql;
EXECUTE pickup_order_stmt;
DEALLOCATE PREPARE pickup_order_stmt;

SELECT COUNT(*) INTO @pickup_duplicate_orders FROM (SELECT order_sn FROM li_order_gift_card_record WHERE biz_type='PICKUP_REDEEM' AND delete_flag=0 GROUP BY order_sn HAVING COUNT(*)>1) pickup_duplicates;

-- 保留原兑换流水，追加已完成取消处理的标志记录，禁止据此退款或回卡；重复关联存在时不生成标志。
SET @pickup_order_sql = IF(@pickup_has_stock AND @pickup_duplicate_orders=0, 'INSERT INTO li_order_gift_card_record (id,create_time,update_time,delete_flag,order_sn,trade_sn,member_id,credential_id,card_no,deduct_amount,change_amount,balance_after,flow_type,biz_type,biz_sn) SELECT REPLACE(UUID(),''-'',''''),NOW(),NOW(),0,o.sn,r.trade_sn,r.member_id,r.credential_id,r.card_no,0,0,0,''DECREASE'',''PICKUP_CANCEL'',o.sn FROM li_order o JOIN li_order_gift_card_record r ON r.order_sn=o.sn AND r.biz_type=''PICKUP_REDEEM'' AND r.delete_flag=0 WHERE o.pickup_stock_released=1 AND NOT EXISTS(SELECT 1 FROM li_order_gift_card_record m WHERE m.order_sn=o.sn AND m.biz_type=''PICKUP_CANCEL'' AND m.delete_flag=0)', 'SELECT 1');
PREPARE pickup_order_stmt FROM @pickup_order_sql;
EXECUTE pickup_order_stmt;
DEALLOCATE PREPARE pickup_order_stmt;

-- 删除旧列前验证关联和取消标志均已保留，任意校验失败均保留两列。
SET @pickup_missing_relation=0;
SET @pickup_missing_cancel=0;
SET @pickup_order_sql = IF(@pickup_has_card, 'SELECT COUNT(*) INTO @pickup_missing_relation FROM li_order o WHERE o.pickup_card_id IS NOT NULL AND o.pickup_card_id<>'''' AND NOT EXISTS(SELECT 1 FROM li_order_gift_card_record r WHERE r.order_sn=o.sn AND r.credential_id=o.pickup_card_id AND r.biz_type=''PICKUP_REDEEM'' AND r.delete_flag=0)', 'SELECT 1');
PREPARE pickup_order_stmt FROM @pickup_order_sql;
EXECUTE pickup_order_stmt;
DEALLOCATE PREPARE pickup_order_stmt;

SET @pickup_order_sql = IF(@pickup_has_stock, 'SELECT COUNT(*) INTO @pickup_missing_cancel FROM li_order o WHERE o.pickup_stock_released=1 AND NOT EXISTS(SELECT 1 FROM li_order_gift_card_record r WHERE r.order_sn=o.sn AND r.biz_type=''PICKUP_CANCEL'' AND r.delete_flag=0)', 'SELECT 1');
PREPARE pickup_order_stmt FROM @pickup_order_sql;
EXECUTE pickup_order_stmt;
DEALLOCATE PREPARE pickup_order_stmt;

SET @pickup_order_safe=(@pickup_missing_relation=0 AND @pickup_missing_cancel=0 AND @pickup_duplicate_orders=0);
SELECT @pickup_missing_relation AS missing_relations,@pickup_missing_cancel AS missing_cancel_markers,@pickup_duplicate_orders AS duplicate_relations,@pickup_order_safe AS safe_to_drop_legacy_columns;

SET @pickup_order_sql = IF(@pickup_has_card AND @pickup_order_safe, 'ALTER TABLE li_order DROP COLUMN pickup_card_id', 'SELECT ''Legacy card column absent or verification blocked cleanup'' AS migration_status');
PREPARE pickup_order_stmt FROM @pickup_order_sql;
EXECUTE pickup_order_stmt;
DEALLOCATE PREPARE pickup_order_stmt;

SET @pickup_order_sql = IF(@pickup_has_stock AND @pickup_order_safe, 'ALTER TABLE li_order DROP COLUMN pickup_stock_released', 'SELECT ''Legacy stock column absent or verification blocked cleanup'' AS migration_status');
PREPARE pickup_order_stmt FROM @pickup_order_sql;
EXECUTE pickup_order_stmt;
DEALLOCATE PREPARE pickup_order_stmt;

-- 非唯一索引只改善原兑换查询、批量回显及取消行锁定位，不修改既有角色、菜单和财务数据。
SET @pickup_order_sql = IF(NOT EXISTS(SELECT 1 FROM information_schema.statistics WHERE table_schema=DATABASE() AND table_name='li_order_gift_card_record' AND index_name='idx_pickup_order_flow'), 'ALTER TABLE li_order_gift_card_record ADD INDEX idx_pickup_order_flow (order_sn,biz_type,delete_flag)', 'SELECT 1');
PREPARE pickup_order_stmt FROM @pickup_order_sql;
EXECUTE pickup_order_stmt;
DEALLOCATE PREPARE pickup_order_stmt;

-- 验证：以下重复关联应为空；取消后原兑换记录应仍存在。
-- SELECT order_sn,COUNT(*) FROM li_order_gift_card_record WHERE biz_type='PICKUP_REDEEM' AND delete_flag=0 GROUP BY order_sn HAVING COUNT(*)>1;
-- SELECT order_sn,biz_type,credential_id,change_amount FROM li_order_gift_card_record WHERE biz_type IN ('PICKUP_REDEEM','PICKUP_CANCEL') ORDER BY order_sn,biz_type;
