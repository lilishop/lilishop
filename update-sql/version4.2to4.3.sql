/** 添加结算单积分、砍价结算价信息**/
ALTER TABLE li_bill ADD point_settlement_price double DEFAULT 0.00 COMMENT '积分商品结算金额 ';
ALTER TABLE li_bill ADD kanjia_settlement_price double DEFAULT 0.00 COMMENT '砍价商品结算金额';
ALTER TABLE li_store_flow ADD point_settlement_price double DEFAULT 0.00 COMMENT '积分商品结算金额';
ALTER TABLE li_store_flow ADD kanjia_settlement_price double DEFAULT 0.00 COMMENT '砍价商品结算金额';