/** 新增已退货数量 **/
ALTER TABLE li_order_item ADD return_goods_number int DEFAULT 0 COMMENT '退货数量 ';
ALTER TABLE li_order_item ADD identification_status varchar(255) COMMENT '标识是否被4.2.4版本处理过的订单';