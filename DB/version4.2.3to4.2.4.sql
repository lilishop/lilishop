/** 新增已退货数量 **/
ALTER TABLE li_order_item ADD return_goods_number int DEFAULT 0 COMMENT '退货数量 ';