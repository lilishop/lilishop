/** 新增已退货数量 **/
ALTER TABLE li_order_item ADD return_goods_number int DEFAULT 0 COMMENT '已退货数量 ';
ALTER TABLE li_order_item ADD is_goods_number int DEFAULT 0 COMMENT '正在进行售后的商品数量 ';