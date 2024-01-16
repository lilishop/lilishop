
/**
  交易唤醒表，增加交易流水详情
 */
ALTER TABLE li_order_item ADD `is_refund` varchar(255) DEFAULT NULL COMMENT '是否退款';

/**
  交易表增加订单状态字段
 */
ALTER TABLE li_order_item ADD `refund_price` decimal(10,2) DEFAULT NULL COMMENT '退款金额';
