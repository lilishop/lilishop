ALTER TABLE `li_order`
    ADD COLUMN `seller_remark` varchar(255) NULL COMMENT '商家订单备注' AFTER `remark`;