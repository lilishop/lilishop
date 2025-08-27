-- 针对WHERE条件、排序和分组的组合索引
CREATE INDEX idx_order_delete_flag_create_time_id_sn ON li_order (delete_flag, create_time DESC, id DESC, sn);

-- 针对WHERE条件和连接条件的组合索引
CREATE INDEX idx_order_status_delete_flag_sn ON li_order (order_status, delete_flag, sn);

-- 针对连接条件的索引
CREATE INDEX idx_order_item_order_sn ON li_order_item (order_sn);

-- 针对过滤条件、排序字段的组合索引
CREATE INDEX idx_li_member_disabled_create_time ON li_member (disabled, create_time DESC);

-- 针对过滤条件、排序字段的组合索引
CREATE INDEX idx_li_goods_delete_flag_create_time ON li_goods (delete_flag, create_time DESC);