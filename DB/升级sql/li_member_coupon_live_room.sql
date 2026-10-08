-- 会员优惠券增加直播间来源字段。买家端列表会查询整行，缺列时接口会返回「服务器繁忙」。
-- 可重复执行。

SET @db = DATABASE();

SET @sql = (
    SELECT IF(
        EXISTS(
            SELECT 1 FROM information_schema.COLUMNS
            WHERE TABLE_SCHEMA = @db AND TABLE_NAME = 'li_member_coupon' AND COLUMN_NAME = 'live_room_id'
        ),
        'SELECT 1',
        'ALTER TABLE li_member_coupon ADD COLUMN live_room_id varchar(64) NULL COMMENT ''领取来源直播间ID'' AFTER member_coupon_status'
    )
);
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @sql = (
    SELECT IF(
        EXISTS(
            SELECT 1 FROM information_schema.COLUMNS
            WHERE TABLE_SCHEMA = @db AND TABLE_NAME = 'li_member_coupon' AND COLUMN_NAME = 'live_room_name'
        ),
        'SELECT 1',
        'ALTER TABLE li_member_coupon ADD COLUMN live_room_name varchar(255) NULL COMMENT ''领取来源直播间名称'' AFTER live_room_id'
    )
);
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;
