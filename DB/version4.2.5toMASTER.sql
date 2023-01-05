-- 会员优惠券标识
CREATE TABLE `li_member_coupon_sign`  (
                                     `id` bigint NOT NULL,
                                     `coupon_activity_Id` bigint NULL DEFAULT NULL COMMENT '优惠券活动id',
                                     `member_id` bigint NULL DEFAULT NULL COMMENT '会员id',
                                     `invalid_time` datetime NULL DEFAULT NULL COMMENT '过期时间',
                                     `create_time` datetime NULL DEFAULT NULL COMMENT '创建时间',
                                     PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_bin ROW_FORMAT = DYNAMIC;