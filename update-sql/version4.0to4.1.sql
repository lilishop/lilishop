/** 修改文章表**/
ALTER TABLE li_article DROP COLUMN open_status;
ALTER TABLE li_article ADD open_status bit ( 1 );
UPDATE li_article SET open_status = 1;
/** 增加会员等级会员经验值**/
ALTER TABLE li_member ADD gradeId VARCHAR ( 32 );
ALTER TABLE li_member ADD experience BIGINT;
/**去除商品运费承担**/
ALTER TABLE li_goods DROP COLUMN freight_payer;
/**增加优惠券有效期类型**/
ALTER TABLE li_coupon ADD range_day_type VARCHAR ( 32 );
ALTER TABLE li_coupon ADD effective_days INT;
/**秒杀增加显示商品数量**/
ALTER TABLE li_seckill ADD goods_num INT;
