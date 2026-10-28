-- 商品发布 > SPU编码（系统自动生成，只读展示）
-- 页面：lilishop-ui/seller/src/views/goods/goods-seller/goodsOperationSec.vue
-- 生成规则：P + yyyyMMddHHmmss + 2位随机数，见 cn.lili.modules.goods.util.GoodsCodeGenerator
ALTER TABLE li_goods ADD COLUMN spu_sn varchar(64) NULL COMMENT 'SPU编码' AFTER goods_name;
