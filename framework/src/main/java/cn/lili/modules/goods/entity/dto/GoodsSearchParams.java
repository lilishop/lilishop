package cn.lili.modules.goods.entity.dto;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * 商品查询条件
 *
 * @author pikachu
 * @since 2020-02-24 19:27:20
 */
@EqualsAndHashCode(callSuper = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class GoodsSearchParams extends PageVO {

    private static final long serialVersionUID = 2544015852728566887L;


    @Schema(description = "商品编号")
    private String goodsId;

    @Schema(description = "商品规格ID")
    private String skuId;

    @Schema(description = "商品规格ID集合")
    private List<String> skuIds;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "商品规格货号")
    private String sn;

    @Schema(description = "规格简短信息")
    private String simpleSpecs;

    @Schema(description = "SKU名称")
    private String skuName;

    @Schema(description = "SKU货号")
    private String skuSn;

    @Schema(description = "商品编号")
    private String id;

    @Schema(description = "商品编号")
    private List<String> ids;

    @Schema(description = "商家ID")
    private String storeId;

    @Schema(description = "卖家名字")
    private String storeName;

    @Schema(description = "价格,可以为范围，如10_1000")
    private String price;

    @Schema(description = "分类path")
    private String categoryPath;

    @Schema(description = "店铺分类id")
    private String storeCategoryPath;

    @Schema(description = "是否自营")
    private Boolean selfOperated;

    /**
     * @see GoodsStatusEnum
     */
    @Schema(description = "上下架状态")
    private String marketEnable;

    /**
     * @see GoodsAuthEnum
     */
    @Schema(description = "审核状态")
    private String authFlag;

    @Schema(description = "商品状态")
    private String goodsStatus;

    @Schema(description = "库存数量")
    private Integer leQuantity;

    @Schema(description = "库存数量")
    private Integer geQuantity;

    @Schema(description = "是否为推荐商品")
    private Boolean recommend;

    /**
     * @see cn.lili.modules.goods.entity.enums.GoodsTypeEnum
     */
    @Schema(description = "商品类型")
    private String goodsType;

    /**
     * @see cn.lili.modules.goods.entity.enums.GoodsSalesModeEnum
     */
    @Schema(description = "销售模式", requiredMode = Schema.RequiredMode.REQUIRED)
    private String salesModel;

    @Schema(description = "商品分组ID")
    private String groupId;

    @Schema(description = "预警库存")
    private Boolean alertQuantity;

    @Schema(description = "品牌ID")
    private String brandId;

    @Schema(description = "销量下限")
    private Integer geBuyCount;

    @Schema(description = "销量上限")
    private Integer leBuyCount;

    @Schema(description = "创建开始时间")
    private String startDate;

    @Schema(description = "创建结束时间")
    private String endDate;

    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();

        // 统一使用 CharSequenceUtil.isNotEmpty() 处理字符串
        if (CharSequenceUtil.isNotEmpty(goodsId)) {
            queryWrapper.eq("goods_id", goodsId);
        }
        if (CharSequenceUtil.isNotEmpty(skuId)) {
            queryWrapper.eq("id", skuId);
        }
        if (CollUtil.isNotEmpty(skuIds)) {
            queryWrapper.in("id", skuIds);
        }
        if (CharSequenceUtil.isNotEmpty(goodsName)) {
            queryWrapper.like("goods_name", goodsName);
        }
        if (CharSequenceUtil.isNotEmpty(sn)) {
            queryWrapper.like("sn", sn);
        }
        if (CharSequenceUtil.isNotEmpty(simpleSpecs)) {
            queryWrapper.like("simple_specs", simpleSpecs);
        }
        if (CharSequenceUtil.isNotEmpty(skuName)) {
            queryWrapper.apply(
                    "EXISTS (SELECT 1 FROM li_goods_sku gs WHERE gs.goods_id = li_goods.id AND gs.delete_flag = 0 AND gs.goods_name LIKE CONCAT('%', {0}, '%'))",
                    skuName
            );
        }
        if (CharSequenceUtil.isNotEmpty(skuSn)) {
            queryWrapper.apply(
                    "EXISTS (SELECT 1 FROM li_goods_sku gs WHERE gs.goods_id = li_goods.id AND gs.delete_flag = 0 AND gs.sn LIKE CONCAT('%', {0}, '%'))",
                    skuSn
            );
        }
        if (CharSequenceUtil.isNotEmpty(id)) {
            queryWrapper.in("id", Arrays.asList(id.split(",")));
        }

        // 统一使用 CollUtil.isNotEmpty() 处理集合
        if (CollUtil.isNotEmpty(ids)) {
            queryWrapper.in("id", ids);
        }

        if (CharSequenceUtil.isNotEmpty(storeId)) {
            queryWrapper.eq("store_id", storeId);
        }
        if (CharSequenceUtil.isNotEmpty(storeName)) {
            queryWrapper.like("store_name", storeName);
        }
        if (CharSequenceUtil.isNotEmpty(categoryPath)) {
            queryWrapper.like("category_path", categoryPath);
        }
        if (CharSequenceUtil.isNotEmpty(storeCategoryPath)) {
            queryWrapper.like("store_category_path", storeCategoryPath);
        }
        if (CharSequenceUtil.isNotEmpty(goodsStatus)) {
            if (goodsStatus.equals(GoodsStatusEnum.UPPER.name())) {
                queryWrapper.eq("auth_flag", GoodsAuthEnum.PASS.name());
                queryWrapper.eq("market_enable", GoodsStatusEnum.UPPER.name());
            } else if (goodsStatus.equals(GoodsStatusEnum.DOWN.name())) {
                queryWrapper.eq("auth_flag", GoodsAuthEnum.PASS.name());
                queryWrapper.eq("market_enable", GoodsStatusEnum.DOWN.name());
            } else if (goodsStatus.equals(GoodsAuthEnum.TOBEAUDITED.name())) {
                queryWrapper.eq("auth_flag", GoodsAuthEnum.TOBEAUDITED.name());
            } else if (goodsStatus.equals(GoodsAuthEnum.REFUSE.name())) {
                queryWrapper.eq("auth_flag", GoodsAuthEnum.REFUSE.name());
            }
        }

        // 统一使用 Objects.nonNull() 处理对象非空判断
        if (Objects.nonNull(selfOperated)) {
            queryWrapper.eq("self_operated", selfOperated);
        }

        if (CharSequenceUtil.isNotEmpty(marketEnable)) {
            queryWrapper.eq("market_enable", marketEnable);
        }
        if (CharSequenceUtil.isNotEmpty(authFlag)) {
            queryWrapper.eq("auth_flag", authFlag);
        }

        if (Objects.nonNull(leQuantity)) {
            queryWrapper.le("quantity", leQuantity);
        }
        if (Objects.nonNull(geQuantity)) {
            queryWrapper.ge("quantity", geQuantity);
        }
        if (Objects.nonNull(leBuyCount)) {
            queryWrapper.le("buy_count", leBuyCount);
        }
        if (Objects.nonNull(geBuyCount)) {
            queryWrapper.ge("buy_count", geBuyCount);
        }
        if (CharSequenceUtil.isNotEmpty(brandId)) {
            queryWrapper.eq("brand_id", brandId);
        }
        if (Objects.nonNull(recommend)) {
            queryWrapper.le("recommend", recommend);
        }

        if (CharSequenceUtil.isNotEmpty(goodsType)) {
            queryWrapper.eq("goods_type", goodsType);
        }
        if (CharSequenceUtil.isNotEmpty(salesModel)) {
            queryWrapper.eq("sales_model", salesModel);
        }
        if (CharSequenceUtil.isNotEmpty(groupId)) {
            queryWrapper.apply(
                    "EXISTS (SELECT 1 FROM li_goods_group_goods ggg WHERE ggg.goods_id = li_goods.id AND ggg.group_id = {0} AND ggg.delete_flag = 0)",
                    groupId
            );
        }

        if (Objects.nonNull(alertQuantity) && alertQuantity) {
            queryWrapper.apply("quantity <= alert_quantity");
            queryWrapper.ge("alert_quantity", 0);
        }

        queryWrapper.in(CollUtil.isNotEmpty(ids), "id", ids);
        queryWrapper.eq("delete_flag", false);
        if (StringUtils.isNotEmpty(startDate)) {
            queryWrapper.ge("create_time", DateUtil.parse(startDate));
        }
        if (StringUtils.isNotEmpty(endDate)) {
            queryWrapper.le("create_time", DateUtil.parse(endDate));
        }
        this.betweenWrapper(queryWrapper);
        return queryWrapper;
    }

    private <T> void betweenWrapper(QueryWrapper<T> queryWrapper) {
        if (CharSequenceUtil.isNotEmpty(price)) {
            String[] s = price.split("_");
            if (s.length > 1) {
                queryWrapper.between("price", s[0], s[1]);
            } else {
                queryWrapper.ge("price", s[0]);
            }
        }
    }


}
