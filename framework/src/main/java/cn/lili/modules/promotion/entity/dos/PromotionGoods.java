package cn.lili.modules.promotion.entity.dos;

import cn.hutool.core.bean.BeanUtil;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.promotion.entity.dto.KanjiaActivityGoodsDTO;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.data.elasticsearch.annotations.DateFormat;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;

import java.util.Date;

/**
 * 促销活动商品实体类
 *
 * @author Chopper
 * @since 2020-03-19 10:44 上午
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("li_promotion_goods")
@ApiModel(value = "促销商品")
@NoArgsConstructor
public class PromotionGoods extends BaseEntity {

    private static final long serialVersionUID = 4150737500248136108L;

    @ApiModelProperty(value = "商家ID")
    private String storeId;

    @ApiModelProperty(value = "商家名称")
    private String storeName;

    @ApiModelProperty(value = "商品id")
    private String goodsId;

    @ApiModelProperty(value = "商品SkuId")
    private String skuId;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "缩略图")
    private String thumbnail;

    @ApiModelProperty(value = "活动开始时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @Field(type = FieldType.Date, format = DateFormat.custom, pattern = "yyyy-MM-dd HH:mm:ss || yyyy-MM-dd || yyyy/MM/dd HH:mm:ss|| yyyy/MM/dd ||epoch_millis")
    private Date startTime;

    @ApiModelProperty(value = "活动结束时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @Field(type = FieldType.Date, format = DateFormat.custom, pattern = "yyyy-MM-dd HH:mm:ss || yyyy-MM-dd || yyyy/MM/dd HH:mm:ss|| yyyy/MM/dd ||epoch_millis")
    private Date endTime;

    @ApiModelProperty(value = "活动id")
    private String promotionId;

    /**
     * @see PromotionTypeEnum
     */
    @ApiModelProperty(value = "促销工具类型")
    private String promotionType;

    /**
     * @see cn.lili.modules.goods.entity.enums.GoodsTypeEnum
     */
    @ApiModelProperty(value = "商品类型")
    private String goodsType;

    @ApiModelProperty(value = "活动标题")
    private String title;

    @ApiModelProperty(value = "卖出的商品数量")
    private Integer num;

    @ApiModelProperty(value = "原价")
    private Double originalPrice;

    @ApiModelProperty(value = "促销价格")
    private Double price;

    @ApiModelProperty(value = "兑换积分")
    private Long points;

    @ApiModelProperty(value = "限购数量")
    private Integer limitNum;

    @ApiModelProperty(value = "促销库存")
    private Integer quantity;

    @ApiModelProperty(value = "分类path")
    private String categoryPath;

    /**
     * @see PromotionsScopeTypeEnum
     */
    @ApiModelProperty(value = "关联范围类型")
    private String scopeType = PromotionsScopeTypeEnum.PORTION_GOODS.name();


    @ApiModelProperty(value = "范围关联的id")
    private String scopeId;

    public PromotionGoods(GoodsSku sku) {
        if (sku != null) {
            BeanUtil.copyProperties(sku, this, "id", "price");
            this.skuId = sku.getId();
            this.originalPrice = sku.getPrice();
        }
    }

    public PromotionGoods(PointsGoods pointsGoods, GoodsSku sku) {
        if (pointsGoods != null) {
            BeanUtil.copyProperties(sku, this, "id");
            BeanUtil.copyProperties(pointsGoods, this, "id");
            this.promotionId = pointsGoods.getId();
            this.quantity = pointsGoods.getActiveStock();
            this.originalPrice = sku.getPrice();
            this.promotionType = PromotionTypeEnum.POINTS_GOODS.name();
            this.scopeId = sku.getId();
        }
    }


    public PromotionGoods(KanjiaActivityGoodsDTO kanjiaActivityGoodsDTO) {
        if (kanjiaActivityGoodsDTO != null) {
            BeanUtil.copyProperties(kanjiaActivityGoodsDTO, this, "id");
            BeanUtil.copyProperties(kanjiaActivityGoodsDTO.getGoodsSku(), this, "id");
            this.setQuantity(kanjiaActivityGoodsDTO.getStock());
            this.setPromotionId(kanjiaActivityGoodsDTO.getId());
            this.setPromotionType(PromotionTypeEnum.KANJIA.name());
            this.setTitle(PromotionTypeEnum.KANJIA.name() + "-" + kanjiaActivityGoodsDTO.getGoodsName());
            this.setScopeType(PromotionsScopeTypeEnum.PORTION_GOODS.name());
            this.setPromotionType(PromotionTypeEnum.KANJIA.name());
        }
    }
}