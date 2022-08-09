package cn.lili.modules.goods.entity.dos;

import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.Max;
import java.util.Date;

/**
 * 商品sku
 *
 * @author pikachu
 * @since 2020-02-23 9:14:33
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("li_goods_sku")
@ApiModel(value = "商品sku对象")
@NoArgsConstructor
public class GoodsSku extends BaseEntity {

    private static final long serialVersionUID = 4865908658161118934L;

    @ApiModelProperty(value = "商品id")
    private String goodsId;

    @ApiModelProperty(value = "规格信息json", hidden = true)
    @JsonIgnore
    private String specs;

    @ApiModelProperty(value = "规格信息")
    private String simpleSpecs;

    @ApiModelProperty(value = "配送模版id")
    private String freightTemplateId;

    @ApiModelProperty(value = "是否是促销商品")
    private Boolean promotionFlag;

    @ApiModelProperty(value = "促销价")
    private Double promotionPrice;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @Length(max = 30, message = "商品规格编号太长，不能超过30个字符")
    @ApiModelProperty(value = "商品编号")
    private String sn;

    @ApiModelProperty(value = "品牌id")
    private String brandId;

    @ApiModelProperty(value = "分类path")
    private String categoryPath;

    @ApiModelProperty(value = "计量单位")
    private String goodsUnit;

    @ApiModelProperty(value = "卖点")
    private String sellingPoint;

    @ApiModelProperty(value = "重量")
    @Max(value = 99999999, message = "重量不能超过99999999")
    private Double weight;
    /**
     * @see GoodsStatusEnum
     */
    @ApiModelProperty(value = "上架状态")
    private String marketEnable;

    @ApiModelProperty(value = "商品详情")
    private String intro;

    @Max(value = 99999999, message = "价格不能超过99999999")
    @ApiModelProperty(value = "商品价格")
    private Double price;

    @Max(value = 99999999, message = "成本价格99999999")
    @ApiModelProperty(value = "成本价格")
    private Double cost;

    @ApiModelProperty(value = "浏览数量")
    private Integer viewCount;

    @ApiModelProperty(value = "购买数量")
    private Integer buyCount;

    @Max(value = 99999999, message = "库存不能超过99999999")
    @ApiModelProperty(value = "库存")
    private Integer quantity;

    @ApiModelProperty(value = "商品好评率")
    private Double grade;

    @ApiModelProperty(value = "缩略图路径")
    private String thumbnail;

    @ApiModelProperty(value = "大图路径")
    private String big;

    @ApiModelProperty(value = "小图路径")
    private String small;

    @ApiModelProperty(value = "原图路径")
    private String original;

    @ApiModelProperty(value = "店铺分类id")
    private String storeCategoryPath;

    @ApiModelProperty(value = "评论数量")
    private Integer commentNum;

    @ApiModelProperty(value = "卖家id")
    private String storeId;

    @ApiModelProperty(value = "卖家名字")
    private String storeName;

    @ApiModelProperty(value = "运费模板id")
    private String templateId;

    /**
     * @see GoodsAuthEnum
     */
    @ApiModelProperty(value = "审核状态")
    private String authFlag;

    @ApiModelProperty(value = "审核信息")
    private String authMessage;

    @ApiModelProperty(value = "下架原因")
    private String underMessage;

    @ApiModelProperty(value = "是否自营")
    private Boolean selfOperated;

    @ApiModelProperty(value = "商品移动端详情")
    private String mobileIntro;

    @ApiModelProperty(value = "商品视频")
    private String goodsVideo;

    @ApiModelProperty(value = "是否为推荐商品", required = true)
    private Boolean recommend;

    /**
     * @see cn.lili.modules.goods.entity.enums.GoodsSalesModeEnum
     */
    @ApiModelProperty(value = "销售模式", required = true)
    private String salesModel;
    /**
     * @see cn.lili.modules.goods.entity.enums.GoodsTypeEnum
     */
    @ApiModelProperty(value = "商品类型", required = true)
    private String goodsType;

    public Double getWeight() {
        if (weight == null) {
            return 0d;
        }
        return weight;
    }

    @Override
    public Date getCreateTime() {
        if (super.getCreateTime() == null) {
            return new Date(1593571928);
        } else {
            return super.getCreateTime();
        }
    }

    /**
     * 设置规格商品的基本商品信息
     *
     * @param goods 基本商品信息
     */
    public GoodsSku(Goods goods) {
        //商品基本信息
        this.goodsId = goods.getId();
        this.goodsName = goods.getGoodsName();
        this.goodsType = goods.getGoodsType();

        this.selfOperated = goods.getSelfOperated();
        this.sellingPoint = goods.getSellingPoint();
        this.categoryPath = goods.getCategoryPath();
        this.brandId = goods.getBrandId();
        this.marketEnable = goods.getMarketEnable();
        this.intro = goods.getIntro();
        this.mobileIntro = goods.getMobileIntro();
        this.goodsUnit = goods.getGoodsUnit();
        this.grade = 100D;
        //商品状态
        this.authFlag = goods.getAuthFlag();
        this.salesModel = goods.getSalesModel();
        //卖家信息
        this.storeId = goods.getStoreId();
        this.storeName = goods.getStoreName();
        this.storeCategoryPath = goods.getStoreCategoryPath();
        this.freightTemplateId = goods.getTemplateId();
        this.recommend = goods.getRecommend();
    }

}