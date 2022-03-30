package cn.lili.modules.search.entity.dos;

import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.elasticsearch.EsSuffix;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsParamsDTO;
import cn.lili.modules.promotion.tools.PromotionTools;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * 商品索引
 *
 * @author paulG
 **/
@Data
@Document(indexName = "#{@elasticsearchProperties.indexPrefix}_" + EsSuffix.GOODS_INDEX_NAME)
@ToString
@NoArgsConstructor
@Accessors(chain = true)
public class EsGoodsIndex implements Serializable {

    private static final long serialVersionUID = -6856471777036048874L;

    @Id
    @ApiModelProperty("商品skuId")
    private String id;

    /**
     * 商品id
     */
    @ApiModelProperty("商品Id")
    @Field(type = FieldType.Text)
    private String goodsId;

    /**
     * 商品名称
     */
    @Field(type = FieldType.Text, analyzer = "ik_max_word")
    @ApiModelProperty("商品名称")
    private String goodsName;

    /**
     * 商品编号
     */
    @Field(type = FieldType.Keyword)
    @ApiModelProperty("商品编号")
    private String sn;

    /**
     * 卖家id
     */
    @Field(type = FieldType.Text)
    @ApiModelProperty("卖家id")
    private String storeId;

    /**
     * 卖家名称
     */
    @Field(type = FieldType.Text)
    @ApiModelProperty("卖家名称")
    private String storeName;

    /**
     * 销量
     */
    @Field(type = FieldType.Integer)
    @ApiModelProperty("销量")
    private Integer buyCount;

    /**
     * 小图
     */
    @ApiModelProperty("小图")
    private String small;

    /**
     * 缩略图
     */
    @ApiModelProperty("缩略图")
    private String thumbnail;

    /**
     * 品牌id
     */
    @Field(type = FieldType.Text, fielddata = true)
    @ApiModelProperty("品牌id")
    private String brandId;

    /**
     * 品牌名称
     */
    @Field(type = FieldType.Text, fielddata = true)
    @ApiModelProperty("品牌名称")
    private String brandName;

    /**
     * 品牌图片地址
     */
    @Field(type = FieldType.Text, fielddata = true)
    @ApiModelProperty("品牌图片地址")
    private String brandUrl;

    /**
     * 分类path
     */
    @Field(type = FieldType.Text, fielddata = true)
    @ApiModelProperty("分类path")
    private String categoryPath;

    /**
     * 分类名称path
     */
    @Field(type = FieldType.Text, fielddata = true)
    @ApiModelProperty("分类名称path")
    private String categoryNamePath;

    /**
     * 店铺分类id
     */
    @Field(type = FieldType.Text, fielddata = true)
    @ApiModelProperty("店铺分类id")
    private String storeCategoryPath;

    /**
     * 店铺分类名称
     */
    @Field(type = FieldType.Keyword)
    @ApiModelProperty("店铺分类名称")
    private String storeCategoryNamePath;

    /**
     * 商品价格
     */
    @Field(type = FieldType.Double)
    @ApiModelProperty("商品价格")
    private Double price;

    /**
     * 促销价
     */
    @Field(type = FieldType.Double)
    @ApiModelProperty("促销价")
    private Double promotionPrice;

    /**
     * 如果是积分商品需要使用的积分
     */
    @Field(type = FieldType.Integer)
    @ApiModelProperty("积分商品需要使用的积分")
    private Integer point;

    /**
     * 评价数量
     */
    @Field(type = FieldType.Integer)
    @ApiModelProperty("评价数量")
    private Integer commentNum;

    /**
     * 好评数量
     */
    @Field(type = FieldType.Integer)
    @ApiModelProperty("好评数量")
    private Integer highPraiseNum;

    /**
     * 好评率
     */
    @Field(type = FieldType.Double)
    @ApiModelProperty("好评率")
    private Double grade;

    /**
     * 详情
     */
    @Field(type = FieldType.Text)
    @ApiModelProperty("详情")
    private String intro;

    /**
     * 商品移动端详情
     */
    @Field(type = FieldType.Text)
    @ApiModelProperty("商品移动端详情")
    private String mobileIntro;

    /**
     * 是否自营
     */
    @Field(type = FieldType.Boolean)
    @ApiModelProperty("是否自营")
    private Boolean selfOperated;

    /**
     * 是否为推荐商品
     */
    @Field(type = FieldType.Boolean)
    @ApiModelProperty("是否为推荐商品")
    private Boolean recommend;

    /**
     * 销售模式
     */
    @Field(type = FieldType.Text)
    @ApiModelProperty("销售模式")
    private String salesModel;

    /**
     * 审核状态
     */
    @Field(type = FieldType.Text)
    @ApiModelProperty("审核状态")
    private String authFlag;

    /**
     * 卖点
     */
    @Field(type = FieldType.Text)
    @ApiModelProperty("卖点")
    private String sellingPoint;

    /**
     * 上架状态
     */
    @Field(type = FieldType.Text)
    @ApiModelProperty("上架状态")
    private String marketEnable;

    /**
     * 商品视频
     */
    @Field(type = FieldType.Text)
    @ApiModelProperty("商品视频")
    private String goodsVideo;

    @ApiModelProperty("商品发布时间")
    @Field(type = FieldType.Date)
    private Long releaseTime;

    /**
     * @see cn.lili.modules.goods.entity.enums.GoodsTypeEnum
     */
    @ApiModelProperty(value = "商品类型", required = true)
    private String goodsType;

    @ApiModelProperty(value = "商品sku基础分数", required = true)
    private Integer skuSource;

    /**
     * 商品属性（参数和规格）
     */
    @Field(type = FieldType.Nested)
    private List<EsGoodsAttribute> attrList;

    /**
     * 商品促销活动集合
     * key 为 促销活动类型
     *
     * @see PromotionTypeEnum
     * value 为 促销活动实体信息
     */
    @Field(type = FieldType.Nested)
    @ApiModelProperty("商品促销活动集合JSON，key 为 促销活动类型，value 为 促销活动实体信息 ")
    private String promotionMapJson;


    public EsGoodsIndex(GoodsSku sku) {
        if (sku != null) {
            this.id = sku.getId();
            this.goodsId = sku.getGoodsId();
            this.goodsName = sku.getGoodsName();
            this.price = sku.getPrice();
            this.storeName = sku.getStoreName();
            this.storeId = sku.getStoreId();
            this.thumbnail = sku.getThumbnail();
            this.categoryPath = sku.getCategoryPath();
            this.goodsVideo = sku.getGoodsVideo();
            this.mobileIntro = sku.getMobileIntro();
            this.buyCount = sku.getBuyCount() != null ? sku.getBuyCount() : 0;
            this.commentNum = sku.getCommentNum();
            this.small = sku.getSmall();
            this.brandId = sku.getBrandId();
            this.sn = sku.getSn();
            this.storeCategoryPath = sku.getStoreCategoryPath();
            this.sellingPoint = sku.getSellingPoint();
            this.selfOperated = sku.getSelfOperated();
            this.salesModel = sku.getSalesModel();
            this.marketEnable = sku.getMarketEnable();
            this.authFlag = sku.getAuthFlag();
            this.intro = sku.getIntro();
            this.grade = sku.getGrade();
            this.recommend = sku.getRecommend();
            this.goodsType = sku.getGoodsType();
            this.releaseTime = new Date().getTime();
        }
    }

    /**
     * 参数索引增加
     *
     * @param sku 商品sku信息
     * @param goodsParamDTOS 商品参数信息
     */
    public EsGoodsIndex(GoodsSku sku, List<GoodsParamsDTO> goodsParamDTOS) {
        this(sku);
        //如果参数不为空
        if (goodsParamDTOS != null && !goodsParamDTOS.isEmpty()) {
            //接受不了参数索引
            List<EsGoodsAttribute> attributes = new ArrayList<>();
            //循环参数分组
            goodsParamDTOS.forEach(goodsParamGroup -> {
                //如果参数有配置，则增加索引
                if (goodsParamGroup.getGoodsParamsItemDTOList() != null && !goodsParamGroup.getGoodsParamsItemDTOList().isEmpty()) {
                    //循环分组的内容
                    goodsParamGroup.getGoodsParamsItemDTOList().forEach(goodsParam -> {
                                //如果字段需要索引，则增加索引字段
                                if (goodsParam.getIsIndex() != null && goodsParam.getIsIndex() == 1) {
                                    EsGoodsAttribute attribute = new EsGoodsAttribute();
                                    attribute.setType(1);
                                    attribute.setName(goodsParam.getParamName());
                                    attribute.setValue(goodsParam.getParamValue());
                                    attribute.setSort(goodsParam.getSort());
                                    attributes.add(attribute);
                                }
                            }
                    );
                }

            });
            this.attrList = attributes;
        }
    }

    public void setGoodsSku(GoodsSku sku) {
        if (sku != null) {
            this.id = sku.getId();
            this.goodsId = sku.getGoodsId();
            this.goodsName = sku.getGoodsName();
            this.price = sku.getPrice();
            this.storeName = sku.getStoreName();
            this.storeId = sku.getStoreId();
            this.thumbnail = sku.getThumbnail();
            this.categoryPath = sku.getCategoryPath();
            this.goodsVideo = sku.getGoodsVideo();
            this.mobileIntro = sku.getMobileIntro();
            this.buyCount = sku.getBuyCount();
            this.commentNum = sku.getCommentNum();
            this.small = sku.getSmall();
            this.brandId = sku.getBrandId();
            this.sn = sku.getSn();
            this.storeCategoryPath = sku.getStoreCategoryPath();
            this.sellingPoint = sku.getSellingPoint();
            this.selfOperated = sku.getSelfOperated();
            this.salesModel = sku.getSalesModel();
            this.marketEnable = sku.getMarketEnable();
            this.authFlag = sku.getAuthFlag();
            this.intro = sku.getIntro();
            this.grade = sku.getGrade();
            this.releaseTime = new Date().getTime();
        }
    }

    public Map<String, Object> getOriginPromotionMap() {
        return JSONUtil.parseObj(this.promotionMapJson);
    }

    public Map<String, Object> getPromotionMap() {
        return PromotionTools.filterInvalidPromotionsMap(JSONUtil.parseObj(this.promotionMapJson));
    }
}
