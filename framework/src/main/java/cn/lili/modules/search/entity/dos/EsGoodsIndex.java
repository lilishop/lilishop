package cn.lili.modules.search.entity.dos;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.elasticsearch.EsSuffix;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsParamsItemDTO;
import cn.lili.modules.promotion.tools.PromotionTools;
import com.alibaba.fastjson2.JSON;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.*;

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
@Document(indexName = "#{@elasticsearchProperties.indexPrefix}_" + EsSuffix.GOODS_INDEX_NAME, createIndex = false)
@ToString
@NoArgsConstructor
@Accessors(chain = true)
public class EsGoodsIndex implements Serializable {

    private static final long serialVersionUID = -6856471777036048874L;

    @Id
    @Schema(description = "商品skuId")
    private String id;

    /**
     * 商品id
     */
    @Schema(description = "商品Id")
    @Field(type = FieldType.Text)
    private String goodsId;

    /**
     * 商品名称
     */
    @Field(type = FieldType.Text, analyzer = "ik_max_word")
    @Schema(description = "商品名称")
    private String goodsName;

    /**
     * 商品编号
     */
    @Field(type = FieldType.Keyword)
    @Schema(description = "商品编号")
    private String sn;

    /**
     * 卖家id
     */
    @Field(type = FieldType.Text)
    @Schema(description = "卖家id")
    private String storeId;

    /**
     * 卖家名称
     */
    @Field(type = FieldType.Text)
    @Schema(description = "卖家名称")
    private String storeName;

    /**
     * 销量
     */
    @Field(type = FieldType.Integer)
    @Schema(description = "销量")
    private Integer buyCount;

    /**
     * 小图
     */
    @Schema(description = "小图")
    private String small;

    /**
     * 缩略图
     */
    @Schema(description = "缩略图")
    private String thumbnail;

    /**
     * 品牌id
     */
    @Field(type = FieldType.Text, fielddata = true)
    @Schema(description = "品牌id")
    private String brandId;

    /**
     * 品牌名称
     */
    @MultiField(mainField = @Field(type = FieldType.Text, fielddata = true), otherFields = @InnerField(suffix = "keyword", type = FieldType.Keyword))
    @Schema(title = "品牌名称")
    private String brandName;

    /**
     * 品牌图片地址
     */
    @MultiField(mainField = @Field(type = FieldType.Text, fielddata = true), otherFields = @InnerField(suffix = "keyword", type = FieldType.Keyword))
    @Schema(title = "品牌图片地址")
    private String brandUrl;

    /**
     * 分类path
     */
    @Field(type = FieldType.Text, fielddata = true)
    @Schema(description = "分类path")
    private String categoryPath;

    /**
     * 分类名称path
     */
    @MultiField(mainField = @Field(type = FieldType.Text, fielddata = true), otherFields = @InnerField(suffix = "keyword", type = FieldType.Keyword))
    @Schema(title = "分类名称path")
    private String categoryNamePath;

    /**
     * 店铺分类id
     */
    @Field(type = FieldType.Text, fielddata = true)
    @Schema(description = "店铺分类id")
    private String storeCategoryPath;

    /**
     * 店铺分类名称
     */
    @MultiField(mainField = @Field(type = FieldType.Text, fielddata = true), otherFields = @InnerField(suffix = "keyword", type = FieldType.Keyword))
    @Schema(title = "店铺分类名称")
    private String storeCategoryNamePath;

    /**
     * 商品价格
     */
    @Field(type = FieldType.Double)
    @Schema(description = "商品价格")
    private Double price;

    /**
     * 促销价
     */
    @Field(type = FieldType.Double)
    @Schema(description = "促销价")
    private Double promotionPrice;

    /**
     * 如果是积分商品需要使用的积分
     */
    @Field(type = FieldType.Integer)
    @Schema(description = "积分商品需要使用的积分")
    private Integer point;

    /**
     * 评价数量
     */
    @Field(type = FieldType.Integer)
    @Schema(description = "评价数量")
    private Integer commentNum;

    /**
     * 好评数量
     */
    @Field(type = FieldType.Integer)
    @Schema(description = "好评数量")
    private Integer highPraiseNum;

    /**
     * 好评率
     */
    @Field(type = FieldType.Double)
    @Schema(description = "好评率")
    private Double grade;

    /**
     * 详情
     */
    @Field(type = FieldType.Text)
    @Schema(description = "详情")
    private String intro;

    /**
     * 商品移动端详情
     */
    @Field(type = FieldType.Text)
    @Schema(description = "商品移动端详情")
    private String mobileIntro;

    /**
     * 是否自营
     */
    @Field(type = FieldType.Boolean)
    @Schema(description = "是否自营")
    private Boolean selfOperated;

    /**
     * 是否为推荐商品
     */
    @Field(type = FieldType.Boolean)
    @Schema(description = "是否为推荐商品")
    private Boolean recommend;

    /**
     * 销售模式
     *
     * @see cn.lili.modules.goods.entity.enums.GoodsSalesModeEnum
     */
    @Field(type = FieldType.Text)
    @Schema(description = "销售模式")
    private String salesModel;

    /**
     * 审核状态
     */
    @Field(type = FieldType.Text)
    @Schema(description = "审核状态")
    private String authFlag;

    /**
     * 卖点
     */
    @MultiField(mainField = @Field(type = FieldType.Text, fielddata = true), otherFields = @InnerField(suffix = "keyword", type = FieldType.Keyword))
    @Schema(description = "卖点")
    private String sellingPoint;

    /**
     * 上架状态
     */
    @Field(type = FieldType.Text)
    @Schema(description = "上架状态")
    private String marketEnable;

    /**
     * 商品视频
     */
    @Field(type = FieldType.Text)
    @Schema(description = "商品视频")
    private String goodsVideo;

    @Schema(description = "商品发布时间")
    @Field(type = FieldType.Date)
    private Date releaseTime;

    /**
     * @see cn.lili.modules.goods.entity.enums.GoodsTypeEnum
     */
    @Schema(description = "商品类型", required = true)
    @Field(type = FieldType.Text)
    private String goodsType;

    @Schema(description = "商品sku基础分数", required = true)
    @Field(type = FieldType.Integer)
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
    @Field(type = FieldType.Text)
    @Schema(description = "商品促销活动集合JSON，key 为 促销活动类型，value 为 促销活动实体信息 ")
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
            this.buyCount = (sku.getBuyCount() != null ? sku.getBuyCount() : 0)
                    + (sku.getVirtualSales() != null ? sku.getVirtualSales() : 0);
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
            this.releaseTime = new Date();
        }
    }

    /**
     * 参数索引增加
     *
     * @param sku            商品sku信息
     * @param goodsParamDTOS 商品参数信息
     */
    public EsGoodsIndex(GoodsSku sku, List<GoodsParamsItemDTO> goodsParamDTOS) {
        this(sku);
        //如果参数不为空
        if (goodsParamDTOS != null && !goodsParamDTOS.isEmpty()) {
            //接受不了参数索引
            List<EsGoodsAttribute> attributes = new ArrayList<>();
            //循环分组的内容
            goodsParamDTOS.forEach(goodsParam -> {
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
            this.attrList = attributes;
        }
    }

    public EsGoodsIndex(GoodsSku sku, Date createDate) {
        this(sku);
        this.releaseTime = createDate;
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
            this.buyCount = (sku.getBuyCount() != null ? sku.getBuyCount() : 0)
                    + (sku.getVirtualSales() != null ? sku.getVirtualSales() : 0);
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
            this.releaseTime = new Date();
        }
    }

    public Map<String, Object> getOriginPromotionMap() {
        return JSON.parseObject(this.promotionMapJson);
    }

    public Map<String, Object> getPromotionMap() {
        return PromotionTools.filterInvalidPromotionsMap(JSON.parseObject(this.promotionMapJson));
    }
}
