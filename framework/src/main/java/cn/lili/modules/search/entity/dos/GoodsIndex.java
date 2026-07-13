package cn.lili.modules.search.entity.dos;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsParamsItemDTO;
import cn.lili.modules.promotion.tools.PromotionTools;
import com.alibaba.fastjson2.JSON;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * 商品搜索数据
 */
@Data
@ToString
@NoArgsConstructor
@Accessors(chain = true)
public class GoodsIndex implements Serializable {

    private static final long serialVersionUID = -6856471777036048874L;

    @Schema(description = "商品skuId")
    private String id;

    @Schema(description = "商品Id")
    private String goodsId;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "商品编号")
    private String sn;

    @Schema(description = "卖家id")
    private String storeId;

    @Schema(description = "卖家名称")
    private String storeName;

    @Schema(description = "销量")
    private Integer buyCount;

    @Schema(description = "小图")
    private String small;

    @Schema(description = "缩略图")
    private String thumbnail;

    @Schema(description = "品牌id")
    private String brandId;

    @Schema(title = "品牌名称")
    private String brandName;

    @Schema(title = "品牌图片地址")
    private String brandUrl;

    @Schema(description = "分类path")
    private String categoryPath;

    @Schema(title = "分类名称path")
    private String categoryNamePath;

    @Schema(description = "店铺分类id")
    private String storeCategoryPath;

    @Schema(title = "店铺分类名称")
    private String storeCategoryNamePath;

    @Schema(description = "商品价格")
    private Double price;

    @Schema(description = "促销价")
    private Double promotionPrice;

    @Schema(description = "积分商品需要使用的积分")
    private Integer point;

    @Schema(description = "评价数量")
    private Integer commentNum;

    @Schema(description = "好评数量")
    private Integer highPraiseNum;

    @Schema(description = "好评率")
    private Double grade;

    @Schema(description = "详情")
    private String intro;

    @Schema(description = "商品移动端详情")
    private String mobileIntro;

    @Schema(description = "是否自营")
    private Boolean selfOperated;

    @Schema(description = "是否为推荐商品")
    private Boolean recommend;

    @Schema(description = "销售模式")
    private String salesModel;

    @Schema(description = "审核状态")
    private String authFlag;

    @Schema(description = "卖点")
    private String sellingPoint;

    @Schema(description = "上架状态")
    private String marketEnable;

    @Schema(description = "商品视频")
    private String goodsVideo;

    @Schema(description = "商品发布时间")
    private Date releaseTime;

    @Schema(description = "商品类型", required = true)
    private String goodsType;

    @Schema(description = "商品sku基础分数", required = true)
    private Integer skuSource;

    private List<GoodsSearchAttribute> attrList;

    @Schema(description = "商品促销活动集合JSON，key 为 促销活动类型，value 为 促销活动实体信息 ")
    private String promotionMapJson;

    public GoodsIndex(GoodsSku sku) {
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

    public GoodsIndex(GoodsSku sku, List<GoodsParamsItemDTO> goodsParamDTOS) {
        this(sku);
        if (goodsParamDTOS != null && !goodsParamDTOS.isEmpty()) {
            List<GoodsSearchAttribute> attributes = new ArrayList<>();
            goodsParamDTOS.forEach(goodsParam -> {
                if (goodsParam.getIsIndex() != null && goodsParam.getIsIndex() == 1) {
                    GoodsSearchAttribute attribute = new GoodsSearchAttribute();
                    attribute.setType(1);
                    attribute.setName(goodsParam.getParamName());
                    attribute.setValue(goodsParam.getParamValue());
                    attribute.setSort(goodsParam.getSort());
                    attributes.add(attribute);
                }
            });
            this.attrList = attributes;
        }
    }

    public GoodsIndex(GoodsSku sku, Date createDate) {
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
