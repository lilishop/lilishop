package cn.lili.modules.goods.entity.dos;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.http.HtmlUtil;
import cn.lili.modules.goods.entity.enums.DraftGoodsSaveType;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Max;

/**
 * 草稿商品
 *
 * @author pikachu
 * @since 2020-02-23 9:14:33
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("li_draft_goods")
@ApiModel(value = "草稿商品")
@AllArgsConstructor
@NoArgsConstructor
public class DraftGoods extends BaseEntity {

    private static final long serialVersionUID = 370683495251252601L;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @Max(value = 99999999, message = "价格不能超过99999999")
    @ApiModelProperty(value = "商品价格")
    private Double price;


    @ApiModelProperty(value = "品牌id")
    private String brandId;

    @ApiModelProperty(value = "分类path")
    private String categoryPath;

    @ApiModelProperty(value = "计量单位")
    private String goodsUnit;

    @ApiModelProperty(value = "卖点")
    private String sellingPoint;

    /**
     * @see GoodsStatusEnum
     */
    @ApiModelProperty(value = "上架状态")
    private String marketEnable;

    @ApiModelProperty(value = "详情")
    private String intro;


    @ApiModelProperty(value = "商品移动端详情")
    private String mobileIntro;

    @ApiModelProperty(value = "购买数量")
    private Integer buyCount;

    @Max(value = 99999999, message = "库存不能超过99999999")
    @ApiModelProperty(value = "库存")
    private Integer quantity;

    @ApiModelProperty(value = "可用库存")
    private Integer enableQuantity;

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

    @ApiModelProperty(value = "是否自营")
    private Boolean selfOperated;

    @ApiModelProperty(value = "商品视频")
    private String goodsVideo;

    @ApiModelProperty(value = "是否为推荐商品")
    private Boolean recommend;

    /**
     * @see cn.lili.modules.goods.entity.enums.GoodsSalesModeEnum
     */
    @ApiModelProperty(value = "销售模式")
    private String salesModel;

    /**
     * @see DraftGoodsSaveType
     */
    @ApiModelProperty(value = "草稿商品保存类型")
    private String saveType;

    @ApiModelProperty(value = "分类名称JSON")
    private String categoryNameJson;

    @ApiModelProperty(value = "商品参数JSON")
    private String goodsParamsListJson;

    @ApiModelProperty(value = "商品图片JSON")
    private String goodsGalleryListJson;

    @ApiModelProperty(value = "sku列表JSON")
    private String skuListJson;

    /**
     * @see cn.lili.modules.goods.entity.enums.GoodsTypeEnum
     */
    @ApiModelProperty(value = "商品类型", required = true)
    private String goodsType;

    public String getIntro() {
        if (CharSequenceUtil.isNotEmpty(intro)) {
            return HtmlUtil.unescape(intro);
        }
        return intro;
    }

    public String getMobileIntro() {
        if (CharSequenceUtil.isNotEmpty(mobileIntro)) {
            return HtmlUtil.unescape(mobileIntro);
        }
        return mobileIntro;
    }

}