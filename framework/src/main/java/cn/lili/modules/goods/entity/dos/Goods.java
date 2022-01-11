package cn.lili.modules.goods.entity.dos;

import cn.hutool.core.convert.Convert;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.http.HtmlUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.xkcoding.http.util.StringUtil;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.Map;

/**
 * 商品
 *
 * @author pikachu
 * @since 2020-02-23 9:14:33
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("li_goods")
@ApiModel(value = "商品")
public class Goods extends BaseEntity {

    private static final long serialVersionUID = 370683495251252601L;

    @ApiModelProperty(value = "商品名称")
    @NotEmpty(message = "商品名称不能为空")
    @Length(max = 100, message = "商品名称太长，不能超过100个字符")
    private String goodsName;

    @ApiModelProperty(value = "商品价格", required = true)
    @NotNull(message = "商品价格不能为空")
    @Min(value = 0, message = "商品价格不能为负数")
    @Max(value = 99999999, message = "商品价格不能超过99999999")
    private Double price;

    @ApiModelProperty(value = "品牌id")
    private String brandId;

    @ApiModelProperty(value = "分类path")
    private String categoryPath;

    @ApiModelProperty(value = "计量单位")
    private String goodsUnit;


    @Length(max = 60, message = "商品卖点太长，不能超过60个字符")
    @ApiModelProperty(value = "卖点")
    private String sellingPoint;

    /**
     * @see GoodsStatusEnum
     */
    @ApiModelProperty(value = "上架状态")
    private String marketEnable;

    @ApiModelProperty(value = "详情")
    private String intro;

    @ApiModelProperty(value = "购买数量")
    private Integer buyCount;

    @Max(value = 99999999, message = "库存不能超过99999999")
    @ApiModelProperty(value = "库存")
    private Integer quantity;

    @ApiModelProperty(value = "商品好评率")
    private Double grade;

    @ApiModelProperty(value = "缩略图路径")
    private String thumbnail;

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

    @ApiModelProperty(value = "销售模式", required = true)
    private String salesModel;


    /**
     * @see cn.lili.modules.goods.entity.enums.GoodsTypeEnum
     */
    @ApiModelProperty(value = "商品类型", required = true)
    private String goodsType;

    @ApiModelProperty(value = "商品参数json", hidden = true)
    @JsonIgnore
    private String params;


    public Goods() {
    }

    public Goods(GoodsOperationDTO goodsOperationDTO) {
        this.goodsName = goodsOperationDTO.getGoodsName();
        this.categoryPath = goodsOperationDTO.getCategoryPath();
        this.storeCategoryPath = goodsOperationDTO.getStoreCategoryPath();
        this.brandId = goodsOperationDTO.getBrandId();
        this.templateId = goodsOperationDTO.getTemplateId();
        this.recommend = goodsOperationDTO.getRecommend();
        this.sellingPoint = goodsOperationDTO.getSellingPoint();
        this.salesModel = goodsOperationDTO.getSalesModel();
        this.goodsUnit = goodsOperationDTO.getGoodsUnit();
        this.intro = goodsOperationDTO.getIntro();
        this.mobileIntro = goodsOperationDTO.getMobileIntro();
        this.goodsVideo = goodsOperationDTO.getGoodsVideo();
        this.price = goodsOperationDTO.getPrice();
        if (goodsOperationDTO.getGoodsParamsDTOList() != null && goodsOperationDTO.getGoodsParamsDTOList().isEmpty()) {
            this.params = JSONUtil.toJsonStr(goodsOperationDTO.getGoodsParamsDTOList());
        }
        //如果立即上架则
        this.marketEnable = Boolean.TRUE.equals(goodsOperationDTO.getRelease()) ? GoodsStatusEnum.UPPER.name() : GoodsStatusEnum.DOWN.name();
        this.goodsType = goodsOperationDTO.getGoodsType();
        this.grade = 100D;

        //循环sku，判定sku是否有效
        for (Map<String, Object> sku : goodsOperationDTO.getSkuList()) {
            //判定参数不能为空
            if (!sku.containsKey("sn") || sku.get("sn") == null) {
                throw new ServiceException(ResultCode.GOODS_SKU_SN_ERROR);
            }
            if (!sku.containsKey("price") || StringUtil.isEmpty(sku.get("price").toString()) || Convert.toDouble(sku.get("price")) <= 0) {
                throw new ServiceException(ResultCode.GOODS_SKU_PRICE_ERROR);
            }
            if (!sku.containsKey("cost") || StringUtil.isEmpty(sku.get("cost").toString()) || Convert.toDouble(sku.get("cost")) <= 0) {
                throw new ServiceException(ResultCode.GOODS_SKU_COST_ERROR);
            }
            //虚拟商品没有重量字段
            if (this.goodsType.equals(GoodsTypeEnum.PHYSICAL_GOODS.name()) && (!sku.containsKey("weight") || sku.containsKey("weight") && (StringUtil.isEmpty(sku.get("weight").toString()) || Convert.toDouble(sku.get("weight").toString()) < 0))) {
                throw new ServiceException(ResultCode.GOODS_SKU_WEIGHT_ERROR);
            }
            if (!sku.containsKey("quantity") || StringUtil.isEmpty(sku.get("quantity").toString()) || Convert.toInt(sku.get("quantity").toString()) < 0) {
                throw new ServiceException(ResultCode.GOODS_SKU_QUANTITY_ERROR);
            }

        }
    }

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