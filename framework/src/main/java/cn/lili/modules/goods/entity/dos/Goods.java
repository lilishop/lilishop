package cn.lili.modules.goods.entity.dos;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.convert.Convert;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.http.HtmlUtil;
import com.alibaba.fastjson2.JSON;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsSalesModeEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.xkcoding.http.util.StringUtil;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hibernate.validator.constraints.Length;

import java.util.Date;
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
@Schema(description = "商品")
public class Goods extends BaseEntity {

    private static final long serialVersionUID = 370683495251252601L;

    @Schema(description = "商品名称")
    @NotEmpty(message = "商品名称不能为空")
    @Length(max = 100, message = "商品名称太长，不能超过100个字符")
    private String goodsName;

    @Schema(description = "SPU编码")
    private String spuSn;

    @Schema(description = "商品价格", requiredMode = Schema.RequiredMode.REQUIRED)
    @NotNull(message = "商品价格不能为空")
    @Min(value = 0, message = "商品价格不能为负数")
    @Max(value = 99999999, message = "商品价格不能超过99999999")
    private Double price;

    @Schema(description = "品牌id")
    private String brandId;

    @Schema(description = "分类path")
    private String categoryPath;

    @Schema(description = "计量单位")
    private String goodsUnit;


    @Length(max = 60, message = "商品卖点太长，不能超过60个字符")
    @Schema(description = "卖点")
    private String sellingPoint;

    /**
     * @see GoodsStatusEnum
     */
    @Schema(description = "上架状态")
    private String marketEnable;

    @Schema(description = "详情")
    private String intro;

    @Schema(description = "购买数量")
    private Integer buyCount;

    @Max(value = 99999999, message = "库存不能超过99999999")
    @Schema(description = "库存")
    private Integer quantity;

    @Schema(description = "商品好评率")
    private Double grade;

    @Schema(description = "缩略图路径")
    private String thumbnail;

    @Schema(description = "小图路径")
    private String small;

    @Schema(description = "原图路径")
    private String original;

    @Schema(description = "店铺分类id")
    private String storeCategoryPath;

    @Schema(description = "评论数量")
    private Integer commentNum;

    @Schema(description = "卖家id")
    private String storeId;

    @Schema(description = "卖家名字")
    private String storeName;

    @Schema(description = "运费模板id")
    private String templateId;

    /**
     * @see GoodsAuthEnum
     */
    @Schema(description = "审核状态")
    private String authFlag;

    @Schema(description = "审核信息")
    private String authMessage;

    @Schema(description = "下架原因")
    private String underMessage;

    @Schema(description = "计划上架时间（到秒，yyyy-MM-dd HH:mm:ss）")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss", timezone = "GMT+8")
    private Date scheduledUpperTime;

    @Schema(description = "计划上架原因")
    private String scheduledUpperReason;

    @Schema(description = "计划下架时间（到秒，yyyy-MM-dd HH:mm:ss）")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss", timezone = "GMT+8")
    private Date scheduledDownTime;

    @Schema(description = "计划下架原因")
    private String scheduledDownReason;

    @Schema(description = "是否自营")
    private Boolean selfOperated;

    @Schema(description = "商品移动端详情")
    private String mobileIntro;

    @Schema(description = "商品视频")
    private String goodsVideo;


    @Schema(description = "是否为推荐商品", requiredMode = Schema.RequiredMode.REQUIRED)
    private Boolean recommend;

    /**
     * 销售模式兼容字段，统一按 RETAIL（零售）处理历史值。
     * @see cn.lili.modules.goods.entity.enums.GoodsSalesModeEnum
     */
    @Schema(description = "销售模式（兼容字段，固定零售）", allowableValues = "RETAIL")
    private String salesModel;


    /**
     * @see cn.lili.modules.goods.entity.enums.GoodsTypeEnum
     */
    @Schema(description = "商品类型", requiredMode = Schema.RequiredMode.REQUIRED)
    private String goodsType;

    /**
     * 商品参数 JSON。与倒排表 {@code li_goods_search_param} 同源：实体上的最终值才是写倒排的输入。
     * {@code null} 表示本次不更新该列（edit 走 updateById 时该列不进 UPDATE）。
     */
    @Schema(description = "商品参数json。null 表示本次不更新该列，空数组 [] 表示清空参数", hidden = true)
    private String params;


    /** 返回统一的零售模式，兼容历史商品和缓存数据。 */
    public String getSalesModel() {
        return GoodsSalesModeEnum.RETAIL.name();
    }

    public Goods() {
    }

    public Goods(GoodsOperationDTO goodsOperationDTO) {
        this.goodsName = goodsOperationDTO.getGoodsName();
        this.spuSn = goodsOperationDTO.getSpuSn();
        this.categoryPath = goodsOperationDTO.getCategoryPath();
        this.storeCategoryPath = goodsOperationDTO.getStoreCategoryPath();
        this.brandId = goodsOperationDTO.getBrandId();
        this.templateId = goodsOperationDTO.getTemplateId();
        this.recommend = goodsOperationDTO.getRecommend();
        this.sellingPoint = goodsOperationDTO.getSellingPoint();
        this.salesModel = GoodsSalesModeEnum.RETAIL.name();
        this.goodsUnit = goodsOperationDTO.getGoodsUnit();
        this.intro = goodsOperationDTO.getIntro();
        this.mobileIntro = goodsOperationDTO.getMobileIntro();
        this.goodsVideo = goodsOperationDTO.getGoodsVideo();
        this.price = goodsOperationDTO.getPrice();
        if (goodsOperationDTO.getGoodsParamsDTOList() != null) {
            this.params = JSON.toJSONString(goodsOperationDTO.getGoodsParamsDTOList());
        }
        //如果立即上架则
        this.marketEnable = Boolean.TRUE.equals(goodsOperationDTO.getRelease()) ? GoodsStatusEnum.UPPER.name() : GoodsStatusEnum.DOWN.name();
        this.goodsType = goodsOperationDTO.getGoodsType();
        this.grade = 100D;

        //循环sku，判定sku是否有效
        for (Map<String, Object> sku : goodsOperationDTO.getSkuList()) {
            //SKU编码(sn)由后端自动生成，不再要求前端必传
            if ((!sku.containsKey("price") || StringUtil.isEmpty(sku.get("price").toString()) || Convert.toDouble(sku.get("price")) <= 0)) {
                throw new ServiceException(ResultCode.GOODS_SKU_PRICE_ERROR);
            }
            if ((!sku.containsKey("cost") || StringUtil.isEmpty(sku.get("cost").toString()) || Convert.toDouble(sku.get("cost")) <= 0)) {
                throw new ServiceException(ResultCode.GOODS_SKU_COST_ERROR);
            }
            //虚拟商品没有重量字段
            if (this.goodsType.equals(GoodsTypeEnum.PHYSICAL_GOODS.name()) && (!sku.containsKey("weight") || sku.containsKey("weight") && (StringUtil.isEmpty(sku.get("weight").toString()) || Convert.toDouble(sku.get("weight").toString()) < 0))) {
                throw new ServiceException(ResultCode.GOODS_SKU_WEIGHT_ERROR);
            }
            if (!sku.containsKey("quantity") || StringUtil.isEmpty(sku.get("quantity").toString()) || Convert.toInt(sku.get("quantity").toString()) < 0) {
                throw new ServiceException(ResultCode.GOODS_SKU_QUANTITY_ERROR);
            }
            sku.forEach((key, value) -> {
                //SKU编码(sn)允许为空，空值由后端生成
                if ("sn".equals(key)) {
                    return;
                }
                if (CharSequenceUtil.isBlank(value.toString())) {
                    throw new ServiceException(ResultCode.MUST_HAVE_GOODS_SKU_VALUE);
                }
            });


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
