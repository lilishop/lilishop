package cn.lili.modules.goods.entity.dos;

import cn.hutool.core.convert.Convert;
import cn.hutool.json.JSONUtil;
import cn.lili.base.BaseEntity;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.xkcoding.http.util.StringUtil;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.Max;
import java.util.Map;

/**
 * 商品
 *
 * @author pikachu
 * @date 2020-02-23 9:14:33
 */
@Data
@Entity
@Table(name = "li_goods")
@TableName("li_goods")
@ApiModel(value = "商品")
public class Goods extends BaseEntity {

    private static final long serialVersionUID = 370683495251252601L;
    /**
     * 商品名称
     */
    @ApiModelProperty(value = "商品名称")
    private String goodsName;
    /**
     * 商品编号
     */
    @Length(max = 30, message = "商品规格编号太长，不能超过30个字符")
    @ApiModelProperty(value = "商品编号")
    private String sn;

    @ApiModelProperty(value = "品牌id")
    private String brandId;

    @ApiModelProperty(value = "分类path")
    private String categoryPath;

    @ApiModelProperty(value = "计量单位")
    private String goodsUnit;

    /**
     * 卖点
     */
    @ApiModelProperty(value = "卖点")
    private String sellingPoint;

    /**
     * 重量
     */
    @ApiModelProperty(value = "重量")
    @Max(value = 99999999, message = "重量不能超过99999999")
    private Double weight;
    /**
     * 上架状态
     *
     * @see GoodsStatusEnum
     */
    @ApiModelProperty(value = "上架状态")
    private String marketEnable;
    /**
     * 详情
     */
    @ApiModelProperty(value = "详情")
    private String intro;
    /**
     * 商品价格
     */
    @Max(value = 99999999, message = "价格不能超过99999999")
    @ApiModelProperty(value = "商品价格")
    private Double price;
    /**
     * 成本价格
     */
    @Max(value = 99999999, message = "成本价格99999999")
    @ApiModelProperty(value = "成本价格")
    private Double cost;

    /**
     * 购买数量
     */
    @ApiModelProperty(value = "购买数量")
    private Integer buyCount;
    /**
     * 库存
     */
    @Max(value = 99999999, message = "库存不能超过99999999")
    @ApiModelProperty(value = "库存")
    private Integer quantity;
    /**
     * 商品好评率
     */
    @ApiModelProperty(value = "商品好评率")
    private Double grade;
    /**
     * 缩略图路径
     */
    @ApiModelProperty(value = "缩略图路径")
    private String thumbnail;
    /**
     * 小图路径
     */
    @ApiModelProperty(value = "小图路径")
    private String small;
    /**
     * 原图路径
     */
    @ApiModelProperty(value = "原图路径")
    private String original;
    /**
     * 店铺分类id
     */
    @ApiModelProperty(value = "店铺分类id")
    private String storeCategoryPath;
    /**
     * 评论数量
     */
    @ApiModelProperty(value = "评论数量")
    private Integer commentNum;
    /**
     * 卖家id
     */
    @ApiModelProperty(value = "卖家id")
    private String storeId;
    /**
     * 卖家名字
     */
    @ApiModelProperty(value = "卖家名字")
    private String storeName;
    /**
     * 运费模板id
     */
    @ApiModelProperty(value = "运费模板id")
    private String templateId;
    /**
     * 审核状态
     *
     * @see GoodsAuthEnum
     */
    @ApiModelProperty(value = "审核状态")
    private String isAuth;
    /**
     * 审核信息
     */
    @ApiModelProperty(value = "审核信息")
    private String authMessage;
    /**
     * 下架原因
     */
    @ApiModelProperty(value = "下架原因")
    private String underMessage;
    /**
     * 是否自营
     */
    @ApiModelProperty(value = "是否自营")
    private Boolean selfOperated;
    /**
     * 商品移动端详情
     */
    @ApiModelProperty(value = "商品移动端详情")
    private String mobileIntro;
    /**
     * 商品视频
     */
    @ApiModelProperty(value = "商品视频")
    private String goodsVideo;


    @ApiModelProperty(value = "是否为推荐商品", required = true)
    private boolean recommend;

    @ApiModelProperty(value = "销售模式", required = true)
    private String salesModel;


    /**
     * @see cn.lili.modules.goods.entity.enums.GoodsTypeEnum
     */
    @ApiModelProperty(value = "商品类型", required = true)
    private String goodsType;

    @ApiModelProperty(value = "商品参数json", hidden = true)
    @Column(columnDefinition = "TEXT")
    @JsonIgnore
    private String params;


    public Goods() {
    }

    public Goods(GoodsOperationDTO goodsOperationDTO) {
        this.goodsName = goodsOperationDTO.getGoodsName();
        this.categoryPath = goodsOperationDTO.getCategoryPath();
        this.storeCategoryPath = goodsOperationDTO.getStoreCategoryPath();
        this.brandId = goodsOperationDTO.getBrandId();
        this.sn = goodsOperationDTO.getSn();
        this.price = goodsOperationDTO.getPrice();
        this.weight = goodsOperationDTO.getWeight();
        this.templateId = goodsOperationDTO.getTemplateId();
        this.recommend = goodsOperationDTO.getRecommend();
        this.sellingPoint = goodsOperationDTO.getSellingPoint();
        this.salesModel = goodsOperationDTO.getSalesModel();
        this.goodsUnit = goodsOperationDTO.getGoodsUnit();
        this.intro = goodsOperationDTO.getIntro();
        this.mobileIntro = goodsOperationDTO.getMobileIntro();
        this.cost = goodsOperationDTO.getCost();
        if (goodsOperationDTO.getGoodsParamsDTOList() != null && goodsOperationDTO.getGoodsParamsDTOList().isEmpty()) {
            this.params = JSONUtil.toJsonStr(goodsOperationDTO.getGoodsParamsDTOList());
        }
        //如果立即上架则
        this.marketEnable = goodsOperationDTO.getRelease() ? GoodsStatusEnum.UPPER.name() : GoodsStatusEnum.DOWN.name();
        this.goodsType = goodsOperationDTO.getGoodsType();

        //循环sku，判定sku是否有效
        for (Map<String, Object> sku : goodsOperationDTO.getSkuList()) {
            //判定参数不能为空
            if (sku.get("sn") == null) {
                throw new ServiceException(ResultCode.GOODS_SKU_SN_ERROR);
            }
            if (StringUtil.isEmpty(sku.get("price").toString()) || Convert.toDouble(sku.get("price")) <= 0) {
                throw new ServiceException(ResultCode.GOODS_SKU_PRICE_ERROR);
            }
            if (StringUtil.isEmpty(sku.get("cost").toString()) || Convert.toDouble(sku.get("cost")) <= 0) {
                throw new ServiceException(ResultCode.GOODS_SKU_COST_ERROR);
            }
            //虚拟商品没有重量字段
            if (sku.containsKey("weight")) {
                if (StringUtil.isEmpty(sku.get("weight").toString()) || Convert.toDouble(sku.get("weight").toString()) < 0) {
                    throw new ServiceException(ResultCode.GOODS_SKU_WEIGHT_ERROR);
                }
            }
            if (StringUtil.isEmpty(sku.get("quantity").toString()) || Convert.toInt(sku.get("quantity").toString()) < 0) {
                throw new ServiceException(ResultCode.GOODS_SKU_QUANTITY_ERROR);
            }

        }
    }
}