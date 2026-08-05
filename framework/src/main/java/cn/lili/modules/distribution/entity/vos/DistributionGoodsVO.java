package cn.lili.modules.distribution.entity.vos;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 分销商品信息
 *
 * @author pikachu
 * @since 2020-03-26 09:04:53
 */
@Data
public class DistributionGoodsVO {

    @Schema(description = "分销商品ID")
    private String id;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "规格")
    private String specs;

    @Schema(description = "库存")
    private Integer quantity;

    @Schema(description = "商品图片")
    private String thumbnail;

    @Schema(description = "商品价格")
    private Double price;

    @Schema(description = "商品编号")
    private String sn;

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "商品类型")
    private String goodsType;

    @JsonIgnore
    @Schema(description = "分类路径")
    private String categoryPath;

    @Schema(description = "平台类目")
    private String platformCategoryName;

    @Schema(description = "品牌")
    private String brandName;

    /**
     * @see cn.lili.modules.distribution.entity.enums.DistributionGoodsAuthEnum
     */
    @Schema(description = "审核状态")
    private String authStatus;

    @Schema(description = "审核备注")
    private String authRemark;

    @Schema(description = "规格ID")
    private String skuId;

    @Schema(description = "店铺名称")
    private String storeName;

    @Schema(description = "佣金金额")
    private Double commission;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "添加时间")
    private Date createTime;

}
