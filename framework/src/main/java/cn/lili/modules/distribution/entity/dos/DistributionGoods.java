package cn.lili.modules.distribution.entity.dos;

import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import javax.validation.constraints.Max;
import javax.validation.constraints.NotNull;
import java.util.Date;
import java.util.Map;

/**
 * 分销商品
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
@Data
@ApiModel(value = "分销商品")
@TableName("li_distribution_goods")
@NoArgsConstructor
public class DistributionGoods extends BaseIdEntity {

    private static final long serialVersionUID = 1L;

    @CreatedBy
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建者", hidden = true)
    private String createBy;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建时间", hidden = true)
    private Date createTime;

    @NotNull(message = "商品ID不能为空")
    @ApiModelProperty(value = "商品ID", required = true)
    private String goodsId;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @NotNull(message = "规格ID不能为空")
    @ApiModelProperty(value = "规格ID", required = true)
    private String skuId;

    @ApiModelProperty(value = "规格信息json", hidden = true)
    @JsonIgnore
    private String specs;

    @NotNull(message = "店铺ID不能为空")
    @ApiModelProperty(value = "店铺ID")
    private String storeId;

    @NotNull(message = "店铺名称不能为空")
    @ApiModelProperty(value = "店铺名称")
    private String storeName;

    @NotNull(message = "佣金金额")
    @ApiModelProperty(value = "佣金金额", required = true)
    private Double commission = 0D;

    @Max(value = 99999999, message = "价格不能超过99999999")
    @ApiModelProperty(value = "商品价格")
    private Double price;

    @ApiModelProperty(value = "缩略图路径")
    private String thumbnail;

    @Max(value = 99999999, message = "库存不能超过99999999")
    @ApiModelProperty(value = "库存")
    private Integer quantity;

    public DistributionGoods(GoodsSku goodsSku, Double commission) {
        this.goodsId = goodsSku.getGoodsId();
        this.goodsName = goodsSku.getGoodsName();
        this.skuId = goodsSku.getId();
        this.specs = "";
        JSONObject jsonObject = JSONUtil.parseObj(goodsSku.getSpecs());
        for (Map.Entry<String, Object> entry : jsonObject.entrySet()) {
            if (!"images".equals(entry.getKey())) {
                this.specs = this.specs + entry.getKey() + ":" + entry.getValue() + " ";
            }
        }

        this.storeId = goodsSku.getStoreId();
        this.storeName = goodsSku.getStoreName();
        this.price = goodsSku.getPrice();
        this.thumbnail = goodsSku.getThumbnail();
        this.quantity = goodsSku.getQuantity();
        this.commission = commission;
    }

}