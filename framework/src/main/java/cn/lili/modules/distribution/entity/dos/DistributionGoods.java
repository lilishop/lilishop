package cn.lili.modules.distribution.entity.dos;

import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.Max;
import javax.validation.constraints.NotNull;
import java.util.Date;
import java.util.Map;

/**
 * 分销商品
 *
 * @author pikachu
 * @date 2020-03-14 23:04:56
 */
@Data
@Entity
@ApiModel(value = "分销商品")
@Table(name = "li_distribution_goods")
@TableName("li_distribution_goods")
@NoArgsConstructor
public class DistributionGoods {

    private static final long serialVersionUID = 1L;

    @Id
    @TableId
    @TableField
    @Column(columnDefinition = "bigint(20)")
    @ApiModelProperty(value = "唯一标识", hidden = true)
    private String id;

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
    @Column(columnDefinition = "TEXT")
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