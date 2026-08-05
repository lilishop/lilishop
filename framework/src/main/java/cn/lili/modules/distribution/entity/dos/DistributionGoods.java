package cn.lili.modules.distribution.entity.dos;


import cn.lili.modules.distribution.entity.enums.DistributionGoodsAuthEnum;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.mybatis.BaseIdEntity;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;
import java.util.Map;

/**
 * 分销商品
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
@Data
@Schema(description = "分销商品")
@TableName("li_distribution_goods")
@NoArgsConstructor
@EqualsAndHashCode(callSuper = false)
public class DistributionGoods extends BaseIdEntity {

    private static final long serialVersionUID = 1L;

    @CreatedBy
    @TableField(fill = FieldFill.INSERT)
    @Schema(description = "创建者", hidden = true)
    private String createBy;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @Schema(description = "创建时间", hidden = true)
    private Date createTime;

    @NotNull(message = "商品ID不能为空")
    @Schema(description = "商品ID", requiredMode = Schema.RequiredMode.REQUIRED)
    private String goodsId;

    @Schema(description = "商品名称")
    private String goodsName;

    @NotNull(message = "规格ID不能为空")
    @Schema(description = "规格ID", requiredMode = Schema.RequiredMode.REQUIRED)
    private String skuId;

    @Schema(description = "规格信息json", hidden = true)
    @JsonIgnore
    private String specs;

    @NotNull(message = "店铺ID不能为空")
    @Schema(description = "店铺ID")
    private String storeId;

    @NotNull(message = "店铺名称不能为空")
    @Schema(description = "店铺名称")
    private String storeName;

    @NotNull(message = "佣金金额")
    @Schema(description = "佣金金额", requiredMode = Schema.RequiredMode.REQUIRED)
    private Double commission = 0D;

    @Max(value = 99999999, message = "价格不能超过99999999")
    @Schema(description = "商品价格")
    private Double price;

    @Schema(description = "缩略图路径")
    private String thumbnail;

    /**
     * @see cn.lili.modules.distribution.entity.enums.DistributionGoodsAuthEnum
     */
    @Schema(description = "审核状态")
    @TableField("auth_status")
    private String authStatus;

    @Schema(description = "审核备注")
    @TableField("auth_remark")
    private String authRemark;

    public DistributionGoods(GoodsSku goodsSku, Double commission) {
        this.goodsId = goodsSku.getGoodsId();
        this.goodsName = goodsSku.getGoodsName();
        this.skuId = goodsSku.getId();
        this.specs = "";
        JSONObject jsonObject = JSON.parseObject(goodsSku.getSpecs());
        for (Map.Entry<String, Object> entry : jsonObject.entrySet()) {
            if (!"images".equals(entry.getKey())) {
                this.specs = this.specs + entry.getKey() + ":" + entry.getValue() + " ";
            }
        }

        this.storeId = goodsSku.getStoreId();
        this.storeName = goodsSku.getStoreName();
        this.price = goodsSku.getPrice();
        this.thumbnail = goodsSku.getThumbnail();
        this.commission = commission;
        this.authStatus = DistributionGoodsAuthEnum.TOBEAUDITED.name();
    }

}