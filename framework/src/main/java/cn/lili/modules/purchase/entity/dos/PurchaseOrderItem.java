package cn.lili.modules.purchase.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 采购单子内容
 *
 * @author Bulbasaur
 * @since 2020/11/26 19:32
 */
@Data
@TableName("li_purchase_order_item")
@ApiModel(value = "采购单子内容")
public class PurchaseOrderItem extends BaseIdEntity {

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建时间", hidden = true)
    private Date createTime;

    @ApiModelProperty(value = "采购ID")
    private String purchaseOrderId;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "数量")
    private String num;

    @ApiModelProperty(value = "数量单位")
    private String goodsUnit;

    @ApiModelProperty(value = "价格")
    private Double price;

    @ApiModelProperty(value = "规格")
    private String specs;

    @ApiModelProperty(value = "图片")
    private String images;


}
