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
 * 报价单字内容
 *
 * @author Bulbasaur
 * @since 2020/11/26 20:43
 */
@Data
@ApiModel(value = "供求单报价")
@TableName("li_purchase_quoted_item")
public class PurchaseQuotedItem extends BaseIdEntity {

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建时间", hidden = true)
    private Date createTime;

    @ApiModelProperty(value = "报价单ID")
    private String PurchaseQuotedId;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "规格")
    private String specs;

    @ApiModelProperty(value = "数量")
    private String num;

    @ApiModelProperty(value = "数量单位")
    private String goodsUnit;

    @ApiModelProperty(value = "价格")
    private Double price;
}
