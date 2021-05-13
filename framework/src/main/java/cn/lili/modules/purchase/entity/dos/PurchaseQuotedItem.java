package cn.lili.modules.purchase.entity.dos;

import cn.lili.common.utils.SnowFlake;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Date;

/**
 * 报价单字内容
 *
 * @author Bulbasaur
 * @date 2020/11/26 20:43
 */
@Data
@Entity
@ApiModel(value = "供求单报价")
@TableName("li_purchase_quoted_item")
@Table(name = "li_purchase_quoted_item")
public class PurchaseQuotedItem {

    @Id
    @TableId
    @TableField
    @Column(columnDefinition = "bigint(20)")
    @ApiModelProperty(value = "唯一标识", hidden = true)
    private String id;

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
    @Column(columnDefinition = "TEXT")
    private String specs;

    @ApiModelProperty(value = "数量")
    private String num;

    @ApiModelProperty(value = "数量单位")
    private String goodsUnit;

    @ApiModelProperty(value = "价格")
    private Double price;
}
