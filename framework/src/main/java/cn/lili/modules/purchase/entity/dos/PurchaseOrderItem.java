package cn.lili.modules.purchase.entity.dos;

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
 * 采购单子内容
 *
 * @author Bulbasaur
 * @date 2020/11/26 19:32
 */
@Data
@Entity
@Table(name = "li_purchase_order_item")
@TableName("li_purchase_order_item")
@ApiModel(value = "采购单子内容")
public class PurchaseOrderItem {

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
    @Column(columnDefinition = "TEXT")
    private String specs;

    @ApiModelProperty(value = "图片")
    private String images;


}
