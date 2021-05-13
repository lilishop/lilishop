package cn.lili.modules.purchase.entity.dos;

import cn.lili.base.BaseEntity;
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
 * 报价单
 *
 * @author Chopper
 * @date 2020/11/26 20:43
 */
@Data
@Entity
@ApiModel(value = "供求单报价")
@TableName("li_purchase_quoted")
@Table(name = "li_purchase_quoted")
public class PurchaseQuoted {

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

    @ApiModelProperty(value = "采购单ID")
    private String purchaseOrderId;

    @ApiModelProperty(value = "标题")
    private String title;

    @ApiModelProperty(value = "报价说明")
    private String context;

    @ApiModelProperty(value = "附件")
    private String annex;

    @ApiModelProperty(value = "公司名称")
    private String companyName;

    @ApiModelProperty(value = "联系人")
    private String contacts;

    @ApiModelProperty(value = "联系电话")
    private String contactNumber;

    @ApiModelProperty(value = "报价人")
    private String memberId;

}
