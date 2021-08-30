package cn.lili.modules.order.order.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * 发票
 *
 * @author Bulbasaur
 * @since 2020/11/28 11:38
 */
@Data
@TableName("li_receipt")
@ApiModel(value = "发票")
public class Receipt extends BaseEntity {

    private static final long serialVersionUID = -8210927482915675995L;

    @ApiModelProperty(value = "订单编号")
    private String orderSn;

    @ApiModelProperty(value = "发票抬头")
    private String receiptTitle;

    @ApiModelProperty(value = "纳税人识别号")
    private String taxpayerId;

    @ApiModelProperty(value = "发票内容")
    private String receiptContent;

    @ApiModelProperty(value = "发票金额")
    private Double receiptPrice;

    @ApiModelProperty(value = "会员ID")
    private String memberId;

    @ApiModelProperty(value = "会员名称")
    private String memberName;

    @ApiModelProperty(value = "商家ID")
    private String storeId;

    @ApiModelProperty(value = "商家名称")
    private String storeName;

    @ApiModelProperty(value = "发票状态 0未开 1已开")
    private Integer receiptStatus;

    @ApiModelProperty(value = "发票详情")
    private String receiptDetail;

}
