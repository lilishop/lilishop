package cn.lili.modules.purchase.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * 供求单
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
@Data
@ApiModel(value = "供求单")
@TableName("li_purchase_order")
public class PurchaseOrder extends BaseEntity {

    @ApiModelProperty(value = "标题")
    private String title;

    @ApiModelProperty(value = "截止时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date deadline;

    @ApiModelProperty(value = "收货时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date receiptTime;

    @ApiModelProperty(value = "价格类型", notes = "可议价、不可议价、面议")
    private String priceMethod;

    @ApiModelProperty(value = "地址名称， '，'分割")
    private String consigneeAddressPath;

    @ApiModelProperty(value = "地址id，'，'分割 ")
    private String consigneeAddressIdPath;

    @ApiModelProperty(value = "是否需要发票")
    private Boolean needReceipt;

    @ApiModelProperty(value = "补充说明")
    private String supplement;

    @ApiModelProperty(value = "联系类型", notes = "联系方式什么时候可见 公开后、公开")
    private String contactType;

    @ApiModelProperty(value = "联系人")
    private String contacts;

    @ApiModelProperty(value = "联系电话")
    private String contactNumber;

    @ApiModelProperty(value = "供求人")
    private String memberId;

    @ApiModelProperty(value = "状态，开启：OPEN，关闭：CLOSE")
    private String status;

    @ApiModelProperty(value = "分类ID")
    private String categoryId;

    @ApiModelProperty(value = "分类名称")
    private String categoryName;

}