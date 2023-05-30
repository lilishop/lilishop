package cn.lili.modules.order.order.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;


/**
 * 交易投诉通信
 *
 * @author paulG
 * @since 2020/12/5
 **/
@Data
@TableName("li_order_complaint_communication")
@ApiModel(value = "订单交易投诉通信")
@AllArgsConstructor
@NoArgsConstructor
public class OrderComplaintCommunication extends BaseEntity {

    private static final long serialVersionUID = -2384351827382795547L;

    /**
     * 投诉id
     */
    @ApiModelProperty(value = "投诉id")
    private String complainId;
    /**
     * 对话内容
     */
    @ApiModelProperty(value = "对话内容")
    private String content;
    /**
     * 所属，买家/卖家
     */
    @ApiModelProperty(value = "所属，买家/卖家")
    private String owner;
    /**
     * 对话所属名称
     */
    @ApiModelProperty(value = "对话所属名称")
    private String ownerName;
    /**
     * 对话所属id,卖家id/买家id
     */
    @ApiModelProperty(value = "对话所属id,卖家id/买家id")
    private String ownerId;



}
