package cn.lili.modules.order.order.entity.vo;

import cn.hutool.core.util.StrUtil;
import cn.lili.modules.order.order.entity.dos.OrderComplaintCommunication;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 订单投诉搜索参数
 *
 * @author paulG
 * @since 2020/12/5
 **/
@Data
public class OrderComplaintCommunicationSearchParams {

    /**
     * 投诉id
     */
    @ApiModelProperty(value = "投诉id")
    private String complainId;

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

    public LambdaQueryWrapper<OrderComplaintCommunication> lambdaQueryWrapper() {
        LambdaQueryWrapper<OrderComplaintCommunication> queryWrapper = new LambdaQueryWrapper<>();
        if (StrUtil.isNotEmpty(complainId)) {
            queryWrapper.eq(OrderComplaintCommunication::getComplainId, complainId);
        }
        if (StrUtil.isNotEmpty(owner)) {
            queryWrapper.eq(OrderComplaintCommunication::getOwner, owner);
        }
        if (StrUtil.isNotEmpty(ownerName)) {
            queryWrapper.eq(OrderComplaintCommunication::getOwnerName, ownerName);
        }
        if (StrUtil.isNotEmpty(ownerId)) {
            queryWrapper.eq(OrderComplaintCommunication::getOwnerId, ownerId);
        }
        return queryWrapper;
    }

}
