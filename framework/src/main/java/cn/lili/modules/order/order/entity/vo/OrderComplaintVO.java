package cn.lili.modules.order.order.entity.vo;

import cn.hutool.core.bean.BeanUtil;
import cn.lili.modules.order.order.entity.dos.OrderComplaint;
import cn.lili.modules.order.order.entity.dos.OrderComplaintCommunication;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * 订单交易投诉VO
 *
 * @author paulG
 * @since 2020/12/4
 **/
@Data
@NoArgsConstructor
@AllArgsConstructor
public class OrderComplaintVO extends OrderComplaint {


    private static final long serialVersionUID = -7013465343480854816L;

    @ApiModelProperty(value = "投诉对话")
    private List<OrderComplaintCommunication> orderComplaintCommunications;

    @ApiModelProperty(value = "投诉图片")
    private String[] orderComplaintImages;

    @ApiModelProperty(value = "申诉商家上传的图片")
    private String[] appealImagesList;

    public OrderComplaintVO(OrderComplaint orderComplaint) {
        BeanUtil.copyProperties(orderComplaint, this);
    }
}
