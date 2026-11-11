package cn.lili.modules.wxchannels.entity.dto;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.wxchannels.entity.dos.WxChannelRefund;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = false)
public class WxChannelRefundSearchParams extends PageVO {

    @Schema(description = "退单编号")
    private String channelRefundSn;

    @Schema(description = "订单编号")
    private String channelOrderSn;

    @Schema(description = "客户昵称")
    private String memberNickName;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "退单状态")
    private String status;

    @Schema(description = "下单场景 LIVE,WINDOW")
    private String scene;

    @Schema(description = "开始时间")
    private Long startTime;

    @Schema(description = "结束时间")
    private Long endTime;

    public QueryWrapper<WxChannelRefund> queryWrapper() {
        QueryWrapper<WxChannelRefund> wrapper = new QueryWrapper<>();
        wrapper.eq(CharSequenceUtil.isNotEmpty(channelRefundSn), "channel_refund_sn", channelRefundSn);
        wrapper.eq(CharSequenceUtil.isNotEmpty(channelOrderSn), "channel_order_sn", channelOrderSn);
        wrapper.eq(CharSequenceUtil.isNotEmpty(memberNickName), "member_nick_name", memberNickName);
        wrapper.like(CharSequenceUtil.isNotEmpty(goodsName), "goods_name", goodsName);
        wrapper.eq(CharSequenceUtil.isNotEmpty(status), "status", status);
        wrapper.eq(CharSequenceUtil.isNotEmpty(scene), "scene", scene);
        if (startTime != null) {
            wrapper.ge("create_time", new java.util.Date(startTime));
        }
        if (endTime != null) {
            wrapper.le("create_time", new java.util.Date(endTime));
        }
        wrapper.eq("delete_flag", false);
        return wrapper;
    }
}
