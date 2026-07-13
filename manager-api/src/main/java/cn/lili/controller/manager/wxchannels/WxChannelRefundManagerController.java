package cn.lili.controller.manager.wxchannels;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.wxchannels.entity.dos.WxChannelRefund;
import cn.lili.modules.wxchannels.entity.dto.WxChannelRefundSearchParams;
import cn.lili.modules.wxchannels.service.WxChannelRefundService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

@RestController
@Tag(name = "管理端,微信视频号退单接口")
@RequestMapping("/manager/wxchannels/refund")
public class WxChannelRefundManagerController {

    @Autowired
    private WxChannelRefundService wxChannelRefundService;

    @GetMapping
    @Operation(summary = "分页获取视频号退单")
    @Parameter(name = "channelRefundSn", description = "退单编号")
    @Parameter(name = "channelOrderSn", description = "订单编号")
    @Parameter(name = "memberNickName", description = "客户昵称")
    @Parameter(name = "goodsName", description = "商品名称")
    @Parameter(name = "status", description = "退单状态")
    @Parameter(name = "scene", description = "下单场景 LIVE,WINDOW")
    @Parameter(name = "startTime", description = "开始时间")
    @Parameter(name = "endTime", description = "结束时间")
    public ResultMessage<IPage<WxChannelRefund>> page(WxChannelRefundSearchParams params) {
        return ResultUtil.data(wxChannelRefundService.page(params));
    }

    @GetMapping("/{id}")
    @Operation(summary = "查看退单详情")
    public ResultMessage<WxChannelRefund> get(@PathVariable String id) {
        return ResultUtil.data(wxChannelRefundService.getById(id));
    }
}
