package cn.lili.controller.manager.wxchannels;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.wxchannels.entity.dos.WxChannelGoods;
import cn.lili.modules.wxchannels.entity.dto.WxChannelGoodsSearchParams;
import cn.lili.modules.wxchannels.service.WxChannelGoodsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

@RestController
@Tag(name = "管理端,微信视频号商品接口")
@RequestMapping("/manager/wxchannels/goods")
public class WxChannelGoodsManagerController {

    @Autowired
    private WxChannelGoodsService wxChannelGoodsService;

    @GetMapping
    @Operation(summary = "分页获取视频号商品")
    @Parameter(name = "goodsName", description = "商品名称")
    @Parameter(name = "status", description = "状态 APPROVED,PENDING,REJECTED")
    public ResultMessage<IPage<WxChannelGoods>> page(WxChannelGoodsSearchParams params) {
        return ResultUtil.data(wxChannelGoodsService.page(params));
    }

    @PutMapping("/{id}")
    @Operation(summary = "编辑视频号商品")
    public ResultMessage<WxChannelGoods> update(@PathVariable String id, @RequestBody WxChannelGoods body) {
        body.setId(id);
        wxChannelGoodsService.updateById(body);
        return ResultUtil.data(body);
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "删除视频号商品")
    public ResultMessage<Object> delete(@PathVariable String id) {
        wxChannelGoodsService.removeById(id);
        return ResultUtil.success();
    }
}
