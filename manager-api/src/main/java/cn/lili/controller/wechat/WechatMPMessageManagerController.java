package cn.lili.controller.wechat;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.wechat.entity.dos.WechatMPMessage;
import cn.lili.modules.wechat.service.WechatMPMessageService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * @author Chopper
 */
@RestController
@Api(tags = "微信小程序消息订阅接口")
@RequestMapping("/manager/message/wechatMPMessage")
@Transactional(rollbackFor = Exception.class)
public class WechatMPMessageManagerController {
    @Autowired
    private WechatMPMessageService wechatMPMessageService;

    @GetMapping(value = "/init")
    @ApiOperation(value = "初始化微信小程序消息订阅")
    public ResultMessage init() {
        wechatMPMessageService.init();
        return ResultUtil.success();
    }

    @GetMapping(value = "/{id}")
    @ApiOperation(value = "查看微信小程序消息订阅详情")
    public ResultMessage<WechatMPMessage> get(@PathVariable String id) {

        WechatMPMessage wechatMPMessage = wechatMPMessageService.getById(id);
        return new ResultUtil<WechatMPMessage>().setData(wechatMPMessage);
    }

    @GetMapping
    @ApiOperation(value = "分页获取微信小程序消息订阅")
    public ResultMessage<IPage<WechatMPMessage>> getByPage(WechatMPMessage entity,
                                                           SearchVO searchVo,
                                                           PageVO page) {
        IPage<WechatMPMessage> data = wechatMPMessageService.page(PageUtil.initPage(page), PageUtil.initWrapper(entity, searchVo));
        return new ResultUtil<IPage<WechatMPMessage>>().setData(data);
    }

    @PostMapping
    @ApiOperation(value = "新增微信小程序消息订阅")
    public ResultMessage<WechatMPMessage> save(WechatMPMessage wechatMPMessage) {

        wechatMPMessageService.save(wechatMPMessage);
        return new ResultUtil<WechatMPMessage>().setData(wechatMPMessage);
    }

    @PutMapping("/{id}")
    @ApiOperation(value = "更新微信小程序消息订阅")
    public ResultMessage<WechatMPMessage> update(@PathVariable String id, WechatMPMessage wechatMPMessage) {
        wechatMPMessageService.updateById(wechatMPMessage);
        return new ResultUtil<WechatMPMessage>().setData(wechatMPMessage);
    }

    @DeleteMapping(value = "/{ids}")
    @ApiOperation(value = "删除微信小程序消息订阅")
    public ResultMessage<Object> delAllByIds(@PathVariable List ids) {

        wechatMPMessageService.removeByIds(ids);
        return ResultUtil.success();
    }
}
