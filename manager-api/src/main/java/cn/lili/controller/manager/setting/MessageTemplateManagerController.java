package cn.lili.controller.manager.setting;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.message.entity.dto.MessageTemplateAggregateDTO;
import cn.lili.modules.message.service.NoticeMessageService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 统一消息模板管理（站内信、短信、微信服务号、微信小程序订阅消息矩阵式维护）
 */
@RestController
@Tag(name = "管理端,消息模板聚合管理")
@RequestMapping("/manager/setting/messageTemplate")
public class MessageTemplateManagerController {

    @Autowired
    private NoticeMessageService noticeMessageService;

    @Operation(summary = "消息模板分页（聚合站内信、短信与微信模板）")
    @Parameter(name = "type", description = "模板受众类型: MEMBER/STORE/PLATFORM", required = false)
    @GetMapping("/page")
    public ResultMessage<IPage<MessageTemplateAggregateDTO>> page(PageVO pageVO, String type) {
        return ResultUtil.data(noticeMessageService.getMessageTemplateAggregatePage(pageVO, type));
    }

    @Operation(summary = "快速切换渠道开关")
    @PutMapping("/switch")
    public ResultMessage<Boolean> updateSwitch(@RequestParam String id,
                                               @RequestParam String channel,
                                               @RequestParam String status) {
        boolean ok = noticeMessageService.updateChannelSwitch(id, channel, status);
        if (!ok) {
            throw new ServiceException(ResultCode.ERROR);
        }
        return ResultUtil.data(true);
    }

    @Operation(summary = "保存站内信配置")
    @PutMapping("/notice")
    public ResultMessage<Boolean> updateNotice(@RequestParam String id,
                                               @RequestParam(required = false) String title,
                                               @RequestParam String content,
                                               @RequestParam(required = false) String status) {
        boolean ok = noticeMessageService.updateNoticeTemplate(id, title, content, status);
        if (!ok) {
            throw new ServiceException(ResultCode.NOTICE_NOT_EXIST);
        }
        return ResultUtil.data(true);
    }

    @Operation(summary = "保存短信模板ID与开关(短信正文只读不可编辑)")
    @PutMapping("/sms")
    public ResultMessage<Boolean> updateSms(@RequestParam String id,
                                            @RequestParam String smsTemplateCode,
                                            @RequestParam(required = false) String status) {
        boolean ok = noticeMessageService.updateSmsTemplate(id, smsTemplateCode, status);
        if (!ok) {
            throw new ServiceException(ResultCode.NOTICE_NOT_EXIST);
        }
        return ResultUtil.data(true);
    }

    @Operation(summary = "保存微信公众号服务号模板ID与开关(正文只读)")
    @PutMapping("/wechat-oa")
    public ResultMessage<Boolean> updateWechatOa(@RequestParam String id,
                                                 @RequestParam String code,
                                                 @RequestParam(required = false) Boolean enable) {
        boolean ok = noticeMessageService.updateWechatOaTemplate(id, code, enable);
        if (!ok) {
            throw new ServiceException(ResultCode.NOTICE_NOT_EXIST);
        }
        return ResultUtil.data(true);
    }

    @Operation(summary = "保存微信小程序订阅消息模板ID与开关(正文只读)")
    @PutMapping("/wechat-mp")
    public ResultMessage<Boolean> updateWechatMp(@RequestParam String id,
                                                 @RequestParam String code,
                                                 @RequestParam(required = false) Boolean enable) {
        boolean ok = noticeMessageService.updateWechatMpTemplate(id, code, enable);
        if (!ok) {
            throw new ServiceException(ResultCode.NOTICE_NOT_EXIST);
        }
        return ResultUtil.data(true);
    }

}
