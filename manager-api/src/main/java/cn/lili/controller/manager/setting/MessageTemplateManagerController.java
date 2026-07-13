package cn.lili.controller.manager.setting;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.message.entity.dto.MessageTemplateAggregateDTO;
import cn.lili.modules.message.service.NoticeMessageService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 客户消息模板聚合分页（站内信 + 微信分表，scene_code 关联）。
 */
@RestController
@Tag(name = "管理端,消息模板聚合")
@RequestMapping("/manager/setting/messageTemplate")
public class MessageTemplateManagerController {

    @Autowired
    private NoticeMessageService noticeMessageService;

    @Operation(summary = "消息模板分页（聚合站内信与微信模板）")
    @Parameter(name = "type", description = "消息类型", required = false)
    @GetMapping("/page")
    public ResultMessage<IPage<MessageTemplateAggregateDTO>> page(PageVO pageVO, String type) {
        return ResultUtil.data(noticeMessageService.getMessageTemplateAggregatePage(pageVO, type));
    }
}
