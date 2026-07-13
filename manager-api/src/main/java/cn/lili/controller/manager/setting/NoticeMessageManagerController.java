package cn.lili.controller.manager.setting;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.enums.SwitchEnum;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.message.entity.dos.NoticeMessage;
import cn.lili.modules.message.entity.dto.NoticeMessageDetailDTO;
import cn.lili.modules.message.entity.enums.NoticeMessageParameterEnum;
import cn.lili.modules.message.service.NoticeMessageService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;  
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.EnumUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.List;


/**
 * 管理端,客户站内信管理接口
 *
 * @author Chopper
 * @since 2020/11/17 4:31 下午
 */
@Slf4j
@RestController
@Tag(name = "管理端,客户站内信管理接口")
@RequestMapping("/manager/setting/noticeMessage")
public class NoticeMessageManagerController {
    @Autowired
    private NoticeMessageService noticeMessageService;

    @Operation(summary = "消息模板分页")
    @Parameter(name = "type", description = "消息类型", required = false)
    @GetMapping
    public ResultMessage<IPage<NoticeMessage>> getPage(PageVO pageVO, String type) {
        return ResultUtil.data(noticeMessageService.getMessageTemplate(pageVO, type));
    }


    @Operation(summary = "根据id获取通知详情")
    @Parameter(name = "id", description = "模板id", required = true)
    @GetMapping("/{id}")
    public ResultMessage<NoticeMessageDetailDTO> get(@PathVariable String id) {
        //根据id获取当前消息模板
        NoticeMessage noticeMessage = noticeMessageService.getById(id);
        if (noticeMessage != null) {
            NoticeMessageDetailDTO noticeMessageDetailDTO = new NoticeMessageDetailDTO();
            BeanUtil.copyProperties(noticeMessage, noticeMessageDetailDTO);
            //组织消息变量
            String[] variableNames = noticeMessage.getVariable().split(",");
            //定义返回参数中变量列表
            List<String> variableValues = new ArrayList<>();
            //循环消息变量赋值
            if (variableNames.length > 0) {
                for (String variableName : variableNames) {
                    if (NoticeMessageParameterEnum.getValueByType(variableName) != null) {
                        variableValues.add(NoticeMessageParameterEnum.getValueByType(variableName));
                    }
                }
                noticeMessageDetailDTO.setVariables(variableValues);
            }

            return ResultUtil.data(noticeMessageDetailDTO);
        }
        throw new ServiceException(ResultCode.NOTICE_NOT_EXIST.message());
    }


    @Operation(summary = "修改站内信模板")
    @Parameter(name = "noticeContent", description = "站内信内容", required = true)
    @Parameter(name = "noticeTitle", description = "站内信模板标题", required = true)
    @Parameter(name = "id", description = "模板id", required = true)
    @PutMapping("/{id}")
    public ResultMessage<NoticeMessage> updateNoticeTemplate(@RequestParam String noticeContent,
                                                             @RequestParam String noticeTitle,
                                                             @PathVariable String id) {
        //根据id获取当前消息模板
        NoticeMessage noticeMessage = noticeMessageService.getById(id);
        if (noticeMessage != null) {
            noticeMessage.setNoticeContent(noticeContent);
            noticeMessage.setNoticeTitle(noticeTitle);
            noticeMessageService.updateById(noticeMessage);
            return ResultUtil.data(noticeMessage);
        }
        throw new ServiceException(ResultCode.NOTICE_NOT_EXIST.message());
    }

    @Operation(summary = "修改站内信状态")
    @Parameter(name = "id", description = "站内信状态", required = true)
    @Parameter(name = "status", description = "站内信状态", required = true)
    @PutMapping("/{id}/{status}")
    public ResultMessage<NoticeMessage> status(@PathVariable String id, @PathVariable String status) {
        //根据id获取当前消息模板
        NoticeMessage messageTemplate = noticeMessageService.getById(id);
        if (messageTemplate != null) {
            //校验传递站内信是否符合规范
            Boolean b = EnumUtils.isValidEnum(SwitchEnum.class, status);
            if (b) {
                //赋值执行修改操作
                messageTemplate.setNoticeStatus(status);
                noticeMessageService.updateById(messageTemplate);
                return ResultUtil.data(messageTemplate);
            }
            throw new ServiceException(ResultCode.NOTICE_ERROR);
        }
        throw new ServiceException(ResultCode.NOTICE_NOT_EXIST.message());
    }

    @Operation(summary = "修改邮箱渠道状态")
    @PutMapping("/{id}/email/{status}")
    public ResultMessage<NoticeMessage> emailStatus(@PathVariable String id, @PathVariable String status) {
        NoticeMessage messageTemplate = noticeMessageService.getById(id);
        if (messageTemplate != null) {
            if (EnumUtils.isValidEnum(SwitchEnum.class, status)) {
                messageTemplate.setEmailStatus(status);
                noticeMessageService.updateById(messageTemplate);
                return ResultUtil.data(messageTemplate);
            }
            throw new ServiceException(ResultCode.NOTICE_ERROR);
        }
        throw new ServiceException(ResultCode.NOTICE_NOT_EXIST.message());
    }

}
