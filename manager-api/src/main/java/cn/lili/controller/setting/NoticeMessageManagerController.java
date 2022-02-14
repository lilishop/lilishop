package cn.lili.controller.setting;

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
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.EnumUtils;
import org.elasticsearch.ResourceNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.List;


/**
 * 管理端,会员站内信管理接口
 *
 * @author Chopper
 * @since 2020/11/17 4:31 下午
 */
@Slf4j
@RestController
@Api(tags = "管理端,会员站内信管理接口")
@RequestMapping("/manager/setting/noticeMessage")
public class NoticeMessageManagerController {
    @Autowired
    private NoticeMessageService noticeMessageService;

    @ApiOperation(value = "消息模板分页")
    @GetMapping
    @ApiImplicitParams({
            @ApiImplicitParam(name = "type", value = "消息类型", dataType = "String", paramType = "query",
                    allowableValues = "MEMBER,OTHER,STORE,WECHAT", allowMultiple = true)
    })
    public ResultMessage<IPage<NoticeMessage>> getPage(PageVO pageVO, String type) {
        return ResultUtil.data(noticeMessageService.getMessageTemplate(pageVO, type));
    }


    @ApiOperation(value = "根据id获取通知详情")
    @ApiImplicitParam(name = "id", value = "模板id", dataType = "String", paramType = "path")
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
        throw new ResourceNotFoundException(ResultCode.NOTICE_NOT_EXIST.message());
    }


    @ApiOperation(value = "修改站内信模板")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "noticeContent", value = "站内信内容", dataType = "String", paramType = "query"),
            @ApiImplicitParam(name = "noticeTitle", value = "站内信模板标题", dataType = "String", paramType = "query"),
            @ApiImplicitParam(name = "id", value = "模板id", dataType = "String", paramType = "path")
    })
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
        throw new ResourceNotFoundException(ResultCode.NOTICE_NOT_EXIST.message());
    }

    @ApiOperation(value = "修改站内信状态")

    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "站内信状态", dataType = "String", paramType = "path"),
            @ApiImplicitParam(name = "status", value = "站内信状态", dataType = "String", paramType = "path")
    })
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
        throw new ResourceNotFoundException(ResultCode.NOTICE_NOT_EXIST.message());
    }

}
