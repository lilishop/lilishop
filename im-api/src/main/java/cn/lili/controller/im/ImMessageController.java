package cn.lili.controller.im;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.im.entity.dos.ImMessage;
import cn.lili.modules.im.entity.dto.MessageQueryParams;
import cn.lili.modules.im.service.ImMessageService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * @author Chopper
 */
@RestController
@Api(tags = "Im消息接口")
@RequestMapping("/im/message")
@Transactional(rollbackFor = Exception.class)
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class ImMessageController {

    private final ImMessageService imMessageService;

    @GetMapping(value = "/{id}")
    @ApiOperation(value = "查看Im消息详情")
    public ResultMessage<ImMessage> get(@PathVariable String id) {
        ImMessage imMessage = imMessageService.getById(id);
        return new ResultUtil<ImMessage>().setData(imMessage);
    }

    @GetMapping
    @ApiOperation(value = "分页获取Im消息")
    public ResultMessage<List<ImMessage>> historyMessage(MessageQueryParams messageQueryParams) {
        List<ImMessage> data = imMessageService.getList(messageQueryParams);
        return new ResultUtil<List<ImMessage>>().setData(data);
    }

    @PostMapping
    @ApiOperation(value = "新增Im消息")
    public ResultMessage<ImMessage> save(ImMessage imMessage) {
        if (imMessageService.save(imMessage)) {
            return new ResultUtil<ImMessage>().setData(imMessage);
        }
        return new ResultUtil<ImMessage>().setErrorMsg(ResultCode.ERROR);
    }

    @PutMapping("/{id}")
    @ApiOperation(value = "更新Im消息")
    public ResultMessage<ImMessage> update(@PathVariable String id, ImMessage imMessage) {
        if (imMessageService.updateById(imMessage)) {
            return new ResultUtil<ImMessage>().setData(imMessage);
        }
        return new ResultUtil<ImMessage>().setErrorMsg(ResultCode.ERROR);
    }

    @DeleteMapping(value = "/{ids}")
    @ApiOperation(value = "删除Im消息")
    public ResultMessage<Object> delAllByIds(@PathVariable List ids) {
        imMessageService.removeByIds(ids);
        return ResultUtil.success(ResultCode.SUCCESS);
    }


    @GetMapping(value = "/newMessage")
    @ApiOperation(value = "查看是否有新消息")
    public ResultMessage<Boolean> hasNewMessage(String accessToken) {
        return ResultUtil.data(imMessageService.hasNewMessage(accessToken));
    }

    @GetMapping(value = "/unredMessage")
    @ApiOperation(value = "获取所有未读消息")
    public ResultMessage<Long> getUnreadMessageCount() {
        return ResultUtil.data(imMessageService.unreadMessageCount());
    }

    @PutMapping(value = "/clean/unred")
    @ApiOperation(value = "清除所有未读消息")
    public ResultMessage<Object> cleanUnreadMessage() {
        imMessageService.cleanUnreadMessage();
        return ResultUtil.success();
    }
}
