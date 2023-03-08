package cn.lili.controller.im;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.im.entity.dos.ImTalk;
import cn.lili.modules.im.entity.vo.ImTalkVO;
import cn.lili.modules.im.service.ImTalkService;
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
@Api(tags = "聊天接口")
@RequestMapping("/im/talk")
@Transactional(rollbackFor = Exception.class)
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class ImTalkController {

    private final ImTalkService imTalkService;

    @GetMapping(value = "/{id}")
    @ApiOperation(value = "查看聊天详情")
    public ResultMessage<ImTalk> get(@PathVariable String id) {

        ImTalk imTalk = imTalkService.getById(id);
        return ResultUtil.data(imTalk);
    }

    @GetMapping(value = "/user/{uid}")
    @ApiOperation(value = "查看与某人聊天详情")
    public ResultMessage<ImTalk> getUser(@PathVariable String uid) {
        //通过长度判断,保证每次都是同一个聊天
        return ResultUtil.data(imTalkService.getTalkByUser(uid));
    }

    @GetMapping(value = "/by/user/{userId}")
    @ApiOperation(value = "查看与某人聊天详情")
    public ResultMessage<ImTalkVO> getByUser(@PathVariable String userId) {
        return ResultUtil.data(new ImTalkVO(imTalkService.getTalkByUser(userId),userId));
    }

    @GetMapping(value = "/top")
    @ApiOperation(value = "查看与某人聊天详情")
    public ResultMessage<Object> top(String id, Boolean top) {
        imTalkService.top(id, top);
        return ResultUtil.success();
    }

    @GetMapping("/list")
    @ApiOperation(value = "分页获取用户聊天")
    public ResultMessage<List<ImTalkVO>> getUserTalkList() {
        return ResultUtil.data(imTalkService.getUserTalkList());
    }

    @GetMapping("/store/list")
    @ApiOperation(value = "分页获取商家聊天")
    public ResultMessage<List<ImTalkVO>> getStoreTalkList() {
        return ResultUtil.data(imTalkService.getStoreTalkList());
    }

    @DeleteMapping(value = "/{id}")
    @ApiOperation(value = "删除聊天")
    public ResultMessage<Object> disable(@PathVariable String id) {
        imTalkService.disable(id);
        return ResultUtil.success(ResultCode.SUCCESS);
    }
}
