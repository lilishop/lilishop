package cn.lili.controller.im;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.im.entity.dos.ImTalk;
import cn.lili.modules.im.entity.vo.ImTalkVO;
import cn.lili.modules.im.service.ImTalkService;
import cn.lili.modules.store.service.StoreService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.stream.Collectors;


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

    @Autowired
    private StoreService storeService;

    @GetMapping(value = "/{id}")
    @ApiOperation(value = "查看聊天详情")
    public ResultMessage<ImTalk> get(@PathVariable String id) {

        ImTalk imTalk = imTalkService.getById(id);
        return new ResultUtil<ImTalk>().setData(imTalk);
    }

    @GetMapping(value = "/user/{uid}")
    @ApiOperation(value = "查看与某人聊天详情")
    public ResultMessage<ImTalk> getUser(@PathVariable String uid) {
        AuthUser authUser = UserContext.getCurrentUser();
        return ResultUtil.data(imTalkService.getTalkByUser(authUser.getId(), uid));
    }

    @GetMapping(value = "/by/user/{userId}")
    @ApiOperation(value = "查看与某人聊天详情")
    public ResultMessage<ImTalkVO> getByUser(@PathVariable String userId) {
        AuthUser authUser = UserContext.getCurrentUser();
        return ResultUtil.data(new ImTalkVO(imTalkService.getTalkByUser(authUser.getId(), userId), authUser.getId()));
    }

    @GetMapping(value = "/top")
    @ApiOperation(value = "查看与某人聊天详情")
    public ResultMessage top(String id, Boolean top) {
        imTalkService.top(id, top);
        return ResultUtil.success();
    }

    @GetMapping("/list")
    @ApiOperation(value = "分页获取聊天")
    @ApiImplicitParam(name = "userName", value = "用户名称", paramType = "query", dataType = "String")
    public ResultMessage<List<ImTalkVO>> getUserTalkList(String userName) {
        return ResultUtil.data(imTalkService.getUserTalkList(userName));
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
