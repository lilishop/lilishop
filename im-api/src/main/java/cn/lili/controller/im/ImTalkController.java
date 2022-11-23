package cn.lili.controller.im;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.im.entity.dos.ImTalk;
import cn.lili.modules.im.entity.vo.ImTalkVO;
import cn.lili.modules.im.service.ImTalkService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import io.swagger.annotations.Api;
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
@RequestMapping("/lili/imTalk")
@Transactional(rollbackFor = Exception.class)
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class ImTalkController {

    private final ImTalkService imTalkService;

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

    @GetMapping(value = "/top")
    @ApiOperation(value = "查看与某人聊天详情")
    public ResultMessage top(String id, Boolean top) {
        imTalkService.top(id, top);
        return ResultUtil.success();
    }

    @GetMapping("/list")
    @ApiOperation(value = "分页获取聊天")
    public ResultMessage<List<ImTalkVO>> getByPage() {
        AuthUser authUser = UserContext.getCurrentUser();
        LambdaQueryWrapper<ImTalk> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(ImTalk::getUserId1, authUser.getId()).or().eq(ImTalk::getUserId2, authUser.getId());
        List<ImTalk> imTalks = imTalkService.list(queryWrapper);

        List<ImTalkVO> results = imTalks.stream().map(imTalk -> {
            return new ImTalkVO(imTalk, authUser.getId());
        }).collect(Collectors.toList());

        return ResultUtil.data(results);
    }

    @GetMapping("/store/list")
    @ApiOperation(value = "分页获取商家聊天")
    public ResultMessage<List<ImTalkVO>> getStoreTalkList() {
        AuthUser authUser = UserContext.getCurrentUser();
        LambdaQueryWrapper<ImTalk> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(ImTalk::getUserId1, authUser.getStoreId()).or().eq(ImTalk::getUserId2, authUser.getStoreId());
        List<ImTalk> imTalks = imTalkService.list(queryWrapper);

        List<ImTalkVO> results = imTalks.stream().map(imTalk -> {
            return new ImTalkVO(imTalk, authUser.getStoreId());
        }).collect(Collectors.toList());

        return ResultUtil.data(results);
    }

    @DeleteMapping(value = "/{id}")
    @ApiOperation(value = "删除聊天")
    public ResultMessage<Object> disable(@PathVariable String id) {
        imTalkService.disable(id);
        return ResultUtil.success(ResultCode.SUCCESS);
    }
}
