package cn.lili.controller.im;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.im.entity.dos.ImTalk;
import cn.lili.modules.im.entity.dto.IMTalkQueryParams;
import cn.lili.modules.im.entity.vo.ImTalkVO;
import cn.lili.modules.im.service.ImTalkService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 聊天会话接口，提供会话创建、查询、置顶与关闭，并对会话参与方身份进行校验
 *
 * @author Chopper
 */
@RestController
@Tag(name = "聊天接口")
@RequestMapping("/im/talk")
@Transactional(rollbackFor = Exception.class)
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class ImTalkController {

    private final ImTalkService imTalkService;

    /**
     * 获取当前登录主体的IM用户标识
     * 买家返回用户ID，商家坐席返回店铺ID
     *
     * @param currentUser 当前登录用户
     * @return IM主体用户标识
     */
    private String getImUserId(AuthUser currentUser) {
        if (UserEnums.STORE.equals(currentUser.getRole())) {
            return currentUser.getStoreId() != null ? currentUser.getStoreId() : currentUser.getId();
        }
        return currentUser.getId();
    }

    @Operation(summary = "查看聊天详情")
    @Parameter(name = "id", description = "聊天ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<ImTalk> get(@PathVariable String id) {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        ImTalk imTalk = imTalkService.getById(id);
        if (imTalk == null) {
            return ResultUtil.data(null);
        }
        // 校验归属权限：只有会话参与双方或管理员允许查看会话元信息
        if (!UserEnums.MANAGER.equals(currentUser.getRole())) {
            String imUserId = getImUserId(currentUser);
            if (!imUserId.equals(imTalk.getUserId1()) && !imUserId.equals(imTalk.getUserId2())) {
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
            }
        }
        return ResultUtil.data(imTalk);
    }

    @Operation(summary = "查看与某人聊天详情")
    @Parameter(name = "uid", description = "用户ID", required = true)
    @GetMapping("/user/{uid}")
    public ResultMessage<ImTalk> getUser(@PathVariable String uid) {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        return ResultUtil.data(imTalkService.getTalkByUser(uid));
    }

    @Operation(summary = "查看与某人聊天详情")
    @Parameter(name = "userId", description = "用户ID", required = true)
    @GetMapping("/by/user/{userId}")
    public ResultMessage<ImTalkVO> getByUser(@PathVariable String userId) {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        return ResultUtil.data(imTalkService.getTalkByUserId(userId));
    }

    @Operation(summary = "设置聊天置顶")
    @Parameter(name = "id", description = "聊天ID", required = true)
    @Parameter(name = "top", description = "是否置顶", required = true)
    @GetMapping("/top")
    public ResultMessage<Object> top(String id, Boolean top) {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        ImTalk imTalk = imTalkService.getById(id);
        if (imTalk == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        if (!UserEnums.MANAGER.equals(currentUser.getRole())) {
            String imUserId = getImUserId(currentUser);
            if (!imUserId.equals(imTalk.getUserId1()) && !imUserId.equals(imTalk.getUserId2())) {
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
            }
        }
        imTalkService.top(id, top);
        return ResultUtil.success();
    }

    @Operation(summary = "分页获取用户聊天")
    @Parameter(name = "imTalkQueryParams", description = "聊天查询参数", required = true)
    @GetMapping("/list")
    public ResultMessage<List<ImTalkVO>> getUserTalkList(IMTalkQueryParams imTalkQueryParams) {
        return ResultUtil.data(imTalkService.getUserTalkList(imTalkQueryParams));
    }

    @Operation(summary = "分页获取商家聊天")
    @Parameter(name = "imTalkQueryParams", description = "聊天查询参数", required = true)
    @GetMapping("/store/list")
    public ResultMessage<List<ImTalkVO>> getStoreTalkList(IMTalkQueryParams imTalkQueryParams) {
        return ResultUtil.data(imTalkService.getStoreTalkList(imTalkQueryParams));
    }

    @Operation(summary = "删除聊天")
    @Parameter(name = "id", description = "聊天ID", required = true)
    @DeleteMapping("/{id}")
    public ResultMessage<Object> disable(@PathVariable String id) {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        ImTalk imTalk = imTalkService.getById(id);
        if (imTalk == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        if (!UserEnums.MANAGER.equals(currentUser.getRole())) {
            String imUserId = getImUserId(currentUser);
            if (!imUserId.equals(imTalk.getUserId1()) && !imUserId.equals(imTalk.getUserId2())) {
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
            }
        }
        imTalkService.disable(id);
        return ResultUtil.success(ResultCode.SUCCESS);
    }
}
