package cn.lili.controller.im;

import cn.hutool.core.util.StrUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.im.entity.dos.ImMessage;
import cn.lili.modules.im.entity.dos.ImTalk;
import cn.lili.modules.im.entity.dto.MessageQueryParams;
import cn.lili.modules.im.service.ImMessageService;
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
 * IM消息接口，提供消息查询、发送、更新与撤销等操作，并对会话主体及权限进行严格校验
 *
 * @author Chopper
 */
@RestController
@Tag(name = "Im消息接口")
@RequestMapping("/im/message")
@Transactional(rollbackFor = Exception.class)
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class ImMessageController {

    private final ImMessageService imMessageService;
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

    @Operation(summary = "查看Im消息详情")
    @Parameter(name = "id", description = "Im消息ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<ImMessage> get(@PathVariable String id) {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        ImMessage imMessage = imMessageService.getById(id);
        if (imMessage == null) {
            return ResultUtil.data(null);
        }
        // 校验归属权限：只有消息的发送方、接收方或平台管理员有权查看
        if (!UserEnums.MANAGER.equals(currentUser.getRole())) {
            String imUserId = getImUserId(currentUser);
            if (!imUserId.equals(imMessage.getFromUser()) && !imUserId.equals(imMessage.getToUser())) {
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
            }
        }
        return ResultUtil.data(imMessage);
    }

    @Operation(summary = "分页获取Im消息")
    @Parameter(name = "messageQueryParams", description = "查询参数", required = true)
    @GetMapping
    public ResultMessage<List<ImMessage>> historyMessage(MessageQueryParams messageQueryParams) {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        // 校验聊天会话参与权：只有会话双方或管理员允许获取历史记录
        if (!UserEnums.MANAGER.equals(currentUser.getRole())) {
            if (StrUtil.isEmpty(messageQueryParams.getTalkId())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR);
            }
            ImTalk talk = imTalkService.getById(messageQueryParams.getTalkId());
            if (talk == null) {
                throw new ServiceException(ResultCode.PARAMS_ERROR);
            }
            String imUserId = getImUserId(currentUser);
            if (!imUserId.equals(talk.getUserId1()) && !imUserId.equals(talk.getUserId2())) {
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
            }
        }
        List<ImMessage> data = imMessageService.getList(messageQueryParams);
        return ResultUtil.data(data);
    }

    @Operation(summary = "新增Im消息")
    @Parameter(name = "imMessage", description = "Im消息对象", required = true)
    @PostMapping
    public ResultMessage<ImMessage> save(ImMessage imMessage) {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        String imUserId = getImUserId(currentUser);
        // 强制发送方为当前登录主体，禁止伪造发件人
        imMessage.setFromUser(imUserId);
        if (StrUtil.isNotEmpty(imMessage.getTalkId()) && !UserEnums.MANAGER.equals(currentUser.getRole())) {
            ImTalk talk = imTalkService.getById(imMessage.getTalkId());
            if (talk != null && !imUserId.equals(talk.getUserId1()) && !imUserId.equals(talk.getUserId2())) {
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
            }
        }
        if (imMessageService.save(imMessage)) {
            return ResultUtil.data(imMessage);
        }
        throw new ServiceException(ResultCode.IM_MESSAGE_ADD_ERROR);
    }

    @Operation(summary = "更新Im消息")
    @Parameter(name = "id", description = "Im消息ID", required = true)
    @Parameter(name = "imMessage", description = "Im消息对象", required = true)
    @PutMapping("/{id}")
    public ResultMessage<ImMessage> update(@PathVariable String id, ImMessage imMessage) {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        ImMessage existing = imMessageService.getById(id);
        if (existing == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        // 只有消息的原发送方或管理员有权修改自身消息
        if (!UserEnums.MANAGER.equals(currentUser.getRole())) {
            String imUserId = getImUserId(currentUser);
            if (!imUserId.equals(existing.getFromUser())) {
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
            }
        }
        imMessage.setId(id);
        if (imMessageService.updateById(imMessage)) {
            return ResultUtil.data(imMessage);
        }
        throw new ServiceException(ResultCode.IM_MESSAGE_EDIT_ERROR);
    }

    @Operation(summary = "删除Im消息")
    @Parameter(name = "ids", description = "Im消息ID列表", required = true)
    @DeleteMapping("/{ids}")
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        if (ids != null && !ids.isEmpty() && !UserEnums.MANAGER.equals(currentUser.getRole())) {
            String imUserId = getImUserId(currentUser);
            List<ImMessage> list = imMessageService.listByIds(ids);
            for (ImMessage msg : list) {
                if (!imUserId.equals(msg.getFromUser()) && !imUserId.equals(msg.getToUser())) {
                    throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
                }
            }
        }
        imMessageService.removeByIds(ids);
        return ResultUtil.success();
    }

    @Operation(summary = "查看是否有新消息")
    @Parameter(name = "accessToken", description = "访问令牌", required = true)
    @GetMapping("/newMessage")
    public ResultMessage<Boolean> hasNewMessage(String accessToken) {
        if (StrUtil.isBlank(accessToken)) {
            AuthUser currentUser = UserContext.getCurrentUser();
            if (currentUser == null) {
                throw new ServiceException(ResultCode.USER_NOT_LOGIN);
            }
            accessToken = UserContext.getCurrentUserToken();
        }
        return ResultUtil.data(imMessageService.hasNewMessage(accessToken));
    }

    @Operation(summary = "获取所有未读消息")
    @GetMapping("/unreadMessage")
    public ResultMessage<Long> getUnreadMessageCount() {
        return ResultUtil.data(imMessageService.unreadMessageCount());
    }

    @Operation(summary = "清除所有未读消息")
    @PutMapping("/clean/unred")
    public ResultMessage<Object> cleanUnreadMessage() {
        imMessageService.cleanUnreadMessage();
        return ResultUtil.success();
    }
}
