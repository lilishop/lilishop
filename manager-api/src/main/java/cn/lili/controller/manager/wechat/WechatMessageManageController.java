package cn.lili.controller.manager.wechat;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.wechat.entity.dos.WechatMessage;
import cn.lili.modules.wechat.service.WechatMessageService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;


/**
 * 管理端,微信消息接口
 *
 * @author Chopper
 * @since 2020/12/2 10:40
 */
@RestController
@Tag(name = "管理端,微信消息接口")
@RequestMapping("/manager/wechat/wechatMessage")
public class WechatMessageManageController {
    @Autowired
    private WechatMessageService wechatMessageService;


    @GetMapping("/init")
    @Operation(summary = "初始化微信消息")
    @DemoSite
    public ResultMessage init() {
        wechatMessageService.init();
        return ResultUtil.success();
    }

    @GetMapping("/{id}")
    @Operation(summary = "查看微信消息详情")
    @Parameter(name = "id", description = "微信消息ID", required = true)
    public ResultMessage<WechatMessage> get(@PathVariable String id) {

        WechatMessage wechatMessage = wechatMessageService.getById(id);
        return ResultUtil.data(wechatMessage);
    }

    @GetMapping
    @Operation(summary = "分页获取微信消息")
    @Parameter(name = "page", description = "分页参数", required = true)
    public ResultMessage<IPage<WechatMessage>> getByPage(PageVO page) {
        IPage<WechatMessage> data = wechatMessageService.page(PageUtil.initPage(page));
        return ResultUtil.data(data);
    }

    @DemoSite
    @PostMapping
    @Operation(summary = "新增微信消息")
    public ResultMessage<WechatMessage> save(WechatMessage wechatMessage) {

        wechatMessageService.save(wechatMessage);
        return ResultUtil.data(wechatMessage);
    }

    @DemoSite
    @PutMapping("/{id}")
    @Operation(summary = "更新微信消息")
    @Parameter(name = "id", description = "微信消息ID", required = true)
    public ResultMessage<WechatMessage> update(@PathVariable String id, WechatMessage wechatMessage) {
        // #region agent log: wechat message update entry
        try {
            String logPath = "d:\\lilishop_source\\lilishop\\debug-ade6ce.log";
            String msg = "{"
                    + "\"sessionId\":\"ade6ce\","
                    + "\"runId\":\"toggleWechatOaClose\","
                    + "\"hypothesisId\":\"H_UPDATE_WECHAT_MESSAGE_ENTRY\","
                    + "\"location\":\"WechatMessageManageController:update\","
                    + "\"message\":\"wechatMessage update payload\","
                    + "\"data\":{\"pathId\":\"" + (id == null ? "" : id) + "\","
                    + "\"bodyId\":\"" + (wechatMessage == null ? "" : wechatMessage.getId()) + "\","
                    + "\"enable\":\"" + (wechatMessage == null || wechatMessage.getEnable() == null ? "" : wechatMessage.getEnable()) + "\","
                    + "\"updateTimeIsNull\":\"" + (wechatMessage == null || wechatMessage.getUpdateTime() == null) + "\""
                    + "},"
                    + "\"timestamp\":" + System.currentTimeMillis()
                    + "}\n";
            Files.write(Path.of(logPath), msg.getBytes(StandardCharsets.UTF_8),
                    StandardOpenOption.CREATE, StandardOpenOption.APPEND);
        } catch (Exception ignore) {
        }
        // #endregion
        wechatMessageService.updateById(wechatMessage);
        return ResultUtil.data(wechatMessage);
    }

    @DemoSite
    @DeleteMapping("/{ids}")
    @Operation(summary = "删除微信消息")
    @Parameter(name = "ids", description = "微信消息ID列表", required = true)
    public ResultMessage<Object> delAllByIds(@PathVariable List ids) {
        wechatMessageService.removeByIds(ids);
        return ResultUtil.success();
    }
}
    
