package cn.lili.controller.manager.wechat;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.wechat.entity.dos.WechatMPMessage;
import cn.lili.modules.wechat.service.WechatMPMessageService;
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
 * @author Chopper
 */
@RestController
@Tag(name = "微信小程序消息订阅接口")
@RequestMapping("/manager/wechat/wechatMPMessage")
public class WechatMPMessageManagerController {
    @Autowired
    private WechatMPMessageService wechatMPMessageService;

    @DemoSite
    @GetMapping("/init")
    @Operation(summary = "初始化微信小程序消息订阅")
    public ResultMessage init() {
        wechatMPMessageService.init();
        return ResultUtil.success();
    }

    @GetMapping("/{id}")
    @Operation(summary = "查看微信小程序消息订阅详情")
    @Parameter(name = "id", description = "微信小程序消息订阅ID", required = true)
    public ResultMessage<WechatMPMessage> get(@PathVariable String id) {

        WechatMPMessage wechatMPMessage = wechatMPMessageService.getById(id);
        return ResultUtil.data(wechatMPMessage);
    }

    @GetMapping
    @Operation(summary = "分页获取微信小程序消息订阅")
    @Parameter(name = "entity", description = "微信小程序消息订阅查询参数", required = true)
    @Parameter(name = "searchVo", description = "分页参数", required = true)
    @Parameter(name = "page", description = "分页参数", required = true)
    public ResultMessage<IPage<WechatMPMessage>> getByPage(WechatMPMessage entity,
                                                           SearchVO searchVo,
                                                           PageVO page) {
        IPage<WechatMPMessage> data = wechatMPMessageService.getByPage(entity, searchVo, page);
        return new ResultUtil<IPage<WechatMPMessage>>().setData(data);
    }

    @DemoSite
    @PostMapping
    @Operation(summary = "新增微信小程序消息订阅")
    public ResultMessage<WechatMPMessage> save(WechatMPMessage wechatMPMessage) {

        wechatMPMessageService.save(wechatMPMessage);
        return ResultUtil.data(wechatMPMessage);
    }

    @DemoSite
    @PutMapping("/{id}")
    @Operation(summary = "更新微信小程序消息订阅")
    @Parameter(name = "id", description = "微信小程序消息订阅ID", required = true)
    @Parameter(name = "wechatMPMessage", description = "微信小程序消息订阅参数", required = true)
    public ResultMessage<WechatMPMessage> update(@PathVariable String id, WechatMPMessage wechatMPMessage) {
        // #region agent log: wechat mp message update entry
        try {
            String logPath = "d:\\lilishop_source\\lilishop\\debug-ade6ce.log";
            String msg = "{"
                    + "\"sessionId\":\"ade6ce\","
                    + "\"runId\":\"toggleWechatMpClose\","
                    + "\"hypothesisId\":\"H_UPDATE_WECHAT_MP_ENTRY\","
                    + "\"location\":\"WechatMPMessageManagerController:update\","
                    + "\"message\":\"wechatMPMessage update payload\","
                    + "\"data\":{\"pathId\":\"" + (id == null ? "" : id) + "\","
                    + "\"bodyId\":\"" + (wechatMPMessage == null ? "" : wechatMPMessage.getId()) + "\","
                    + "\"enable\":\"" + (wechatMPMessage == null || wechatMPMessage.getEnable() == null ? "" : wechatMPMessage.getEnable()) + "\","
                    + "\"updateTimeIsNull\":\"" + (wechatMPMessage == null || wechatMPMessage.getUpdateTime() == null) + "\""
                    + "},"
                    + "\"timestamp\":" + System.currentTimeMillis()
                    + "}\n";
            Files.write(Path.of(logPath), msg.getBytes(StandardCharsets.UTF_8),
                    StandardOpenOption.CREATE, StandardOpenOption.APPEND);
        } catch (Exception ignore) {
        }
        // #endregion
        wechatMPMessageService.updateById(wechatMPMessage);
        return ResultUtil.data(wechatMPMessage);
    }

    @DemoSite
    @DeleteMapping("/{ids}")
    @Operation(summary = "删除微信小程序消息订阅")
    @Parameter(name = "ids", description = "微信小程序消息订阅ID列表", required = true)
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {

        wechatMPMessageService.removeByIds(ids);
        return ResultUtil.success();
    }
}
