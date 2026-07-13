package cn.lili.controller.manager.sms;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.sms.entity.dos.SmsReach;
import cn.lili.modules.sms.service.SmsReachService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 管理端,短信接口
 *
 * @author Bulbasaur
 * @since 2021/1/30 4:09 下午
 */
@RestController
@Tag(name = "管理端,短信接口")
@RequestMapping("/manager/sms/sms")
public class SmsManagerController {
    @Autowired
    private SmsReachService smsReachService;

    @Operation(summary = "接口批量发送短信")
    @Parameter(name = "smsReach", description = "短信任务", required = true)
    @Parameter(name = "mobile", description = "手机号列表", required = true)
    @PostMapping
    public ResultMessage<Object> sendBatchSms(SmsReach smsReach, List<String> mobile) {
        smsReachService.addSmsReach(smsReach,mobile);
        return ResultUtil.success();
    }

    @Operation(summary = "查询短信任务分页")
    @Parameter(name = "page", description = "分页参数", required = true)
    @GetMapping()
    public ResultMessage<IPage<SmsReach>> querySmsReachPage(PageVO page) {
        return ResultUtil.data(smsReachService.page(PageUtil.initPage(page)));
    }

    @Operation(summary = "查询短信任务")
    @Parameter(name = "id", description = "短信任务id", required = true)
    @GetMapping("/{id}")
    public ResultMessage<SmsReach> querySmsReach(@PathVariable String id) {
        return ResultUtil.data(smsReachService.getById(id));
    }

}
