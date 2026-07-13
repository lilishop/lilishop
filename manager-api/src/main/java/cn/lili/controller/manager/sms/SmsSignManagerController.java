package cn.lili.controller.manager.sms;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.sms.entity.dos.SmsSign;
import cn.lili.modules.sms.service.SmsSignService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;

/**
 * 管理端,短信签名接口
 *
 * @author Chopper
 * @since 2021/1/30 4:09 下午
 */
@RestController
@Tag(name = "管理端,短信签名接口")
@RequestMapping("/manager/sms/sign")
public class SmsSignManagerController {
    @Autowired
    private SmsSignService smsSignService;


    @Operation(summary = "新增短信签名")
    @Parameter(name = "smsSign", description = "短信签名", required = true)
    @PostMapping
    public ResultMessage<SmsSign> save(@Valid SmsSign smsSign) {
        smsSignService.addSmsSign(smsSign);
        return ResultUtil.success();
    }

    @Operation(summary = "删除短信签名")
    @Parameter(name = "id", description = "短信签名id", required = true)
    @DeleteMapping("/{id}")
    public ResultMessage<SmsSign> delete(@PathVariable String id) {
        smsSignService.deleteSmsSign(id);
        return ResultUtil.success();
    }


    @Operation(summary = "查询签名详细")
    @Parameter(name = "id", description = "短信签名id", required = true)
    @GetMapping("/{id}")
    public ResultMessage<SmsSign> getDetail(@PathVariable String id) {
        return ResultUtil.data(smsSignService.getById(id));
    }

    @Operation(summary = "查询短信签名状态")
    @Parameter(name = "smsSign", description = "短信签名", required = true)
    @PutMapping("/querySmsSign")
    public ResultMessage<SmsSign> querySmsSign() {
        smsSignService.querySmsSign();
        return ResultUtil.success();
    }

    @Operation(summary = "修改短信签名")
    @Parameter(name = "smsSign", description = "短信签名", required = true)
    @PutMapping("/modifySmsSign")
    public ResultMessage<SmsSign> modifySmsSign(@Valid SmsSign smsSign) {
        smsSignService.modifySmsSign(smsSign);
        return ResultUtil.success();
    }

    @Operation(summary = "查询短信签名分页")
    @Parameter(name = "page", description = "分页信息", required = true)
    @Parameter(name = "signStatus", description = "签名状态", required = true)
    @GetMapping("/querySmsSignPage")
    public ResultMessage<IPage<SmsSign>> querySmsSignPage(PageVO page, Integer signStatus) {
        return ResultUtil.data(smsSignService.page(page, signStatus));
    }

}
