package cn.lili.controller.manager.sms;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.sms.entity.dos.SmsTemplate;
import cn.lili.modules.sms.service.SmsTemplateService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;

/**
 * 管理端,短信模板接口
 *
 * @author Bulbasaur
 * @since 2021/1/30 4:09 下午
 */
@RestController
@Tag(name = "管理端,短信模板接口")
@RequestMapping("/manager/sms/template")
public class SmsTemplateManagerController {
    @Autowired
    private SmsTemplateService smsTemplateService;

    @Operation(summary = "新增短信模板")
    @Parameter(name = "smsTemplate", description = "短信模板", required = true)
    @PostMapping
    @DemoSite
    public ResultMessage<SmsTemplate> save(@Valid SmsTemplate smsTemplate) {
        smsTemplateService.addSmsTemplate(smsTemplate);
        return ResultUtil.success();
    }

    @Operation(summary = "删除短信模板")
    @Parameter(name = "templateCode", description = "短信模板CODE", required = true)
    @DeleteMapping
    @DemoSite
    public ResultMessage<SmsTemplate> delete(String templateCode) {
        smsTemplateService.deleteSmsTemplate(templateCode);
        return ResultUtil.success();
    }

    @Operation(summary = "查询短信模板状态")
    @Parameter(name = "templateCode", description = "短信模板CODE", required = true)
    @PutMapping("/querySmsSign")
    @DemoSite
    public ResultMessage<SmsTemplate> querySmsSign() {
        smsTemplateService.querySmsTemplate();
        return ResultUtil.success();
    }

    @Operation(summary = "修改短信模板")
    @Parameter(name = "smsTemplate", description = "短信模板", required = true)
    @PutMapping("/modifySmsTemplate")
    @DemoSite
    public ResultMessage<SmsTemplate> modifySmsTemplate(@Valid SmsTemplate smsTemplate) {
        smsTemplateService.modifySmsTemplate(smsTemplate);
        return ResultUtil.success();
    }

    @Operation(summary = "查询短信模板分页")
    @Parameter(name = "page", description = "分页参数")
    @Parameter(name = "templateStatus", description = "短信模板状态")
    @GetMapping("/querySmsTemplatePage")
    public ResultMessage<IPage<SmsTemplate>> querySmsTemplatePage(PageVO page, Integer templateStatus) {
        return ResultUtil.data(smsTemplateService.page(page,templateStatus));
    }
}
