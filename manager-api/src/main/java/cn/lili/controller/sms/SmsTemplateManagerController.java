package cn.lili.controller.sms;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.sms.entity.dos.SmsTemplate;
import cn.lili.modules.sms.service.SmsTemplateService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

/**
 * 管理端,短信模板接口
 *
 * @author Bulbasaur
 * @since 2021/1/30 4:09 下午
 */
@RestController
@Api(tags = "管理端,短信模板接口")
@RequestMapping("/manager/sms/template")
public class SmsTemplateManagerController {
    @Autowired
    private SmsTemplateService smsTemplateService;

    @ApiOperation(value = "新增短信模板")
    @PostMapping
    @DemoSite
    public ResultMessage<SmsTemplate> save(@Valid SmsTemplate smsTemplate) {
        smsTemplateService.addSmsTemplate(smsTemplate);
        return ResultUtil.success();
    }

    @ApiOperation(value = "删除短信模板")
    @ApiImplicitParam(name = "templateCode", value = "短信模板CODE", required = true, paramType = "query")
    @DeleteMapping
    @DemoSite
    public ResultMessage<SmsTemplate> delete(String templateCode) {
        smsTemplateService.deleteSmsTemplate(templateCode);
        return ResultUtil.success();
    }

    @ApiOperation(value = "查询短信模板状态")
    @PutMapping("/querySmsSign")
    @DemoSite
    public ResultMessage<SmsTemplate> querySmsSign() {
        smsTemplateService.querySmsTemplate();
        return ResultUtil.success();
    }

    @ApiOperation(value = "修改短信模板")
    @PutMapping("/modifySmsTemplate")
    @DemoSite
    public ResultMessage<SmsTemplate> modifySmsTemplate(@Valid SmsTemplate smsTemplate) {
        smsTemplateService.modifySmsTemplate(smsTemplate);
        return ResultUtil.success();
    }

    @ApiOperation(value = "查询短信模板分页")
    @GetMapping("/querySmsTemplatePage")
    public ResultMessage<IPage<SmsTemplate>> querySmsTemplatePage(PageVO page, Integer templateStatus) {
        return ResultUtil.data(smsTemplateService.page(page,templateStatus));
    }
}
