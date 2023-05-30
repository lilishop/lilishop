package cn.lili.controller.sms;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.sms.entity.dos.SmsSign;
import cn.lili.modules.sms.service.SmsSignService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

/**
 * 管理端,短信签名接口
 *
 * @author Chopper
 * @since 2021/1/30 4:09 下午
 */
@RestController
@Api(tags = "管理端,短信签名接口")
@RequestMapping("/manager/sms/sign")
public class SmsSignManagerController {
    @Autowired
    private SmsSignService smsSignService;


    @ApiOperation(value = "新增短信签名")
    @PostMapping
    public ResultMessage<SmsSign> save(@Valid SmsSign smsSign) {
        smsSignService.addSmsSign(smsSign);
        return ResultUtil.success();
    }

    @ApiOperation(value = "删除短信签名")
    @DeleteMapping("/{id}")
    @ApiImplicitParam(name = "id", value = "短信签名id", required = true, dataType = "String", allowMultiple = true, paramType = "path")
    public ResultMessage<SmsSign> delete(@PathVariable String id) {
        smsSignService.deleteSmsSign(id);
        return ResultUtil.success();
    }


    @ApiOperation(value = "查询签名详细")
    @GetMapping("/{id}")
    @ApiImplicitParam(name = "id", value = "短信签名id", required = true, dataType = "String", allowMultiple = true, paramType = "path")
    public ResultMessage<SmsSign> getDetail(@PathVariable String id) {
        return ResultUtil.data(smsSignService.getById(id));
    }

    @ApiOperation(value = "查询短信签名状态")
    @PutMapping("/querySmsSign")
    public ResultMessage<SmsSign> querySmsSign() {
        smsSignService.querySmsSign();
        return ResultUtil.success();
    }

    @ApiOperation(value = "修改短信签名")
    @PutMapping("/modifySmsSign")
    public ResultMessage<SmsSign> modifySmsSign(@Valid SmsSign smsSign) {
        smsSignService.modifySmsSign(smsSign);
        return ResultUtil.success();
    }

    @ApiOperation(value = "查询短信签名分页")
    @GetMapping("/querySmsSignPage")
    public ResultMessage<IPage<SmsSign>> querySmsSignPage(PageVO page, Integer signStatus) {
        return ResultUtil.data(smsSignService.page(page, signStatus));
    }

}
