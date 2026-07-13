package cn.lili.controller.manager.setting;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.dto.payment.dto.PaymentSupportForm;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 管理端,系统设置扩展接口
 * 对一些系统设置的支持，例如动态表单等
 *
 * @author Chopper
 * @since 2020/11/26 15:53
 */
@RestController
@Tag(name = "管理端,系统设置扩展接口")
@RequestMapping("/manager/setting/settingx")
public class SettingXManagerController {

    @Operation(summary = "支持支付方式表单")
    @GetMapping("/paymentSupport")
    public ResultMessage<PaymentSupportForm> paymentForm() {
        return ResultUtil.data(new PaymentSupportForm());
    }

}
