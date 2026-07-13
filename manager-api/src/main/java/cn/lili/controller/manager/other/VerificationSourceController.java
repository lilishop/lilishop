package cn.lili.controller.manager.other;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.verification.entity.dos.VerificationSource;
import cn.lili.modules.verification.service.VerificationService;
import cn.lili.modules.verification.service.VerificationSourceService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import io.swagger.v3.oas.annotations.Parameter;

import java.util.List;


/**
 * 管理端,验证码资源维护接口
 *
 * @author Chopper
 * @since 2020/12/7 11:33
 */
@RestController
@Tag(name = "管理端,验证码资源维护接口")
@RequestMapping("/manager/other/verificationSource")
public class VerificationSourceController {

    @Autowired
    private VerificationSourceService verificationSourceService;

    @Autowired
    private VerificationService verificationService;

    @GetMapping("/{id}")
    @Operation(summary = "查看验证码资源维护详情")
    public ResultMessage<VerificationSource> get(@PathVariable String id) {

        VerificationSource verificationSource = verificationSourceService.getById(id);
        return ResultUtil.data(verificationSource);
    }

    @GetMapping
    @Operation(summary = "分页获取验证码资源维护")
    public ResultMessage<IPage<VerificationSource>> getByPage(VerificationSource entity,
                                                              SearchVO searchVo,
                                                              PageVO page) {
        IPage<VerificationSource> data = verificationSourceService.getByPage(entity, searchVo, page);
        return ResultUtil.data(data);
    }

    @PostMapping
    @Operation(summary = "新增验证码资源维护")
    @Parameter(description = "验证码资源信息", required = true)
    @DemoSite
    public ResultMessage<VerificationSource> save(VerificationSource verificationSource) {

        verificationService.checkCreateVerification(verificationSource.getType(), verificationSource.getResource());
        verificationSourceService.save(verificationSource);
        verificationSourceService.initCache();
        return ResultUtil.data(verificationSource);
    }

    @PutMapping("/{id}")
    @Operation(summary = "更新验证码资源维护")

    @DemoSite
    public ResultMessage<VerificationSource> update(@Parameter(description = "验证码资源ID", required = true) @PathVariable String id, VerificationSource verificationSource) {
        verificationSource.setId(id);
        verificationService.checkCreateVerification(verificationSource.getType(), verificationSource.getResource());
        verificationSourceService.updateById(verificationSource);
        verificationSourceService.initCache();
        return ResultUtil.data(verificationSource);
    }

    @DeleteMapping("/{ids}")
    @Parameter(description = "验证码资源ID列表", required = true)
    @Operation(summary = "删除验证码资源维护")
    @DemoSite
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {

        verificationSourceService.removeByIds(ids);
        verificationSourceService.initCache();
        return ResultUtil.success();
    }
}
