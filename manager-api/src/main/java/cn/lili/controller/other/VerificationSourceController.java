package cn.lili.controller.other;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.verification.entity.dos.VerificationSource;
import cn.lili.modules.verification.service.VerificationSourceService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 管理端,验证码资源维护接口
 *
 * @author Chopper
 * @since 2020/12/7 11:33
 */
@RestController
@Api(tags = "管理端,验证码资源维护接口")
@RequestMapping("/manager/other/verificationSource")
public class VerificationSourceController {

    @Autowired
    private VerificationSourceService verificationSourceService;

    @GetMapping(value = "/{id}")
    @ApiOperation(value = "查看验证码资源维护详情")
    public ResultMessage<VerificationSource> get(@PathVariable String id) {

        VerificationSource verificationSource = verificationSourceService.getById(id);
        return ResultUtil.data(verificationSource);
    }

    @GetMapping
    @ApiOperation(value = "分页获取验证码资源维护")
    public ResultMessage<IPage<VerificationSource>> getByPage(VerificationSource entity,
                                                              SearchVO searchVo,
                                                              PageVO page) {
        IPage<VerificationSource> data = verificationSourceService.page(PageUtil.initPage(page), PageUtil.initWrapper(entity, searchVo));
        return ResultUtil.data(data);
    }

    @PostMapping
    @ApiOperation(value = "新增验证码资源维护")
    @DemoSite
    public ResultMessage<VerificationSource> save(VerificationSource verificationSource) {

        verificationSourceService.save(verificationSource);
        verificationSourceService.initCache();
        return ResultUtil.data(verificationSource);
    }

    @PutMapping("/{id}")
    @ApiOperation(value = "更新验证码资源维护")
    @DemoSite
    public ResultMessage<VerificationSource> update(@PathVariable String id, VerificationSource verificationSource) {
        verificationSource.setId(id);
        verificationSourceService.updateById(verificationSource);
        verificationSourceService.initCache();
        return ResultUtil.data(verificationSource);
    }

    @DeleteMapping(value = "/{ids}")
    @ApiOperation(value = "删除验证码资源维护")
    @DemoSite
    public ResultMessage<Object> delAllByIds(@PathVariable List ids) {

        verificationSourceService.removeByIds(ids);
        verificationSourceService.initCache();
        return ResultUtil.success();
    }
}
