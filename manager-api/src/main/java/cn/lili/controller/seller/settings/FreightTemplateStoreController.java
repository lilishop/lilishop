package cn.lili.controller.seller.settings;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.store.entity.vos.FreightTemplateVO;
import cn.lili.modules.store.service.FreightTemplateService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;
import java.util.Objects;

/**
 * 店铺端,运费模板接口
 *
 * @author paulG
 * @since 2020/8/26
 **/
@RestController
@Tag(name = "店铺端,运费模板接口")
@RequestMapping("/store/setting/freightTemplate")
public class FreightTemplateStoreController {
    @Autowired
    private FreightTemplateService freightTemplateService;

    @Operation(description = "商家运费模板列表")
    @GetMapping
    public ResultMessage<List<FreightTemplateVO>> list() {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(freightTemplateService.getFreightTemplateList(storeId));
    }

    @Operation(description = "获取商家运费模板详情")
    @Parameter(name = "id", description = "商家模板ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<FreightTemplateVO> list(@PathVariable String id) {
        FreightTemplateVO freightTemplate = OperationalJudgment.judgment(freightTemplateService.getFreightTemplate(id));
        return ResultUtil.data(freightTemplate);
    }

    @Operation(description = "添加商家运费模板")
    @Parameter(name = "freightTemplateVO", description = "运费模板VO", required = true)
    @PostMapping
    public ResultMessage<FreightTemplateVO> add(@Valid @RequestBody FreightTemplateVO freightTemplateVO) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        freightTemplateVO.setStoreId(storeId);
        return ResultUtil.data(freightTemplateService.addFreightTemplate(freightTemplateVO));
    }

    @Operation(description = "修改商家运费模板")
    @Parameter(name = "id", description = "商家模板ID", required = true)
    @Parameter(name = "freightTemplateVO", description = "运费模板VO", required = true)
    @PutMapping("/{id}")
    public ResultMessage<FreightTemplateVO> edit(@PathVariable String id, @RequestBody @Valid FreightTemplateVO freightTemplateVO) {
        OperationalJudgment.judgment(freightTemplateService.getFreightTemplate(id));
        return ResultUtil.data(freightTemplateService.editFreightTemplate(freightTemplateVO));
    }

    @Operation(description = "删除商家运费模板")
    @Parameter(name = "id", description = "商家模板ID", required = true)
    @DeleteMapping("/{id}")
    public ResultMessage<Object> edit(@PathVariable String id) {
        OperationalJudgment.judgment(freightTemplateService.getFreightTemplate(id));
        freightTemplateService.removeFreightTemplate(id);
        return ResultUtil.success();
    }
}
