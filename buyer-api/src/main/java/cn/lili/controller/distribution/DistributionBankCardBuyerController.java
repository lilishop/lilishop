package cn.lili.controller.distribution;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.DistributionBankCard;
import cn.lili.modules.distribution.entity.dto.DistributionBankCardAddDTO;
import cn.lili.modules.distribution.service.DistributionBankCardService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.NotBlank;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 买家端 - 分销员银行卡管理接口
 *
 * @author Bulbasaur
 * @since 2026-08-06
 */
@RestController
@Tag(name = "买家端,分销员银行卡管理接口")
@RequestMapping("/buyer/distribution/bank-card")
@Validated
public class DistributionBankCardBuyerController {

    @Autowired
    private DistributionBankCardService bankCardService;

    @Operation(summary = "查询我的银行卡列表")
    @GetMapping
    public ResultMessage<List<DistributionBankCard>> list() {
        return ResultUtil.data(bankCardService.listMyCards());
    }

    @Operation(summary = "添加银行卡")
    @PostMapping
    public ResultMessage<DistributionBankCard> add(
            @Validated @RequestBody DistributionBankCardAddDTO dto) {
        return ResultUtil.data(bankCardService.addCard(dto));
    }

    @Operation(summary = "删除银行卡")
    @DeleteMapping("/{id}")
    public ResultMessage<Object> delete(
            @Parameter(description = "银行卡ID", required = true)
            @PathVariable String id) {
        bankCardService.deleteCard(id);
        return ResultUtil.success();
    }

    @Operation(summary = "设置默认银行卡")
    @PutMapping("/{id}/default")
    public ResultMessage<Object> setDefault(
            @Parameter(description = "银行卡ID", required = true)
            @PathVariable String id) {
        bankCardService.setDefault(id);
        return ResultUtil.success();
    }
}
