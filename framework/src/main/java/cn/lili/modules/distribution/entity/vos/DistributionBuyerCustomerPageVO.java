package cn.lili.modules.distribution.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

/**
 * 分销员端客户列表分页结果
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionBuyerCustomerPageVO {

    @Schema(description = "总记录数")
    private long total;

    @Schema(description = "当前页")
    private long current;

    @Schema(description = "每页数量")
    private long size;

    @Schema(description = "列表数据")
    private List<DistributionBuyerCustomerVO> records = new ArrayList<>();
}
