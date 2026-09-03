package cn.lili.modules.goods.entity.dto;

import lombok.Data;

/**
 * 参数 selector 聚合行：按 (param_name, param_value) 分组后的 MIN(param_sort)。
 *
 * @author Mike
 * @date 2026-09-03
 */
@Data
public class GoodsSearchParamFacetRow {

    private String paramName;

    private String paramValue;

    private Integer sort;
}
