package cn.lili.modules.statistics.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 商品统计查询参数
 *
 * @author Chopper
 * @since 2020/11/17 7:34 下午
 */
@Data
public class GoodsStatisticsQueryParam extends StatisticsQueryParam {

    @ApiModelProperty(value = "查询类型：按数量（NUM）、按金额（PRICE）")
    private String type;

}
