package cn.lili.modules.system.entity.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * 物流信息
 *
 * @author Chopper
 * @since 2021/1/18 3:28 下午
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Traces {

    /**
     * 物流公司
     */
    private String shipper;

    /**
     * 物流单号
     */
    private String logisticCode;

    /**
     * 物流详细信息
     */
    private List<Map> traces;
}
