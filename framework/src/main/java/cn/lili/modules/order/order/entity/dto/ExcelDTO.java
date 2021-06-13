package cn.lili.modules.order.order.entity.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author liushuai(liushuai711 @ gmail.com)
 * @version v4.1
 * @Description:
 * @since 2021/6/13 2:37 下午
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ExcelDTO {

    private String sn;
    private String logisticsName;
    private String logisticsNo;
}
